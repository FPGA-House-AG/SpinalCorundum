package corundum

import spinal.core._
import spinal.lib._

import scala.math._

// companion object for case class
object AxisDelay {
  // generate VHDL and Verilog
  def main(args: Array[String]) : Unit = {
    val vhdlReport = Config.spinal.generateVhdl(new AxisDelay(Config.corundumDataWidth, 65))
    val verilogReport = Config.spinal.generateVerilog(new AxisDelay(Config.corundumDataWidth, 65))
  }
}

/* Add delay to a Stream. Can be used to match the delay of a parallel pipelined process
 * (of the Stream data) which has the same downstream backpressure (ready == 0) applied.
 *
 * Delay is added to the (internal) delay caused by downstream backpressure (ready==0).
 */
case class AxisDelay(dataWidth : Int, cycleCount : Int) extends Component {

  val streamFifoLatency = 2

  require(cycleCount >= streamFifoLatency)

  val io = new Bundle {
    val sink = slave Stream Fragment(Bits(dataWidth bits))
    val source = master Stream Fragment(Bits(dataWidth bits))
    // sink_length and source_length is given in bytes
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
  }

  // translateWith() for Stream(Fragment())
  // (before this we needed to work-around this, see AxisUpSizer.scala commented out code)
  implicit class FragmentPimper[T <: Data](v: Fragment[T]) {
    def ~~[T2 <: Data](trans: T => T2) = {
      val that = trans(v.fragment)
      val res = Fragment(cloneOf(that))
      res.fragment := trans(v.fragment)
      res.last := v.last
      res
    }
  }

  // concatenate sink_length to Fragment, going through skid buffer in-sync with data
  val x = Stream(Fragment(Bits(12 + dataWidth bits)))
  x << io.sink.~~(_.~~(io.sink_length.asBits ## _)) //.s2mPipe().m2sPipe()

  // x is input stream, z is output stream with added 'cycleCount' delay.
  // the delay is added to the delay caused by backpressure (ready ==0 ).
  // This can be used to match the delay of parallel pipelined processing
  // of the Stream data with the same downstream backpressure applied.

  // h is x, but paused on backpressure (internal delay through handshake)
  // z is the resulting delayed stream
  val h, z = Stream(Fragment(Bits(12 + dataWidth bits)))
  // halt on downstream backpressure (internal delay)
  h << x.continueWhen(z.ready)
  // store data in FIFO, y is the data FIFO output
  val (y, available) = h.queueWithAvailability(cycleCount + streamFifoLatency)
  // assert data FIFO will not overflow
  when (available === 0) { assert(!h.valid) }
  // z is the delayed stream result
  z << y
  // delay valid signal parallel to the data FIFO with a delay line
  // the delay line respects backpressure (adding internal delay due to stall)
  when (True/* override handshaking signals */) {
    // delay valid from x by cycleCount clocks
    z.valid := Delay(h.valid, cycleCount = cycleCount, init = False, when = z.ready)
    // assert the data belonging to z.valid is in the FIFO
    when (z.fire) { assert(y.valid) }
    // pop from data FIFO only when the delayed valid data is taken
    y.ready := z.fire
  }

  // split length from Fragment
  val w = Stream(Fragment(Bits(dataWidth bits)))
  w << z.~~(_.~~(_.resize(dataWidth)))
  val w_length = (z.payload.fragment >> dataWidth).resize(12).asUInt

  if (true) {
    io.source << w
    io.source_length := w_length 
  /* register output */
  } else {
    io.source <-< w
    io.source_length := RegNextWhen(w_length, w.ready)
  }
  
  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
