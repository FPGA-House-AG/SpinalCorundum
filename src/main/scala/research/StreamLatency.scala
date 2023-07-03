package corundum

import spinal.core._
import spinal.lib._

import scala.math._

// companion object for case class
object StreamLatency {
  // generate VHDL and Verilog
  def main(args: Array[String]) : Unit = {
    val vhdlReport = Config.spinal.generateVhdl(StreamLatency(Bits(Config.corundumDataWidth bits), 65))
    val verilogReport = Config.spinal.generateVerilog(StreamLatency(Bits(Config.corundumDataWidth bits), 65))
  }
}

/* Add latency to a Stream. Can be used to match the latency of a parallel pipelined process
 * (of the Stream data) which has the same downstream backpressure (ready == 0) applied.
 *
 * Latency is added to the (internal) latency caused by downstream backpressure (ready==0).
 */
case class StreamLatency[T <: Data](dataType: HardType[T], cycleCount : Int) extends Component {

  // @TODO if true, the assert() does not hold?!
  val hasOutputReg = true
  val streamFifoLatency = 2
  val outputRegLatency = if (hasOutputReg) 1 else 0

  require(cycleCount >= (streamFifoLatency + outputRegLatency))

  val io = new Bundle {
    val sink = slave Stream(dataType)
    val source = master Stream(dataType)
  }

  // to measure latencies in simulation
  val cycle = Reg(UInt(32 bits)).init(0)
  cycle := cycle + 1

  val x = Stream(dataType)
  x << io.sink

  // x is input stream, z is output stream with added 'cycleCount' delay.
  // the delay is added to the delay caused by backpressure (ready ==0 ).
  // This can be used to match the delay of parallel pipelined processing
  // of the Stream data with the same downstream backpressure applied.

  // h is x, but paused on backpressure (internal delay through handshake)
  // z is the resulting delayed stream
  val h, z = Stream(dataType)
  // halt on downstream backpressure (internal delay)
  h << x.continueWhen(io.source.ready)
  // store data in FIFO, y is the data FIFO output
  //val (y, available) = h.queueWithAvailability(cycleCount)
  val y = h.queue(cycleCount)
  // assert data FIFO will not overflow
  //when (available === 0) { assert(!h.valid) }
  // z is the delayed stream result
  z << y
  // delay valid signal parallel to the data FIFO with a delay line
  // the delay line respects backpressure (adding internal delay due to stall)
  when (True/* override handshaking signals */) {
    // delay valid from x by cycleCount clocks
    z.valid := Delay(h.valid, cycleCount = cycleCount - outputRegLatency, init = False, when = z.ready)
    // assert the data belonging to z.valid is in the FIFO
    when (z.fire) { assert(y.valid) }
    // pop from data FIFO only when the delayed valid data is taken
    y.ready := z.fire
  }

  if (!hasOutputReg) {
    io.source << z
  /* register output */
  } else {
    io.source <-< z
  }
  
  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
