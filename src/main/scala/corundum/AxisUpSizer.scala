package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object for case class
object AxisUpSizer {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new AxisUpSizer(Config.cryptoWidth, Config.corundumWidth))
    val verilogReport = Config.spinal.generateVerilog(new AxisUpSizer(Config.cryptoWidth, Config.corundumWidth))
  }
}

/* Upsize the AXIS data width by a factor of 4 or more (2 is not supported yet)
 *
 * sink accepts AXIS frames (Ethernet packet)
 * sink_length is the input packet length in bytes, this packet arrives on the sink
 *
 * source is the output packet (Ethernet payload)
 * source_length is the output packet (Ethernet payload)
 */

case class AxisUpSizer(dataWidthIn : Int, dataWidthOut: Int) extends Component {
  /* upsizing by 2 is not supported yet due to bug in StreamFragmentWidthAdapter(x, y = x * 2, earlyLast = true) */
  require(dataWidthOut >= (2 * dataWidthIn), "dataWidthOut must be an 4 or higher integer multiple of dataWidthIn")
  /* non-integer width factors are not supported */
  require(dataWidthOut % dataWidthIn == 0, "dataWidthOut must be an integer multiple of dataWidthIn")

  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidthIn bits)))
    val source = master Stream(Fragment(Bits(dataWidthOut bits)))
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    // accept drop flag
    val sink_drop = in Bool()
    val source_length = out UInt(12 bits)
    val source_drop = out Bool()
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

  // x is sink, but adds the sink_length as stream payload
  // such that both sink and sink_length are skid buffered
  val x = Stream(Fragment(Bits(dataWidthIn + 12 + 1 bits)))
  x << io.sink.~~(_.~~(io.sink_drop ## io.sink_length.asBits ## _)).s2mPipe().m2sPipe()
   
  // y is input stream with original payload, but after the skid buffer
  val y = Stream(Fragment(Bits(dataWidthIn bits)))
  y << x.~~(_.~~(_.resize(dataWidthIn)))
  val y_length = (x.payload.fragment >> dataWidthIn).resize(12).asUInt
  val y_drop = (x.payload.fragment >> dataWidthIn)(12)

  val z = Stream(Fragment(Bits(dataWidthOut bits)))

  StreamFragmentWidthAdapter(y, z, earlyLast = true)

  io.source <-< z
  io.source_length := RegNext(y_length)
  // @NOTE we could take drop directly from y, to make it line up with last from chacha
  // this is currently taken care of in chacha wrapper already - so leave as is!
  io.source_drop := RegNext(y_drop)

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

// minimal case to reproduce bug for case StreamFragmentWidthAdapter(x, y = x * 2, earlyLast = true)
case class AxisUpSizerIssue963(dataWidthIn : Int, dataWidthOut: Int) extends Component {
  /* upsizing is not supported yet */
  require(dataWidthOut == (2 * dataWidthIn), "dataWidthOut must be an 2 or higher integer multiple of dataWidthIn")
  /* non-integer width factors are not supported */
  require(dataWidthOut % dataWidthIn == 0, "dataWidthOut must be an integer multiple of dataWidthIn")

  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidthIn bits)))
    val source = master Stream(Fragment(Bits(dataWidthOut bits)))
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
  }
  StreamFragmentWidthAdapter(io.sink, io.source, earlyLast = true)
  
  io.source_length := io.sink_length
}
