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
    val vhdlReport = Config.spinal.generateVhdl(new AxisUpSizer(Config.cryptoDataWidth, Config.corundumDataWidth))
    val verilogReport = Config.spinal.generateVerilog(new AxisUpSizer(Config.cryptoDataWidth, Config.corundumDataWidth))
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

  // y is input stream with original payload, but after the skid buffer
  val y = Stream(Fragment(Bits(dataWidthIn bits)))
  y <-< io.sink
  val y_length = RegNextWhen(io.sink_length, io.sink.ready)
  val y_drop = RegNextWhen(io.sink_drop, io.sink.ready)

  val z = Stream(Fragment(Bits(dataWidthOut bits)))

  StreamFragmentWidthAdapter(y, z, earlyLast = true)

  io.source <-< z
  io.source_length := RegNextWhen(y_length, z.ready)
  io.source_drop := RegNextWhen(y_drop, z.ready)

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
