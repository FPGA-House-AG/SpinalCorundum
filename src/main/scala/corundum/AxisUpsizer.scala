package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object AxisUpsizer {
  def main(args: Array[String]) {
    SpinalVerilog(new AxisUpsizer(128, 512))
    SpinalVhdl(new AxisUpsizer(128, 512))
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

case class AxisUpsizer(dataWidthIn : Int, dataWidthOut: Int) extends Component {
  /* upsizing by 2 is not supported yet due to bug in StreamFragmentWidthAdapter(x, y = x * 2, earlyLast = true) */
  require(dataWidthOut >= (4 * dataWidthIn), "dataWidthOut must be an 4 or higher integer multiple of dataWidthIn")
  /* non-integer width factors are not supported */
  require(dataWidthOut % dataWidthIn == 0, "dataWidthOut must be an integer multiple of dataWidthIn")

  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidthIn bits)))
    val source = master Stream(Fragment(Bits(dataWidthOut bits)))
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
  }

  // xx is sink, but adds the sink_length as stream payload
  val xx = Stream(Fragment(Bits(dataWidthIn + 12 bits)))
  val ff = Fragment(io.sink_length.asBits ## io.sink.payload.fragment)
  ff.fragment := io.sink_length.asBits ## io.sink.payload.fragment
  ff.last := io.sink.payload.last

  // and xx also has a pipelined skid buffer (or elastic buffer)
  xx << io.sink.translateWith(ff).s2mPipe().m2sPipe()
  
  // x is the original stream after the skid buffer
  val x = Stream(Fragment(Bits(dataWidthIn bits)))
  val fff = Fragment(Bits(dataWidthIn bits))
  fff.last := xx.payload.last
  fff.fragment := xx.payload.fragment.resize(dataWidthIn)
  x << xx.translateWith(fff)
  val x_length = (xx.payload.fragment >> dataWidthIn).asUInt

  val y = Stream(Fragment(Bits(dataWidthOut bits)))

  StreamFragmentWidthAdapter(x, y, earlyLast = true)

  io.source <-< y
  io.source_length := RegNext(x_length)

  // Remove io_ prefix
  noIoPrefix()

  // Execute the function renameIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

// minimal case to reproduce bug for case StreamFragmentWidthAdapter(x, y = x * 2, earlyLast = true)
case class AxisUpsizerIssue963(dataWidthIn : Int, dataWidthOut: Int) extends Component {
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
