package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object AxisWidthAdapter {
}

/* Down size the AXIS data width with a factor of 2 or 4
 *
 * sink accepts AXIS frames (Ethernet packet)
 * sink_length is the input packet length in bytes, this packet arrives on the sink
 *
 * source is the output packet (Ethernet payload)
 * source_length is the output packet (Ethernet payload)
 */

case class AxisWidthAdapter(dataWidthIn : Int, dataWidthOut: Int) extends Component {
  /* upsizing is not supported yet */
  require(dataWidthIn > dataWidthOut, "dataWidthOut must be an integer division of dataWidthIn")
  /* non-integer width factors are not supported */
  require(dataWidthIn % dataWidthOut == 0, "dataWidthOut must be an integer division of dataWidthIn")

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

  // extract header at first beat
  val x_is_frame_continuation = RegNextWhen(!x.last, x.fire).init(False)
  val x_is_first_beat = x.fire & !x_is_frame_continuation
  val x_in_frame = x.fire | x_is_frame_continuation
  val x_is_single_beat = x_is_first_beat && x.last

  // calculate number of output beats based on input length
  val out_beat_last = (x_length - 1) / (dataWidthOut / 8)
  val out_beat_num = Reg(U(0, 12 bits)) init(0)

  // current output beat in y?
  when (y.fire) {
    // last output beat for this input frame?
    when (out_beat_num === out_beat_last) {
      out_beat_num := 0
    } otherwise {
      out_beat_num := out_beat_num + 1
    }
  }
  val factor = dataWidthIn / dataWidthOut
  // when the input can take the next beat
  val next_input = ((out_beat_num % factor) === (factor - 1)) |
    (out_beat_num === out_beat_last)

  val counter = out_beat_num.resize(log2Up(factor))
  y.valid := x.valid
  //endianness match {
  //  case `LITTLE` =>
    y.fragment.assignFromBits(x.fragment.asBits.resize(dataWidthIn).subdivideIn(factor slices).read(counter))
  //  case `BIG`    => output.fragment.assignFromBits(x.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).reverse.read(counter))
  // }
  y.last := x.last && (out_beat_num === out_beat_last)
  x.ready := y.ready && next_input

  // register outputs
  io.source <-< y
  io.source_length := RegNext(x_length)

  // Rename SpinalHDL library defaults to AXI naming convention
  private def renameIO(): Unit = {
    io.flatten.foreach(bt => {
      if(bt.getName().contains("_payload_fragment")) bt.setName(bt.getName().replace("_payload_fragment", "_tdata"))
      if(bt.getName().contains("_payload_last")) bt.setName(bt.getName().replace("_payload_last", "_tlast"))
      if(bt.getName().contains("_payload"))  bt.setName(bt.getName().replace("_payload",  ""))
      if(bt.getName().contains("_fragment")) bt.setName(bt.getName().replace("_fragment", ""))
      if(bt.getName().contains("_valid"))    bt.setName(bt.getName().replace("_valid",    "_tvalid"))
      if(bt.getName().contains("_ready"))    bt.setName(bt.getName().replace("_ready",    "_tready"))
      if(bt.getName().contains("_last"))     bt.setName(bt.getName().replace("_last",     "_tlast"))
      if(bt.getName().contains("reset"))     bt.setName(bt.getName().replace("reset",     "rst"))
    })
  }
  // Remove io_ prefix
  noIoPrefix()

  // Execute the function renameIO after the creation of the component
  addPrePopTask(() => renameIO())
}

//Generate the AxisWidthAdapter's Verilog
object AxisWidthAdapterVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new AxisWidthAdapter(512, 128))
  }
}

//Generate the AxisWidthAdapter's VHDL
object AxisWidthAdapterVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new AxisWidthAdapter(512, 128))
  }
}
