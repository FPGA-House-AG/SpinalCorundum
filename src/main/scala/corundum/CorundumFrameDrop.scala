package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameDrop {
}

case class CorundumFrameDrop(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val drop = in Bool()
  }
  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
  val y = Stream Fragment(CorundumFrame(dataWidth))

  // remember if current beat is not last; then next beat is not first
  val is_frame_continuation = RegNextWhen(!x.last, x.fire) init(False)
  val is_first_beat = x.fire & !is_frame_continuation
  val is_last_beat = x.fire & x.last
  val is_intermediate_beat = x.fire & is_frame_continuation & !x.last
  val is_first = x.isFirst

  // capture the drop flag on first
  val drop_this_packet = RegNextWhen(io.drop, io.sink.isFirst)

  // skid buffer on input sink
  x << io.sink.m2sPipe().s2mPipe()
  // drop packet conditionally
  y << x.throwWhen(drop_this_packet)
  io.source << y
}

// Generate the CorundumFrameDrop's Verilog
object CorundumFrameDropVerilog {
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameDrop(512)
      XilinxPatch(toplevel)
    })
    config.generateVerilog({
      val toplevel = new CorundumFrameDrop(512)
      XilinxPatch(toplevel)
    })
  }
}

// Generate the CorundumFrameDrop's VHDL
object CorundumFrameDropVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameDrop(512))
  }
}
