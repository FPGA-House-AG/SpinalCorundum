package corundum

import spinal.core._
import spinal.lib._

// companion object
object CorundumFrameFilter {
}

case class CorundumFrameFilter(dataWidth : Int) extends Component {
  val io = new Bundle {
    val slave0 = slave Stream Fragment(CorundumFrame(dataWidth))
    val master0 = master Stream Fragment(CorundumFrame(dataWidth))
    val keepMask = in Bits(dataWidth bits)
    val keepFilter = in Bits(dataWidth bits)
    val dropMask = in Bits(dataWidth bits)
    val dropFilter = in Bits(dataWidth bits)
  }
  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
  //val is_frame_continuation = RegNextWhen(!io.slave0.last, io.slave0.valid) init(False)
  val is_frame_continuation = RegNextWhen(!io.slave0.last, io.slave0.valid) init(False)
  val keep_matches = (io.slave0.payload.tdata & io.keepMask) === (io.keepFilter & io.keepMask);
  val drop_matches = (io.slave0.payload.tdata & io.dropMask) === (io.dropFilter & io.dropMask);
  val first_beat_keep_matches = RegNextWhen(keep_matches, !is_frame_continuation) init(False)
  val first_beat_drop_matches = RegNextWhen(drop_matches, !is_frame_continuation) init(False)
  // purely for manual debug
  val is_first_beat = io.slave0.ready & io.slave0.valid & !is_frame_continuation
  val keep_frame = first_beat_keep_matches & !first_beat_drop_matches
  val y = x.stage().takeWhen(keep_frame)
  x << io.slave0
  io.master0 << y
}

//Generate the CorundumFrameFilter's Verilog
object CorundumFrameFilterVerilog {
//  def main(args: Array[String]) {
//    SpinalVerilog(new CorundumFrameFilter)
//  }
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameFilter(512)
      XilinxPatch(toplevel)
    })
    config.generateVerilog({
      val toplevel = new CorundumFrameFilter(512)
      XilinxPatch(toplevel)
    })
  }
}

//Generate the CorundumFrameFilter's VHDL
object CorundumFrameFilterVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameFilter(512))
  }
}
