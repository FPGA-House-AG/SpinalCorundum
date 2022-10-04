package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameLengthFilter {
}

// @TODO WIP if want to filter on length and headers
case class CorundumFrameLengthFilter(dataWidth : Int, maxBytes : Int) extends Component {
  val io = new Bundle {
    val slave0 = slave Stream Fragment(CorundumFrame(dataWidth))
    val length = in UInt(12 bits)
    val master0 = master Stream Fragment(CorundumFrame(dataWidth))
    val keepMask = in Bits(dataWidth bits)
    val keepFilter = in Bits(dataWidth bits)
    val dropMask = in Bits(dataWidth bits)
    val dropFilter = in Bits(dataWidth bits)
  }
  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
  val y = Stream Fragment(CorundumFrame(dataWidth))
  val stash = CorundumFrameStash(dataWidth)
  x << io.slave0
  x.payload.tkeep := io.length.asBits
  y << x.m2sPipe().s2mPipe().throwWhen(x.payload.tkeep.asUInt > 10)
  //stash.io.slave0 << io.slave0/*.throwWhen(io.slave0.payload.tuser(0))*/
  io.master0 << y
}

//Generate the CorundumFrameFilter's Verilog
object CorundumFrameLengthFilterVerilog {
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
object CorundumFrameLengthFilterVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameFilter(512))
  }
}
