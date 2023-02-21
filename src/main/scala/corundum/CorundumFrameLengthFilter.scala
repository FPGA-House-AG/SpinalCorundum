package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameLengthFilter {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameLengthFilter(Config.corundumDataWidth, Config.corundumDataWidth))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameLengthFilter(Config.corundumDataWidth, Config.corundumDataWidth))
  }
}

// @NOTE Broken, unmaintained and currently unused - not sure if we need this 
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
  when (True) {
    x.payload.tkeep := io.length.asBits.resize(dataWidth / 8)
  }
  y << x.s2mPipe().m2sPipe().throwWhen(x.payload.tkeep.asUInt > 10)
  //stash.io.slave0 << io.slave0/*.throwWhen(io.slave0.payload.tuser(0))*/
  io.master0 << y

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
