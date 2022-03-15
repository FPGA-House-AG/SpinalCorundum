package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

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

  //printf("hashString = %s\n", SourceCodeGitHash())
  //printf("commitCount = %d\n", SourceCodeGitCommits())

  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    // leet code for Blackwire 1 Filter
    busCtrlWrapped.read(B"32'bB1F117E5", 0x00, documentation = null)
    // 16'b version and 16'b revision
    busCtrlWrapped.read(B"32'b00010001", 0x04, documentation = null)

    val gitCommits = B(BigInt(SourceCodeGitCommits()), 32 bits)
    busCtrlWrapped.read(gitCommits, 0x08, 0, null)
    val gitHash = B(BigInt(SourceCodeGitHash(), 16), 160 bits)
    busCtrlWrapped.readMultiWord(gitHash, 0x0c, documentation = null)

    val keepFilter = Reg(Bits(dataWidth bits))
    busCtrlWrapped.writeMultiWord(keepFilter, 0x40, documentation = null)
    val keepMask = Reg(Bits(dataWidth bits))
    busCtrlWrapped.writeMultiWord(keepMask, 0x80, documentation = null)
    val dropFilter = Reg(Bits(dataWidth bits))
    busCtrlWrapped.writeMultiWord(dropFilter, 0xc0, documentation = null)
    val dropMask = Reg(Bits(dataWidth bits))
    busCtrlWrapped.writeMultiWord(dropMask, 0x100, documentation = null)

    io.keepFilter := keepFilter
    io.keepMask := keepMask
    io.dropFilter := dropFilter
    io.dropMask := dropMask
  }

  //val hexHashString = thisSourceFileGitHash(sourcecode.File().toString())
  //assert(hexHashString.length == 40)
  //printf("hashString = %s\n", hexHashString)
  //val rev = B(BigInt(hexHashString.slice(0, 7), 16), 32 bits)
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
