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
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val keepMask = in Bits(dataWidth bits)
    val keepFilter = in Bits(dataWidth bits)
    val dropMask = in Bits(dataWidth bits)
    val dropFilter = in Bits(dataWidth bits)
  }
  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
  val is_frame_continuation = RegNextWhen(!x.last, x.valid) init(False)
  // purely for manual debug
  val is_first_beat = x.ready & x.valid & !is_frame_continuation

  val is_keepfilter_match = (x.payload.tdata & io.keepMask) === (io.keepFilter & io.keepMask);
  val is_dropfilter_match = (x.payload.tdata & io.dropMask) === (io.dropFilter & io.dropMask);

  // generate byte enables for the filter masks
  val keepfilter_tkeep = Reg(Bits(dataWidth/8 bits))
  val dropfilter_tkeep = Reg(Bits(dataWidth/8 bits))
  for (i <- 0 until dataWidth/8) {
    // if any of the bits are in the filter mask, enable the corresponding byte
    keepfilter_tkeep(i) := io.keepMask((i+1)*8-1 downto i*8).asBits.orR
    dropfilter_tkeep(i) := io.dropMask((i+1)*8-1 downto i*8).asBits.orR
  }
  // all bytes required to match the filters are present in the input data?
  val are_keepfilter_bytes_present = (x.payload.tkeep & keepfilter_tkeep) === (keepfilter_tkeep);
  // @TODO consider matching any bytes, not all, for the drop filter
  val are_dropfilter_bytes_present = (x.payload.tkeep & dropfilter_tkeep) === (dropfilter_tkeep);

  // the final keep and drop criteria for the frame
  val is_first_beat_keepfilter_match = RegNextWhen(is_keepfilter_match & are_keepfilter_bytes_present, !is_frame_continuation) init(False)
  val is_first_beat_dropfilter_match = RegNextWhen(is_dropfilter_match & are_dropfilter_bytes_present, !is_frame_continuation) init(False)

  // keep frame when keep filter matches and drop filter does not match
  val keep_frame = is_first_beat_keepfilter_match & !is_first_beat_dropfilter_match

  val y = x.stage().takeWhen(keep_frame)
  x << io.sink
  io.source << y

  //printf("hashString = %s\n", SourceCodeGitHash())
  //printf("commitCount = %d\n", SourceCodeGitCommits())

  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    // leet code for Blackwire 1 Filter
    busCtrlWrapped.read(B"32'bB1F117E5", 0x00, documentation = null)
    // 16'b version and 16'b revision
    busCtrlWrapped.read(B"32'b00010001", 0x04, documentation = null)
    // some strictly increasing (not per se incrementing) build number
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
  }
}

//Generate the CorundumFrameFilter's VHDL
object CorundumFrameFilterVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameFilter(512))
  }
}
