package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// "0102030405060102030405060102" Ethernet
// "xxxx11887766554433221145" IPv4, IHL=5, protocol=0x11 (UDP)
// "0000FF0000000000000000FF"
// "CCCCLLLLb315SSSS", DDDD=port 5555 (0x15b3)
// "00000000FFFF0000"

// companion object
object CorundumFrameMatchHeader {
  final val addressWidth = 10
}

case class CorundumFrameMatchWireguard() extends Component {
  val dataWidth : Int = 512
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    //val sink_length = in UInt(12 bits)
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val is_type123 = out Bool()
    val is_type4 = out Bool()
  }

  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))

  x << io.sink

  val is_frame_continuation = RegNextWhen(!x.last, x.valid) init(False)
  // purely for manual debug
  val is_first_beat = x.ready & x.valid & !is_frame_continuation
  // byte 14 is IP header, byte 34 is UDP header, byte 42 is UDP payload
  val is_ipv4l5  = x.payload.tdata(14 * 8,  8 bits) === B"8'h45"
  val is_udp     = x.payload.tdata(23 * 8,  8 bits) === B"8'h11"
  val is_type1   = x.payload.tdata(42 * 8, 32 bits) === B"32'h00000001"
  val is_type2   = x.payload.tdata(42 * 8, 32 bits) === B"32'h00000002"
  val is_type3   = x.payload.tdata(42 * 8, 32 bits) === B"32'h00000003"
  val is_type4   = x.payload.tdata(42 * 8, 32 bits) === B"32'h00000004"
  val type4_field = x.payload.tdata(42 * 8, 32 bits)
  val is_type123 = (x.payload.tdata(42 * 8 + 2,  6 bits) === B"6'b000000") &
    (x.payload.tdata(43 * 8, 24 bits) === B"24'h000000")
  val udp_length = x.payload.tdata(34 * 8, 16 bits)
  
  // all bytes required to match the filters are present in the input data?
  val are_bytes_present = x.payload.tkeep(14) & x.payload.tkeep(23) & x.payload.tkeep(42, 4 bits).andR
  val is_match_type123 = is_ipv4l5 & is_udp & is_type123
  val is_match_type4 = is_ipv4l5 & is_udp & is_type4

  // the final keep and drop criteria for the frame
  val is_type123_on_first_beat = RegNextWhen(is_match_type123 & are_bytes_present, !is_frame_continuation) init(False)
  val is_type4_on_first_beat = RegNextWhen(is_match_type4 & are_bytes_present, !is_frame_continuation) init(False)

  io.is_type123 := is_type123_on_first_beat
  io.is_type4 := is_type4_on_first_beat
  io.source << x.stage()

  // Rename SpinalHDL library defaults to AXI naming convention
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

class CorundumFrameDemuxWireguard() extends Component {
  val dataWidth : Int = 512
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source_other = master Stream Fragment(CorundumFrame(dataWidth))
    val source_type4 = master Stream Fragment(CorundumFrame(dataWidth))
  }

  val matcher = CorundumFrameMatchWireguard()
  matcher.io.sink << io.sink
  val select_type4 = matcher.io.is_type4
  
  Vec(io.source_other, io.source_type4) <> StreamDemux(
    matcher.io.source,
    U(select_type4),
    2
  )

  // Rename SpinalHDL library defaults to AXI naming convention
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}


case class CorundumFrameMatchHeader(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val matchMask = in Bits(dataWidth bits)
    val matchHeader = in Bits(dataWidth bits)
    val is_match = out Bool()
  }
  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
  val is_frame_continuation = RegNextWhen(!x.last, x.valid) init(False)
  // purely for manual debug
  val is_first_beat = x.ready & x.valid & !is_frame_continuation

  val is_header_match = (x.payload.tdata & io.matchMask) === (io.matchHeader & io.matchMask)

  // generate byte enables for the filter masks
  val match_tkeep = Reg(Bits(dataWidth/8 bits))
  for (i <- 0 until dataWidth/8) {
    // if any of the bits of a certain byte are in the filter mask, the
    // input byte should be present; to check enable the corresponding
    // bit in tkeep
    match_tkeep(i) := io.matchMask((i+1)*8-1 downto i*8).asBits.orR
  }
  // all bytes required to match the filters are present in the input data?
  val are_bytes_present = (x.payload.tkeep & match_tkeep) === (match_tkeep);

  // the final keep and drop criteria for the frame
  val is_match_on_first_beat = RegNextWhen(is_header_match & are_bytes_present, !is_frame_continuation) init(False)

  io.is_match := is_match_on_first_beat
  x << io.sink
  io.source << x.stage()

  //printf("hashString = %s\n", SourceCodeGitHash())
  //printf("commitCount = %d\n", SourceCodeGitCommits())

  def driveFrom(busCtrl : BusSlaveFactory) = new Area {
    // leet code for Blackwire 1 Filter
    busCtrl.read(B"32'hB1F117E5", 0x00, documentation = null)
    // 16'b version and 16'b revision
    busCtrl.read(B"32'b00010001", 0x04, documentation = null)
    // some strictly increasing (not per se incrementing) build number
    val gitCommits = B(BigInt(SourceCodeGitCommits()), 32 bits)
    busCtrl.read(gitCommits, 0x08, 0, null)
    val gitHash = B(BigInt(SourceCodeGitHash(), 16), 160 bits)
    busCtrl.readMultiWord(gitHash, 0x0c, documentation = null)

    val matchHeader = Reg(Bits(dataWidth bits)) init(0)
    busCtrl.writeMultiWord(matchHeader, 0x40, documentation = null)
    val matchMask = Reg(Bits(dataWidth bits)) init(0)
    busCtrl.writeMultiWord(matchMask, 0x80, documentation = null)

    io.matchHeader := matchHeader
    io.matchMask := matchMask
  }
}

// companion object
object CorundumFrameMatchHeaderAxi4 {
}

// slave must be naturally aligned
case class CorundumFrameMatchHeaderAxi4(dataWidth : Int, busCfg : Axi4Config) extends Component {

  // copy AXI4 properties from bus, but override address width for slave
  val slaveCfg = busCfg.copy(addressWidth = CorundumFrameMatchHeader.addressWidth)
  
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val ctrlbus = slave(Axi4(slaveCfg))
  }

  val filter = CorundumFrameMatchHeader(dataWidth)
  val ctrl = new Axi4SlaveFactory(io.ctrlbus)
  val bridge = filter.driveFrom(ctrl)
  filter.io.sink << io.sink
  io.source << filter.io.source
}

//Generate the CorundumFrameMatchHeader's Verilog
object CorundumFrameMatchHeaderVerilog {
//  def main(args: Array[String]) {
//    SpinalVerilog(new CorundumFrameMatchHeader)
//  }
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameMatchHeader(512)
      XilinxPatch(toplevel)
    })
  }
}

//Generate the CorundumFrameMatchHeader's Verilog
object CorundumFrameMatchHeaderAxi4Verilog {
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameMatchHeaderAxi4(512, Axi4Config(CorundumFrameMatchHeader.addressWidth, 32, 2, useQos = false, useRegion = false))
      XilinxPatch(toplevel)
    })
  }
}

//Generate the CorundumFrameMatchWireguard's Verilog
object CorundumFrameMatchWireguard {
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameMatchWireguard()
      toplevel
    })
    config.generateVhdl({
      val toplevel = new CorundumFrameMatchWireguard()
      toplevel
    })
  }
}

//Generate the CorundumFrameMatchHeader's VHDL
object CorundumFrameMatchHeaderVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameMatchHeader(512))
  }
}

//Generate the CorundumFrameDemuxWireguard's Verilog
object CorundumFrameDemuxWireguardVerilog {
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameDemuxWireguard()
      //XilinxPatch(toplevel)
      toplevel
    })
  }
}
