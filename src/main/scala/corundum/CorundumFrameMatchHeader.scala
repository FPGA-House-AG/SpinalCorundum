package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

/* @NOTE Multiple classes here:
 *
 * CorundumFrameDemuxWireguardType4 (hard-coded, used for PoC)
 *
 * Now of lesser importance:
 * CorundumFrameMatchWireguard
 * CorundumFrameMatchHeader
 * CorundumFrameMatchHeaderAxi4
 */
 

// "0102030405060102030405060102" Ethernet
// "xxxx11887766554433221145" IPv4, IHL=5, protocol=0x11 (UDP)
// "0000FF0000000000000000FF"
// "CCCCLLLLb315SSSS", DDDD=port 5555 (0x15b3)
// "00000000FFFF0000"

//Generate the CorundumFrameMatchWireguard's Verilog
object CorundumFrameMatchWireguard {
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameMatchWireguard())
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameMatchWireguard())
  }
}

case class CorundumFrameMatchWireguard() extends Component {
  val dataWidth : Int = 512
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    //val sink_length = in UInt(12 bits)
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val is_type123 = out Bool()
    val is_type4 = out Bool()
    val is_arp = out Bool()
    val is_icmp = out Bool()
  }

  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))

  x << io.sink

  val is_frame_continuation = RegNextWhen(!x.last, x.valid) init(False)
  // purely for manual debug
  val is_first_beat = x.ready & x.valid & !is_frame_continuation
  // byte 0 is Ethernet, byte 14 is IP, byte 34 is UDP header, byte 42 is UDP payload

  // ARP: TYPE=0806, HTYPE=0001, PTYPE=0800, HLEN=6, PLEN=4
  val is_etharp  = x.payload.tdata(12 * 8, 64 bits).subdivideIn(8 bits).reverse.asBits() === B"64'h0806000108000604"
  // TYPE=0800
  val ethtype = x.payload.tdata(12 * 8, 16 bits).subdivideIn(8 bits).reverse.asBits()
  val is_ethip   = (x.payload.tdata(12 * 8, 16 bits).subdivideIn(8 bits).reverse.asBits() === B"16'h0800") & x.payload.tkeep(13)
  // IPv4 but only with length 5 (*4 == 20 bytes header during PoC @TODO Change for release later)
  val is_ipv4l5  = (x.payload.tdata(14 * 8,  8 bits) === B"8'h45") & x.payload.tkeep(14)
  // IP protocol number x11
  val is_udp     = (x.payload.tdata(23 * 8,  8 bits) === B"8'h11") & x.payload.tkeep(23)
  // IP protocol number 0x01
  val is_icmp    = (x.payload.tdata(23 * 8,  8 bits) === B"8'h01") & x.payload.tkeep(23)
  val is_type1   = (x.payload.tdata(42 * 8, 32 bits) === B"32'h00000001") & x.payload.tkeep(42, 4 bits).andR
  val is_type2   = (x.payload.tdata(42 * 8, 32 bits) === B"32'h00000002") & x.payload.tkeep(42, 4 bits).andR
  val is_type3   = (x.payload.tdata(42 * 8, 32 bits) === B"32'h00000003") & x.payload.tkeep(42, 4 bits).andR
  val is_type4   = (x.payload.tdata(42 * 8, 32 bits) === B"32'h00000004") //& x.payload.tkeep(42, 4 bits).andR
  val type4_field = x.payload.tdata(42 * 8, 32 bits)
  val is_type123 = (x.payload.tdata(42 * 8 + 2,  6 bits) === B"6'b000000") &
    (x.payload.tdata(43 * 8, 24 bits) === B"24'h000000") & x.payload.tkeep(42, 4 bits).andR
  val udp_length = x.payload.tdata(34 * 8, 16 bits)
  
  // all bytes required to match the filters are present in the input data?
  //val are_bytes_present = x.payload.tkeep(14) & x.payload.tkeep(23) //& x.payload.tkeep(42, 4 bits).andR
  val is_match_type123 = is_ethip & is_ipv4l5 & is_udp & is_type123
  val is_match_type4 =   is_ethip & is_ipv4l5 & is_udp & is_type4
  val is_match_arp =     is_etharp
  val is_match_icmp =    is_ethip & is_ipv4l5 & is_icmp

  // the final keep and drop criteria for the frame
  val is_type123_on_first_beat = RegNextWhen(is_match_type123, !is_frame_continuation) init(False)
  val is_type4_on_first_beat = RegNextWhen(is_match_type4, !is_frame_continuation) init(False)
  val is_arp_on_first_beat = RegNextWhen(is_match_arp, !is_frame_continuation) init(False)
  val is_icmp_on_first_beat = RegNextWhen(is_match_icmp, !is_frame_continuation) init(False)

  io.is_type123 := is_type123_on_first_beat
  io.is_type4 := is_type4_on_first_beat
  io.is_arp := is_arp_on_first_beat
  io.is_icmp := is_icmp_on_first_beat
  io.source << x.stage()

  // Rename SpinalHDL library defaults to AXI naming convention
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

object CorundumFrameDemuxWireguardType4 {
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameDemuxWireguardType4(Config.corundumDataWidth))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameDemuxWireguardType4(Config.corundumDataWidth))
  }
}

/* Demuxes Wireguard Type 4 packets
 * Wireguard Type 4 packets are output on a second, separate, stream source
 */
case class CorundumFrameDemuxWireguardType4(dataWidth : Int) extends Component {
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

// companion object
object CorundumFrameMatchHeader {
  final val addressWidth = 10
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameMatchHeader(Config.corundumDataWidth))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameMatchHeader(Config.corundumDataWidth))
  }
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

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))

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
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameMatchHeaderAxi4(Config.corundumDataWidth, Axi4Config(CorundumFrameMatchHeader.addressWidth, 32, 2, useQos = false, useRegion = false)))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameMatchHeaderAxi4(Config.corundumDataWidth, Axi4Config(CorundumFrameMatchHeader.addressWidth, 32, 2, useQos = false, useRegion = false)))
  }
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

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
