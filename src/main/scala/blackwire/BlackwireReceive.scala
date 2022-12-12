package blackwire

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

import corundum._

// companion object
object BlackwireReceive {
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new BlackwireReceive())
    //vhdlReport.mergeRTLSource("merge")
  }
}

// composition of the RX data flow towards ChaCha20-Poly1305
// stash -> downsizer -> key lookup ->
case class BlackwireReceive() extends Component {
  final val corundumDataWidth = 512
  final val cryptoDataWidth = 128
  final val maxPacketLength = 1534
  // 1534 rounded up 2048/(512/8) == 32

  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream Fragment(CorundumFrame(corundumDataWidth))
    val source = master Stream Fragment(CorundumFrame(corundumDataWidth))
  }

  // x is TDATA+TKEEP Ethernet frame from Corundum
  val x = Stream Fragment(CorundumFrame(corundumDataWidth))
  x << io.sink

  // fork x into two streams, Wireguard Type4 and other packet
  val type4_demux = new CorundumFrameDemuxWireguardType4(corundumDataWidth)
  type4_demux.io.sink << x

  // non-Type4 packet are dropped here (in fuller design, go into RISC-V Reader)
  val other_sinkhole = Stream Fragment(CorundumFrame(corundumDataWidth))
  other_sinkhole.ready := True

  val dropOnFull = CorundumFrameDrop(corundumDataWidth)
  val readerStash = CorundumFrameStash(corundumDataWidth, 32)
  dropOnFull.io.sink << type4_demux.io.source_other
  readerStash.io.sink << dropOnFull.io.source 
  dropOnFull.io.drop := (readerStash.io.availability < 2)
  other_sinkhole << readerStash.io.source

  // Type4 goes into stash
  val stash = CorundumFrameStash(corundumDataWidth, 32)
  stash.io.sink << type4_demux.io.source_type4

  // y is stash output but in TDATA+length format
  val y = Stream(Fragment(Bits(corundumDataWidth bits)))
  val fff = Fragment(Bits(corundumDataWidth bits))
  fff.last := stash.io.source.payload.last
  fff.fragment := stash.io.source.payload.fragment.tdata
  y << stash.io.source.translateWith(fff)
  val y_length = stash.io.length

  val w = Stream(Fragment(Bits(corundumDataWidth bits)))

  // w is with Ethernet, IPv4 and UDP headers removed, thus the Type 4 packet
  val headers = AxisExtractHeader(corundumDataWidth, 14 + 20 + 8)
  headers.io.sink << y
  headers.io.sink_length := y_length
  w << headers.io.source
  val w_length = headers.io.source_length

  // z is the Type 4 packet in 128 bits
  val z = Stream(Fragment(Bits(cryptoDataWidth bits)))
  val downsizer = AxisDownSizer(corundumDataWidth, cryptoDataWidth)
  downsizer.io.sink << w
  downsizer.io.sink_length := w_length
  z << downsizer.io.source
  val z_length = downsizer.io.source_length

  // upstream back pressure feedback across ChaCha20-Poly1305
  val output_stash_too_full = Bool()

  // k is the Type 4 packet in 128 bits together with key during first beat (Type 4 header)
  val rxkey = AxisWireguardKeyLookup(cryptoDataWidth, has_internal_test_lut = false)
  rxkey.io.sink << z
  rxkey.io.sink_length := z_length

  val k = Stream(Fragment(Bits(cryptoDataWidth bits)))
  k << rxkey.io.source
  val key = rxkey.io.key_out
  val k_length = rxkey.io.source_length
 
  // s is the decrypted Type 4 payload but with the length determined from the IP header
  val s = Stream(Fragment(Bits(cryptoDataWidth bits)))
  val s_length = Reg(UInt(12 bits))
  val s_drop = Reg(Bool()) init(False)

  val include_chacha = true
  (include_chacha) generate new Area {

    // p is the decrypted Type 4 payload
    val p = Stream(Fragment(Bits(cryptoDataWidth bits)))
    val decrypt = ChaCha20Poly1305DecryptSpinal()
    decrypt.io.sink << k.haltWhen(output_stash_too_full)
    decrypt.io.key := key
    p << decrypt.io.source

    // from the first word, extract the IPv4 Total Length field to determine packet length
    when (p.isFirst) {
      //s_length.assignFromBits(s.payload.fragment.asBits()(16, 16 bits))
      s_length.assignFromBits(p.payload.fragment(16, 16 bits).resize(12))
    }
    s <-< p
    // @NOTE tag_valid is unknown before TLAST beats, so AND it with TLAST
    // so that we do forward an unknown drop signal on non-last beats to the output
    s_drop := (p.last & !decrypt.io.tag_valid)
  }
  (!include_chacha) generate new Area {
    s << k.haltWhen(output_stash_too_full)
    s_length := k_length
  }

  // u is the decrypted Type 4 payload but in 512 bits
  val u = Stream(Fragment(Bits(corundumDataWidth bits)))
  val upsizer = AxisUpSizer(cryptoDataWidth, corundumDataWidth)
  // @NOTE consider pipeline stage
  upsizer.io.sink << s
  upsizer.io.sink_length := s_length
  upsizer.io.sink_drop := s_drop
  u << upsizer.io.source
  val u_length = upsizer.io.source_length
  val u_drop = upsizer.io.source_drop

  printf("Upsizer Latency = %d clock cycles.\n", LatencyAnalysis(s.valid, u.valid))

  // c is the decrypted Type 4 payload but in 512 bits in Corundum format
  // c does not experience back pressure during a packet out
  val c = Stream Fragment(CorundumFrame(corundumDataWidth))
  val corundum = AxisToCorundumFrame(corundumDataWidth)
  // @NOTE consider pipeline stage
  corundum.io.sink << u
  corundum.io.sink_length := u_length
  corundum.io.sink_drop := u_drop
  c << corundum.io.source
  
  // r is the decrypted Type 4 payload but in 512 bits in Corundum format
  // r can receive back pressure from Corundum
  val r = Stream Fragment(CorundumFrame(corundumDataWidth))

  val outstash = CorundumFrameOutputStash(corundumDataWidth, 32, 26)
  outstash.io.sink << c
  r << outstash.io.source

  output_stash_too_full := !outstash.io.sink.ready

  io.source << r

  //printf("x to r = %d clock cycles.\n", LatencyAnalysis(x.valid, r.valid))


  val keys_num = 256
  val lut = LookupTable(256/*bits*/, keys_num, ClockDomain.current)
  lut.mem.initBigInt(Seq.fill(keys_num)(BigInt("80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f".split(" ").reverse.mkString(""), 16)))

  lut.io.portA.en := True
  lut.io.portA.wr := False
  lut.io.portA.wrData := 0
  lut.io.portA.addr := rxkey.io.receiver.resize(log2Up(keys_num))
  rxkey.io.key_in := lut.io.portA.rdData

  //lut.io.portB.clk := ClockDomain.current.readClockWire
  //lut.io.portB.rst := False
  lut.io.portB.en := True
  lut.io.portB.wr := False
  lut.io.portB.wrData := 0
  lut.io.portB.addr := 0

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object BlackwireReceiveSim {
  def main(args: Array[String]) {
    val dataWidth = 512
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    SimConfig

    // GHDL
    //.withGhdl.withFstWave
    //.addRunFlag("--unbuffered").addRunFlag("--disp-tree=inst")
    //.addRunFlag("--ieee-asserts=disable").addRunFlag("--assert-level=none")
    //.addRunFlag("--backtrace-severity=warning")
    
    //.withXSim.withXilinxDevice("xcu50-fsvh2104-2-e")
    //.addSimulatorFlag("--ieee=standard")
    //.addSimulatorFlag("-v")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unisim/v93")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unimacro/v93") 
    // these define bus_pkg and bus_pkg1

    .addRtl(s"MaximVHDL/imports/project_1/ChaCha20.vhd")
    .addRtl(s"MaximVHDL/new/AEAD_ChaCha_Poly.vhd")

    .addRtl(s"MaximVHDL/imports/project_1/q_round.vhd")
    .addRtl(s"MaximVHDL/imports/project_1/diag_round.vhd")
    .addRtl(s"MaximVHDL/imports/project_1/col_round.vhd")
    .addRtl(s"MaximVHDL/imports/project_1/half_round.vhd")
    .addRtl(s"MaximVHDL/new/test_top_ChaCha.vhd")
    .addRtl(s"MaximVHDL/new/Poly1305.vhd")
    .addRtl(s"MaximVHDL/new/ChaCha20_128.vhd")
    .addRtl(s"MaximVHDL/new/mul136_mod_red.vhd")
    .addRtl(s"MaximVHDL/new/mul_red_pipeline.vhd")
    .addRtl(s"MaximVHDL/new/test_top_mod_red.vhd")
    .addRtl(s"MaximVHDL/new/ChaCha_int.vhd")
    .addRtl(s"MaximVHDL/new/r_power_n.vhd")
    .addRtl(s"MaximVHDL/mul_gen_0.vhd")
    .addRtl(s"MaximVHDL/new/mul_36.vhd")
    .addRtl(s"MaximVHDL/new/mul_72.vhd")
    .addRtl(s"MaximVHDL/new/mul_144.vhd")
    .addRtl(s"MaximVHDL/new/mod_red_1305.vhd")
    .addRtl(s"MaximVHDL/new/Poly_1305_pipe_top.vhd")
    //.addRtl(s"MaximVHDL/new/test_top_Poly.vhd")
    .addRtl(s"MaximVHDL/new/Poly_1305_pipe.vhd")
    .addRtl(s"MaximVHDL/new/AEAD_decryption_top.vhd")
    .addRtl(s"MaximVHDL/new/AEAD_top.vhd")
    .addRtl(s"MaximVHDL/new/Poly_pipe_top_test.vhd")
    .addRtl(s"MaximVHDL/new/AEAD_decryption.vhd")
    .addRtl(s"MaximVHDL/AEAD_decryption_wrapper.vhd")

    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    .doSim(BlackwireReceive()){dut =>

      dut.io.sink.valid #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var data0 = 0

      var last0 = false
      var valid0 = false
      var tkeep0 = BigInt(0)
      var pause = false

      dut.io.sink.valid #= valid0
      dut.io.sink.payload.tdata #= 0
      dut.io.sink.last #= last0
      dut.io.sink.payload.tkeep #= tkeep0
      dut.io.sink.payload.tuser #= 0


      dut.clockDomain.waitSampling()

      // 000  4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c  Ladies and Gentl
      // 016  65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73  emen of the clas
      // 032  73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63  s of '99: If I c
      // 048  6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f  ould offer you o
      // 064  6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20  nly one tip for
      // 080  74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73  the future, suns
      // 096  63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69  creen would be i
      // 112  74 2e                                            t.

// "0102030405060102030405060102" Ethernet
// "xxxx11887766554433221145" IPv4, IHL=5, protocol=0x11 (UDP)
// "0000FF0000000000000000FF"
// "CCCCLLLLb315SSSS", DDDD=port 5555 (0x15b3)
// "00000000FFFF0000"

      var packet_number = 0
      val inter_packet_gap = 1
      while (packet_number < 3) {

        val plaintext1 = Vector(
          //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------> < L a  d  i  e  s
          BigInt("01 02 03 04 05 06 01 02 03 04 05 06 01 02 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 4c 61 64 69 65 73".split(" ").reverse.mkString(""), 16),
          //          a  n  d     G  e  n  t  l  e  m  e  n     o  f     t  h  e     c  l  a  s  s     o  f     '  9  9  :     I  f     I     c  o  u  l  d     o  f  f  e  r     y  o  u     o  n  l  y     o  n
          BigInt("20 61 6e 64 20 47 65 6e 74 6c 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63 6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f 6e 6c 79 20 6f 6e".split(" ").reverse.mkString(""), 16),
          //       e     t  i  p     f  o  r     t  h  e     f  u  t  u  r  e  ,     s  u  n  s  c  r  e  e  n     w  o  u  l  d     b  e     i  t  . <---------- Poly 1305 Tag (16 bytes) --------->
          BigInt("65 20 74 69 70 20 66 6f 72 20 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69 74 2e 13 05 13 05 13 05 13 05 13 05 13 05 13 05 13 05 00 00 00 00".split(" ").reverse.mkString(""), 16)
        )
        // 64 - 6 = 58 bytes for all headers
        // 3 * 64 bytes - 4 = 188 bytes for full Ethernet packet (as above)
        // 188 - 58 = 130 bytes for encrypted/decrypted (16 bytes ceiling padded) payload and the Poly1305 tag
        // 130 - 16 = 114 bytes for encrypted/decrypted (16 bytes ceiling padded) )payload
        // 114 bytes fits in 8 128-bit words
        //var packet_length = 16 + 114 // bytes

        val plaintext = Vector(
          Vector(
            //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------>< L  a  d  i  e  s
            BigInt("01 02 03 04 05 06 01 02 03 04 05 06 08 00 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 00 01 02 03 04 05 06 07 D3 1A 8D 34 64 8E".split(" ").reverse.mkString(""), 16),
            //          a  n  d     G  e  n  t  l  e  m  e  n     o  f     t  h  e     c  l  a  s  s     o  f     '  9  9  :     I  f     I     c  o  u  l  d     o  f  f  e  r     y  o  u     o  n  l  y     o  n
            BigInt("60 DB 7B 86 AF BC 53 EF 7E C2 A4 AD ED 51 29 6E 08 FE A9 E2 B5 A7 36 EE 62 D6 3D BE A4 5E 8C A9 67 12 82 FA FB 69 DA 92 72 8B 1A 71 DE 0A 9E 06 0B 29 05 D6 A5 B6 7E CD 3B 36 92 DD BD 7F 2D 77".split(" ").reverse.mkString(""), 16),
            //       e     t  i  p     f  o  r     t  h  e     f  u  t  u  r  e  ,     s  u  n  s  c  r  e  e  n     w  o  u  l  d     b  e     i  t  . 
            //                                                                                                                                                                                    <-Poly 1305 Tag...
            BigInt("8B 8C 98 03 AE E3 28 09 1B 58 FA B3 24 E4 FA D6 75 94 55 85 80 8B 48 31 D7 BC 3F F4 DE F0 8E 4B 7A 9D E5 76 D2 65 86 CE C6 4B 61 16 0C 03 CE 35 6C D7 A6 76 63 FB DD EF FF 64 20 57 AD C1 AF 44".split(" ").reverse.mkString(""), 16),
            //      ...Poly 1305 Tag (16 bytes)->
            //BigInt("E7 01 50 87 55 CA 0A CC 8D 4E 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
            BigInt("E7 01 50 87 55 CA 0A CC 8D 4E 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
          ),
          // invalid TAG (last byte wrong)
          Vector(
            //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 2 ------------------------>< L  a  d  i  e  s
            BigInt("01 02 03 04 05 06 01 02 03 04 05 06 08 00 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 00 01 02 03 04 05 06 07 D3 1A 8D 34 64 8E".split(" ").reverse.mkString(""), 16),
            //          a  n  d     G  e  n  t  l  e  m  e  n     o  f     t  h  e     c  l  a  s  s     o  f     '  9  9  :     I  f     I     c  o  u  l  d     o  f  f  e  r     y  o  u     o  n  l  y     o  n
            BigInt("60 DB 7B 86 AF BC 53 EF 7E C2 A4 AD ED 51 29 6E 08 FE A9 E2 B5 A7 36 EE 62 D6 3D BE A4 5E 8C A9 67 12 82 FA FB 69 DA 92 72 8B 1A 71 DE 0A 9E 06 0B 29 05 D6 A5 B6 7E CD 3B 36 92 DD BD 7F 2D 77".split(" ").reverse.mkString(""), 16),
            //       e     t  i  p     f  o  r     t  h  e     f  u  t  u  r  e  ,     s  u  n  s  c  r  e  e  n     w  o  u  l  d     b  e     i  t  . 
            //                                                                                                                                                                                    <-Poly 1305 Tag...
            BigInt("8B 8C 98 03 AE E3 28 09 1B 58 FA B3 24 E4 FA D6 75 94 55 85 80 8B 48 31 D7 BC 3F F4 DE F0 8E 4B 7A 9D E5 76 D2 65 86 CE C6 4B 61 16 0C 03 CE 35 6C D7 A6 76 63 FB DD EF FF 64 20 57 AD C1 AF 44".split(" ").reverse.mkString(""), 16),
            //      ...Poly 1305 Tag (16 bytes)->
            //BigInt("E7 01 50 87 55 CA 0A CC 8D 4E 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
            BigInt("E7 01 50 87 55 CA 0A CC 8D 4F 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
          ),
          Vector(
            //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------>< L  a  d  i  e  s
            BigInt("01 02 03 04 05 06 01 02 03 04 05 06 08 00 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 00 01 02 03 04 05 06 07 D3 1A 8D 34 64 8E".split(" ").reverse.mkString(""), 16),
            //          a  n  d     G  e  n  t  l  e  m  e  n     o  f     t  h  e     c  l  a  s  s     o  f     '  9  9  :     I  f     I     c  o  u  l  d     o  f  f  e  r     y  o  u     o  n  l  y     o  n
            BigInt("60 DB 7B 86 AF BC 53 EF 7E C2 A4 AD ED 51 29 6E 08 FE A9 E2 B5 A7 36 EE 62 D6 3D BE A4 5E 8C A9 67 12 82 FA FB 69 DA 92 72 8B 1A 71 DE 0A 9E 06 0B 29 05 D6 A5 B6 7E CD 3B 36 92 DD BD 7F 2D 77".split(" ").reverse.mkString(""), 16),
            //       e     t  i  p     f  o  r     t  h  e     f  u  t  u  r  e  ,     s  u  n  s  c  r  e  e  n     w  o  u  l  d     b  e     i  t  . 
            //                                                                                                                                                                                    <-Poly 1305 Tag...
            BigInt("8B 8C 98 03 AE E3 28 09 1B 58 FA B3 24 E4 FA D6 75 94 55 85 80 8B 48 31 D7 BC 3F F4 DE F0 8E 4B 7A 9D E5 76 D2 65 86 CE C6 4B 61 16 0C 03 CE 35 6C D7 A6 76 63 FB DD EF FF 64 20 57 AD C1 AF 44".split(" ").reverse.mkString(""), 16),
            //      ...Poly 1305 Tag (16 bytes)->
            //BigInt("E7 01 50 87 55 CA 0A CC 8D 4E 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
            BigInt("E7 01 50 87 55 CA 0A CC 8D 4E 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
          )
        )
        var packet_length = 3 * 64 + 10 //bytes

        var remaining = packet_length

        var word_index = 0
        // iterate over frame content
        while (remaining > 0) {
          printf("remaining = %d\n", remaining)
          val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
          printf("tkeep_len = %d\n", tkeep_len)
          valid0 = (Random.nextInt(8) > 2)
          valid0 &= !pause
          if (pause) pause ^= (Random.nextInt(16) >= 15)
          if (!pause) pause ^= (Random.nextInt(128) >= 127)

          assert(tkeep_len <= keepWidth)
          tkeep0 = 0
          data0 = 0
          if (valid0) {
            last0 = (remaining <= keepWidth)
            for (i <- 0 until tkeep_len) {
              tkeep0 = (tkeep0 << 1) | 1
            }
          }

          dut.io.sink.valid #= valid0
          dut.io.sink.payload.tdata #= plaintext(packet_number)(word_index)
          dut.io.sink.last #= last0
          dut.io.sink.last #= last0
          dut.io.sink.payload.tkeep #= tkeep0
          dut.io.sink.payload.tuser #= 0

          dut.io.source.ready #= (Random.nextInt(8) > 1)

          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()

          if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
            remaining -= tkeep_len
            word_index += 1
          }
        }
        dut.io.sink.valid #= false

        printf("remaining = %d after while (remaining > 0))\n", remaining)
        assert(remaining == 0)

        dut.clockDomain.waitRisingEdge(inter_packet_gap)

        packet_number += 1
      } // while remaining_packets

      dut.io.source.ready #= true

      dut.clockDomain.waitRisingEdge(8 + 32 + 290)

      while (dut.io.source.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge(8)
      dut.clockDomain.waitRisingEdge(8)
    }
  }
}
