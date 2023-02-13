package blackwire

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

import corundum._

// companion object
object BlackwireReceive {
  val busconfig = Axi4Config(15, 32, 2, useLock = false, useQos = false, useRegion = false)
  def main(args: Array[String]) : Unit = {
    val vhdlReport = Config.spinal.generateVhdl(new BlackwireReceive(busconfig))
    val verilogReport = Config.spinal.generateVerilog(new BlackwireReceive(busconfig))
    //vhdlReport.mergeRTLSource("merge")
  }
}

// composition of the RX data flow towards ChaCha20-Poly1305
// stash -> downsizer -> key lookup ->
case class BlackwireReceive(busCfg : Axi4Config, include_chacha : Boolean = true) extends Component {
  final val corundumDataWidth = 512
  final val cryptoDataWidth = 128
  final val maxPacketLength = 1534
  final val keys_num = 4/*sessions per peers*/ * 256/*maximum number of peers*/

  // 1534 rounded up 2048/(512/8) == 32

  final val rxkey_addr_width = LookupTableAxi4.slave_width(256, keys_num, busCfg)
  println("rxkey_addr_width = " + rxkey_addr_width)

  val rxkeySlaveCfg = busCfg.copy(addressWidth = rxkey_addr_width)

  val io = new Bundle {
    // I/O is the Corundum Frame AXIS tdata/tkeep/tuser format payload
    val sink = slave Stream Fragment(CorundumFrame(corundumDataWidth))
    val source = master Stream Fragment(CorundumFrame(corundumDataWidth))

    val source_handshake = master Stream Fragment(CorundumFrame(corundumDataWidth))
    val ctrl_rxkey = slave(Axi4(rxkeySlaveCfg))
  }

  // x is TDATA+TKEEP Ethernet frame from Corundum
  val x = Stream Fragment(CorundumFrame(corundumDataWidth))
  x << io.sink

  // fork x into two streams, Wireguard Type4 and other packet
  val type4_demux = new CorundumFrameDemuxWireguardType4(corundumDataWidth)
  type4_demux.io.sink << x

  // non-Type4 packet are routed here
  val dropOnFull = CorundumFrameDrop(corundumDataWidth)
  val readerStash = CorundumFrameStash(corundumDataWidth, 32)
  dropOnFull.io.sink << type4_demux.io.source_other
  readerStash.io.sink << dropOnFull.io.source 
  dropOnFull.io.drop := (readerStash.io.availability < 2)
  io.source_handshake << readerStash.io.source

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

  val with_chacha = (include_chacha) generate new Area { 
    // p is the decrypted Type 4 payload
    val p = Stream(Fragment(Bits(cryptoDataWidth bits)))
    val decrypt = ChaCha20Poly1305DecryptSpinal()
    decrypt.io.sink << k.haltWhen(output_stash_too_full)
    decrypt.io.key := key
    p << decrypt.io.source
    //decrypt.io.addAttribute("mark_debug")

    // from the first word, extract the IPv4 Total Length field to determine packet length
    when (p.isFirst) {
      s_length.assignFromBits(p.payload.fragment(16, 16 bits).resize(12))
    }
    s <-< p
    // @NOTE tag_valid is unknown before TLAST beats, so AND it with TLAST
    // so that we do forward an unknown drop signal on non-last beats to the output
    s_drop := (p.last & !decrypt.io.tag_valid)
  }
  val without_chacha = (!include_chacha) generate new Area { 
    s << k.haltWhen(output_stash_too_full)
    s_length := k_length
    s_drop := False
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

  // should be room for 1534 + latency of ChaCha20 to FlowStash
  // Flow goes ready after packet last, and room for 26*64=1664 bytes
  val outstash = CorundumFrameFlowStash(corundumDataWidth, fifoSize = 32, 26)
  outstash.io.sink << c
  r << outstash.io.source

  output_stash_too_full := !outstash.io.sink.ready

  val ethhdr = CorundumFrameInsertHeader(corundumDataWidth, 14)
  ethhdr.io.sink << r
  ethhdr.io.header := B("112'x000a3506a3beaabbcc2222220800").subdivideIn(14 slices).reverse.asBits()
  val h = Stream Fragment(CorundumFrame(corundumDataWidth))
  h << ethhdr.io.source

  val fcs = CorundumFrameAppendTailer(corundumDataWidth, 4)
  fcs.io.sink << h
  val f = Stream Fragment(CorundumFrame(corundumDataWidth))
  f << fcs.io.source

  io.source << f

  //printf("x to r = %d clock cycles.\n", LatencyAnalysis(x.valid, r.valid))

  val has_busctrl = true
  (!has_busctrl) generate new Area {
  val lut = LookupTable(256/*bits*/, keys_num/*, ClockDomain.current*/)

  lut.mem.initBigInt(Seq.fill(keys_num)(BigInt("80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f".split(" ").reverse.mkString(""), 16)))

  lut.io.portA.en := True
  lut.io.portA.wr := False
  lut.io.portA.wrData := 0
  lut.io.portA.addr := rxkey.io.receiver.resize(log2Up(keys_num))
  rxkey.io.key_in := lut.io.portA.rdData

  lut.io.portB.en := True
  lut.io.portB.wr := False
  lut.io.portB.wrData := 0
  lut.io.portB.addr := 0
  }
  // RX key lookup and update via bus controller
  (has_busctrl) generate new Area {
  val lut = LookupTableAxi4(256/*bits*/, keys_num, busCfg)
  lut.mem.mem.initBigInt(Seq.fill(keys_num)(BigInt("80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f".split(" ").reverse.mkString(""), 16)))
  lut.io.en := True
  lut.io.wr := False
  lut.io.wrData := 0
  rxkey.io.receiver.addAttribute("mark_debug")
  val lut_address = U(rxkey.io.receiver.asBits.subdivideIn(4 slices).reverse.asBits.resize(log2Up(keys_num)))
  lut_address.addAttribute("mark_debug")
  lut.io.addr := lut_address //rxkey.io.receiver.resize(log2Up(keys_num))
  
  rxkey.io.key_in := lut.io.rdData
  io.ctrl_rxkey >> lut.io.ctrlbus
  }
  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object BlackwireReceiveSim {
  def main(args: Array[String]) : Unit = {
    val dataWidth = 512
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    val include_chacha = true

    SimConfig
    // GHDL can simulate VHDL, required for ChaCha20Poly1305
    .withGhdl.withFstWave
    //.addRunFlag support is now in SpinalHDL dev branch
    .addRunFlag("--unbuffered") //.addRunFlag("--disp-tree=inst")
    .addRunFlag("--ieee-asserts=disable").addRunFlag("--assert-level=none")
    .addRunFlag("--backtrace-severity=warning")
    
    //.withXSim.withXilinxDevice("xcu50-fsvh2104-2-e")
    //.addSimulatorFlag("--ieee=standard")
    //.addSimulatorFlag("-v")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unisim/v93")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unimacro/v93") 
    // these define bus_pkg and bus_pkg1

    .addRtl(s"../ChaCha20Poly1305/src/ChaCha20.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/AEAD_ChaCha_Poly.vhd")

    .addRtl(s"../ChaCha20Poly1305/src/q_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/diag_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/col_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/half_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/test_top_ChaCha.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/Poly1305.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/ChaCha20_128.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/mul136_mod_red.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/mul_red_pipeline.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/test_top_mod_red.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/ChaCha_int.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/r_power_n.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/mul_gen_0.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/mul_36.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/mul_72.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/mul_144.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/mod_red_1305.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/Poly_1305_pipe_top.vhd")
    //.addRtl(s"../ChaCha20Poly1305/src/test_top_Poly.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/Poly_1305_pipe.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/AEAD_decryption_top.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/AEAD_top.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/Poly_pipe_top_test.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/AEAD_decryption.vhd")
    .addRtl(s"../ChaCha20Poly1305/src/AEAD_decryption_wrapper.vhd")

    //.addRtl(s"../ChaCha20Poly1305/src/convert/aead_decryption_wrapper.v")

    .compile {
      val dut = new BlackwireReceive(BlackwireReceive.busconfig, include_chacha = include_chacha)
      dut.with_chacha.decrypt.io.source.ready.simPublic()
      dut.with_chacha.decrypt.io.source.valid.simPublic()
      dut.with_chacha.decrypt.io.source.last.simPublic()
      dut.with_chacha.decrypt.io.tag_valid.simPublic()
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    // include_chacha = true requires GHDL or XSim
    .doSim { dut =>
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

// "0102030405060102030405060102" Ethernet
// "xxxx11887766554433221145" IPv4, IHL=5, protocol=0x11 (UDP)
// "0000FF0000000000000000FF"
// "CCCCLLLLb315SSSS", DDDD=port 5555 (0x15b3)
// "00000000FFFF0000"

      var packet_number = 0
      val inter_packet_gap = 1

      val plaintext = Vector(
        // RFC7539 2.8.2. Example and Test Vector for AEAD_CHACHA20_POLY1305
        // but with zero-length AAD, and Wireguard 64-bit nonce
        Vector(
          //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <-Wireguard Type 4, I-> <-- Wireguard NONCE --> <L  a  d  i  e  s
          BigInt("01 02 03 04 05 06 01 02 03 04 05 06 08 00 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 40 41 42 43 44 45 46 47 a4 79 cb 54 62 89".split(" ").reverse.mkString(""), 16),
          BigInt("46 d6 f4 04 2a 8e 38 4e f4 bd 2f bc 73 30 b8 be 55 eb 2d 8d c1 8a aa 51 d6 6a 8e c1 f8 d3 61 9a 25 8d b0 ac 56 95 60 15 b7 b4 93 7e 9b 8e 6a a9 57 b3 dc 02 14 d8 03 d7 76 60 aa bc 91 30 92 97".split(" ").reverse.mkString(""), 16),
          BigInt("1d a8 f2 07 17 1c e7 84 36 08 16 2e 2e 75 9d 8e fc 25 d8 d0 93 69 90 af 63 c8 20 ba 87 e8 a9 55 b5 c8 27 4e f7 d1 0f 6f af d0 46 47 1b 14 57 76 ac a2 f7 cf 6a 61 d2 16 64 25 2f b1 f5 ba d2 ee".split(" ").reverse.mkString(""), 16),
          BigInt("98 e9 64 8b b1 7f 43 2d cc e4 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        )
      )
      while (packet_number < 1) {
        var packet_length = 3 * 64 + 10 // MUST MATCH "plaintext"

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

          //if (dut.decrypt.io.sink.ready.toBoolean & dut.decrypt.io.sink.valid.toBoolean) {
          //}
        }
        dut.io.sink.valid #= false

        printf("remaining = %d after while (remaining > 0))\n", remaining)
        assert(remaining == 0)

        dut.clockDomain.waitRisingEdge(inter_packet_gap)

        packet_number += 1
      } // while remaining_packets

      dut.io.source.ready #= true

      if (include_chacha) {
      var limit = 500
      var good_packets = 0
      while ((limit > 0)/* && (good_packets == 0)*/) {
          if (dut.with_chacha.decrypt.io.source.ready.toBoolean &
              dut.with_chacha.decrypt.io.source.valid.toBoolean &
              dut.with_chacha.decrypt.io.source.last.toBoolean
              ) {
                printf("dut.with_chacha.decrypt.io.tag_valid = %b\n", dut.with_chacha.decrypt.io.tag_valid.toBoolean)
                if (dut.with_chacha.decrypt.io.tag_valid.toBoolean == true) {
                  good_packets += 1
                }
          }
          dut.clockDomain.waitRisingEdge()
          limit -= 1//          limit -= 1
      }
      assert(good_packets == 1)
      }

      dut.clockDomain.waitRisingEdge(8)

    }
  }
}
