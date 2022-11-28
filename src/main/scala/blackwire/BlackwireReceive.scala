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
    SpinalVerilog(new BlackwireReceive())
    //SpinalVhdl(new BlackwireReceive())
    
    val report = SpinalConfig(
      mode = VHDL
    ).generate(new BlackwireReceive())

    report.mergeRTLSource("merge")
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
  val type4_demux = new CorundumFrameDemuxWireguard()
  type4_demux.io.sink << x

  // nob-Type4 packet are dropped here (in fuller design, go into RISC-V Reader)
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

  val include_chacha = false
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
  }
  (!include_chacha) generate new Area {
    s << k.haltWhen(output_stash_too_full)
    s_length := k_length
  }

  // u is the decrypted Type 4 payload but in 512 bits
  val u = Stream(Fragment(Bits(corundumDataWidth bits)))
  val upsizer = AxisUpSizer(cryptoDataWidth, corundumDataWidth)
  upsizer.io.sink << s
  upsizer.io.sink_length := s_length
  u << upsizer.io.source
  val u_length = upsizer.io.source_length

  printf("Upsizer Latency = %d clock cycles.\n", LatencyAnalysis(s.valid, u.valid))

  // c is the decrypted Type 4 payload but in 512 bits in Corundum format
  // c does not experience back pressure during a packet out
  val c = Stream Fragment(CorundumFrame(corundumDataWidth))
  val corundum = AxisToCorundumFrame(corundumDataWidth)
  corundum.io.sink << u
  corundum.io.sink_length := u_length
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
    .withVcdWave
    .withGhdl
    .addRtl(s"/home/vexriscv/project/SpinalCorundum/ChaCha20Poly1305Decrypt.vhd")

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

      val plaintext = Vector(
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
      var packet_length = 16 + 114 // bytes
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
        dut.io.sink.payload.tdata #= plaintext(word_index)
        dut.io.sink.last #= last0
        dut.io.sink.payload.tkeep #= tkeep0

        dut.io.source.ready #= (Random.nextInt(8) > 1)

        // Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
          remaining -= tkeep_len
          word_index += 1
        }
      }
      dut.io.sink.valid #= false
      dut.io.source.ready #= true

      dut.clockDomain.waitRisingEdge(8 + 32)

      while (dut.io.source.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge(8)
      dut.clockDomain.waitRisingEdge(8)
    }
  }
}
