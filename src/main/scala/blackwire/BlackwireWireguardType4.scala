package blackwire

import spinal.core._
import spinal.lib._

import scala.math._

import corundum._

// companion object
object BlackwireWireguardType4 {
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new BlackwireWireguardType4())
    Config.spinal.generateVerilog(new BlackwireWireguardType4())
  }
}

// composition of the RX data flow towards ChaCha20-Poly1305
// stash -> downsizer -> key lookup -> endianess
case class BlackwireWireguardType4() extends Component {
 final val corundumDataWidth = 512
 final val cryptoDataWidth = 128
 final val maxPacketLength = 1534
  // 1534 rounded up 2048/(512/8) == 32

  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream Fragment(CorundumFrame(corundumDataWidth))
    val source = master Stream(Fragment(Bits(cryptoDataWidth bits)))
    //val key = out Bits(256 bits)
    //val latency = out UInt(8 bits)
  }

  //val reverse = CorundumFrameEndianess(corundumDataWidth)
  //reverse.io.sink << io.sink
  
  val stash = CorundumFrameStash(corundumDataWidth, 32)
  stash.io.sink << io.sink //reverse.io.source

  // extract only TDATA into fragment
  val x = Stream(Fragment(Bits(corundumDataWidth bits)))
  val fff = Fragment(Bits(corundumDataWidth bits))
  fff.last := stash.io.source.payload.last
  fff.fragment := stash.io.source.payload.fragment.tdata
  x << stash.io.source.translateWith(fff)

  val downsizer = AxisDownSizer(corundumDataWidth, cryptoDataWidth)
  downsizer.io.sink << x
  downsizer.io.sink_length := stash.io.length
  
  val rxkey = AxisWireguardKeyLookup(cryptoDataWidth, has_internal_test_lut = false)
  rxkey.io.sink << downsizer.io.source
  rxkey.io.sink_length := downsizer.io.source_length

  // p is the decrypted Type 4 payload
  val p = Stream(Fragment(Bits(cryptoDataWidth bits)))

  val decrypt = ChaCha20Poly1305DecryptSpinal()
 
  // AXIS from Key Lookup ouput to ChaCha20Poly1305 input
  decrypt.io.sink << rxkey.io.source
  // Key from Key Lookup ouput to ChaCha20Poly1305 input
  decrypt.io.key := rxkey.io.key_out

  // p is the decrypted AXIS from ChaCha20Poly1305 output
  p << decrypt.io.source

  // KeyLookup now adapts for endianess of ChaCha
  //val endianess = AxisEndianess(cryptoDataWidth)
  //endianess.io.sink << rxkey.io.source
  io.source << p

  val keys_num = 256
  val lut = LookupTable(256/*bits*/, keys_num/*, ClockDomain.current*/)
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

 // io.latency := U(LatencyAnalysis(io.sink.valid, io.source.valid))

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

import spinal.sim._
import spinal.core.sim._
import scala.util.Random

// this assumes reversed byte order on the DUT AXIS!
// the plaintext Vector is adapted for it
// the tkeep0 has an extra loop to reverse bit order
// @TODO endianess of ChaCha input should be made correct, and this reverted.
object BlackwireWireguardType4Sim {
  def main(args: Array[String]) {
    val dataWidth = 512
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    SimConfig
    .withGhdl.withFstWave
    //.addRunFlag support is now in SpinalHDL dev branch
    .addRunFlag("--unbuffered") //.addRunFlag("--disp-tree=inst")
    .addRunFlag("--ieee-asserts=disable").addRunFlag("--assert-level=none")
    .addRunFlag("--backtrace-severity=warning")
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
    //.addRtl(s"../ChaCha20Poly1305/convert/AEAD_decryption_wrapper.v")

    //.withFstWave
    .doSim(BlackwireWireguardType4()){dut =>

      dut.io.sink.valid #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var data0 = 0

      var last0 = false
      var valid0 = false
      var tkeep0 = BigInt(0)
      var pause = false

      dut.clockDomain.waitSampling()

      dut.io.sink.tuser #= 0

      // nonce hardcoded 00 00 00 00 (40 41 42 43 44 45 46 47) in the aead_decryption_wrapper.vhd
      //       nonce         <= x"00000000"&msg_reordered(63 downto 0);
      val plaintext = Vector(
        Vector(
          BigInt("04 00 00 80 00 00 00 01 40 41 42 43 44 45 46 47 a4 79 cb 54 62 89 46 d6 f4 04 2a 8e 38 4e f4 bd 2f bc 73 30 b8 be 55 eb 2d 8d c1 8a aa 51 d6 6a 8e c1 f8 d3 61 9a 25 8d b0 ac 56 95 60 15 b7 b4".split(" ").reverse.mkString(""), 16),
          BigInt("93 7e 9b 8e 6a a9 57 b3 dc 02 14 d8 03 d7 76 60 aa bc 91 30 92 97 1d a8 f2 07 17 1c e7 84 36 08 16 2e 2e 75 9d 8e fc 25 d8 d0 93 69 90 af 63 c8 20 ba 87 e8 a9 55 b5 c8 27 4e f7 d1 0f 6f af d0".split(" ").reverse.mkString(""), 16),
          BigInt("46 47 1b 14 57 76 ac a2 f7 cf 6a 61 d2 16 64 25 2f b1 f5 ba d2 ee 98 e9 64 8b b1 7f 43 2d cc e4 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        ),
        Vector(
          BigInt("04 00 00 00 00 00 00 01 40 41 42 43 44 45 46 47 5a 70 0f 88 e7 87 fe 1c 1e f6 64 e6 01 ba 93 5f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        ),
        Vector(
          BigInt("04 00 00 80 00 00 00 01 40 41 42 43 44 45 46 47 a4 79 cb 54 62 89 46 d6 f4 04 2a 8e 38 4e f4 bd 2f bc 73 30 b8 be 55 eb 2d 8d c1 8a aa 51 d6 6a 8e c1 f8 d3 61 9a 25 8d b0 ac 56 95 60 15 b7 b4".split(" ").reverse.mkString(""), 16),
          BigInt("93 7e 9b 8e 6a a9 57 b3 dc 02 14 d8 03 d7 76 60 aa bc 91 30 92 97 1d a8 f2 07 17 1c e7 84 36 08 16 2e 2e 75 9d 8e fc 25 d8 d0 93 69 90 af 63 c8 20 ba 87 e8 a9 55 b5 c8 27 4e f7 d1 0f 6f af d0".split(" ").reverse.mkString(""), 16),
          BigInt("46 47 1b 14 57 76 ac a2 f7 cf 6a 61 d2 16 64 25 2f b1 f5 ba d2 ee 98 e9 64 8b b1 7f 43 2d cc e4 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        )
      )

      printf("%s\n", "04 00 00 80 00 00 00 01 40 41 42 43 44 45 46 47 a4 79 cb 54 62 89 46 d6 f4 04 2a 8e 38 4e f4 bd 2f bc 73 30 b8 be 55 eb 2d 8d c1 8a aa 51 d6 6a 8e c1 f8 d3 61 9a 25 8d b0 ac 56 95 60 15 b7 b4".split(" ")/*.reverse*/.mkString(""))
      printf("%s\n", "93 7e 9b 8e 6a a9 57 b3 dc 02 14 d8 03 d7 76 60 aa bc 91 30 92 97 1d a8 f2 07 17 1c e7 84 36 08 16 2e 2e 75 9d 8e fc 25 d8 d0 93 69 90 af 63 c8 20 ba 87 e8 a9 55 b5 c8 27 4e f7 d1 0f 6f af d0".split(" ")/*.reverse*/.mkString(""))
      printf("%s\n", "46 47 1b 14 57 76 ac a2 f7 cf 6a 61 d2 16 64 25 2f b1 f5 ba d2 ee 98 e9 64 8b b1 7f 43 2d cc e4 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ")/*.reverse*/.mkString(""))
      printf("<idle cycle>\n");
      printf("%s\n", "04 00 00 00 00 00 00 01 40 41 42 43 44 45 46 47 5a 70 0f 88 e7 87 fe 1c 1e f6 64 e6 01 ba 93 5f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ")/*.reverse*/.mkString(""))
      printf("<idle cycle>\n");
      printf("%s\n", "04 00 00 80 00 00 00 01 40 41 42 43 44 45 46 47 a4 79 cb 54 62 89 46 d6 f4 04 2a 8e 38 4e f4 bd 2f bc 73 30 b8 be 55 eb 2d 8d c1 8a aa 51 d6 6a 8e c1 f8 d3 61 9a 25 8d b0 ac 56 95 60 15 b7 b4".split(" ")/*.reverse*/.mkString(""))
      printf("%s\n", "93 7e 9b 8e 6a a9 57 b3 dc 02 14 d8 03 d7 76 60 aa bc 91 30 92 97 1d a8 f2 07 17 1c e7 84 36 08 16 2e 2e 75 9d 8e fc 25 d8 d0 93 69 90 af 63 c8 20 ba 87 e8 a9 55 b5 c8 27 4e f7 d1 0f 6f af d0".split(" ")/*.reverse*/.mkString(""))
      printf("%s\n", "46 47 1b 14 57 76 ac a2 f7 cf 6a 61 d2 16 64 25 2f b1 f5 ba d2 ee 98 e9 64 8b b1 7f 43 2d cc e4 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ")/*.reverse*/.mkString(""))
      printf("<idle cycle>\n");

      var packet_lengths = Vector(64 + 64 + 16 + 16, 16 + 16, 64 + 64 + 16 + 16)

      var remaining_packets = 3
      val inter_packet_gap = 10
      var packet_idx = 0
      while (remaining_packets > 0) {
        // 512/8
        var packet_length = packet_lengths(packet_idx)
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

          valid0 = true
          
          assert(tkeep_len <= keepWidth)
          tkeep0 = 0
          data0 = 0
          if (valid0) {
            last0 = (remaining <= keepWidth)
            for (i <- 0 until tkeep_len) {
              tkeep0 = (tkeep0 << 1) | 1
            }
            // reverse bit order of TKEEP
            //var tkeep1 = tkeep0
            //tkeep0 = 0
            //for (i <- 0 until keepWidth) {
            //  tkeep0 = (tkeep0 << 1) | (tkeep1 & 1)
            //  tkeep1 = tkeep1 >> 1
            //}
          }

          dut.io.sink.valid #= valid0
          dut.io.sink.payload.tdata #= plaintext(packet_idx)(word_index)
          dut.io.sink.last #= last0
          dut.io.sink.payload.tkeep #= tkeep0


          // do not apply backpressure on ChaCha20
          dut.io.source.ready #= true

          // Wait for a rising edge on the clock
          dut.clockDomain.waitRisingEdge()

          if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
            remaining -= tkeep_len
            word_index += 1
          }
          dut.io.sink.valid #= false
          dut.io.sink.last #= false
        }
        printf("remaining = %d after while (remaining > 0))\n", remaining)
        assert(remaining == 0)

        dut.clockDomain.waitRisingEdge(inter_packet_gap)

        remaining_packets -= 1
        packet_idx += 1
      } // while remaining_packets

      //dut.io.sink.valid #= false
      dut.io.sink.payload.tdata #= BigInt(0)

      //printf("LATENCY=%d\n", dut.io.latency.toInt)

      dut.clockDomain.waitRisingEdge(/*dut.io.latency.toInt*/ 12 + 32 + 490 + inter_packet_gap)

      printf("VALID=%d, LAST=%d\n", dut.io.source.valid.toBigInt, dut.io.source.last.toBigInt)
      while (dut.io.source.valid.toBoolean) {
          // Wait for a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
          printf("VALID=%d, LAST=%d\n", dut.io.source.valid.toBigInt, dut.io.source.last.toBigInt)
      }
      //dut.clockDomain.waitRisingEdge(8)
      //dut.clockDomain.waitRisingEdge(8)
    }
  }
}


// companion object
object BlackwireWireguardType4Bench {
  def main(args: Array[String]) {
    SpinalVhdl(new BlackwireWireguardType4Bench())
  }
}

case class BlackwireWireguardType4Bench() extends Component {
  final val corundumDataWidth = 512

  val dut = new BlackwireWireguardType4()

  val counter = Reg(UInt(3 bits)).init(0)

  val source = Stream Fragment(CorundumFrame(corundumDataWidth))
  source.tuser := 0
  source.tdata := 0
  source.tkeep := 0
  source.valid := False
  source.last := False
  when (source.ready) {
    when (counter === 0) {
      source.tdata := B"512'h8B7292DA69FBFA821267A98C5EA4BE3D_D662EE36A7B5E2A9FE086E2951EDADA4_C27EEF53BCAF867BDB608E64348D1AD3_07060504030201000100000090000004";
      source.tkeep := B"64'hFFFFFFFFFFFFFFFF"
      source.valid := True
    }
    .elsewhen (counter === 1) {
      source.tdata := B"512'h4BC6CE8665D276E59D7A4B8EF0DEF43F_BCD731488B8085559475D6FAE424B3FA_581B0928E3AE03988C8B772D7FBDDD92_363BCD7EB6A5D605290B069E0ADE711A";
      source.tkeep := B"64'hFFFFFFFFFFFFFFFF"
    }
    .elsewhen (counter === 2) {
      source.tdata := B"512'h00000000000000000000000000000000_00000000000000000000000000000000_5a12894a779abf9fab3103df54171e80_64FFEFDDFB6376A6D76C35CE030C1661";
      source.tkeep := B"64'h00000000FFFFFFFF"
      source.last  := True
    }
    .elsewhen (counter === 3) {
      source.valid := False
      source.last  := False
    }


    counter := counter + 1
  }

  val sink = Stream Fragment(Bits(128 bits))

  dut.io.sink << source
  sink << dut.io.source
  sink.ready := True
}
