package blackwire

import spinal.core._
import spinal.lib._

import scala.math._

import corundum._


// companion object
object BlackwireWireguardType4 {
  def main(args: Array[String]) {
    SpinalVhdl(new BlackwireWireguardType4())
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
    val key = out Bits(256 bits)
    val latency = out UInt(8 bits)
  }

  val reverse = CorundumFrameEndianess(corundumDataWidth)
  reverse.io.sink << io.sink
  
  val stash = CorundumFrameStash(corundumDataWidth, 32)
  stash.io.sink << reverse.io.source

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

  // KeyLookup now adapts for endianess of ChaCha
  //val endianess = AxisEndianess(cryptoDataWidth)
  //endianess.io.sink << rxkey.io.source
  io.source << rxkey.io.source //endianess.io.source
  io.key := rxkey.io.key_out

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

  io.latency := U(LatencyAnalysis(io.sink.valid, io.source.valid))

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
    .withFstWave
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

      // 000  4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c  Ladies and Gentl
      // 016  65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73  emen of the clas
      // 032  73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63  s of '99: If I c
      // 048  6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f  ould offer you o
      // 064  6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20  nly one tip for
      // 080  74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73  the future, suns
      // 096  63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69  creen would be i
      // 112  74 2e                                            t.

      // @TODO add Poly1305 tag
      //val plaintext = Vector(
      //  BigInt("04 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63".split(" ").reverse.mkString(""), 16),
      //  BigInt("6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f 6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69".split(" ").reverse.mkString(""), 16),
      //  BigInt("74 2e 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
      //)

      // reversed byte order of TKEEP
      val plaintext = Vector(
        BigInt("04 00 00 90 00 00 00 01 00 01 02 03 04 05 06 07 4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63".split(" ")/*.reverse*/.mkString(""), 16),
        BigInt("6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f 6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69".split(" ")/*.reverse*/.mkString(""), 16),
        BigInt("74 2e 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ")/*.reverse*/.mkString(""), 16)
      )

      // 512/8
      var packet_length = 16 + 114 + 16 // bytes
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
          var tkeep1 = tkeep0
          tkeep0 = 0
          for (i <- 0 until keepWidth) {
            tkeep0 = (tkeep0 << 1) | (tkeep1 & 1)
            tkeep1 = tkeep1 >> 1
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
      dut.io.sink.payload.tdata #= BigInt(0)
      dut.io.source.ready #= true
      
      printf("LATENCY=%d\n", dut.io.latency.toInt)

      dut.clockDomain.waitRisingEdge(dut.io.latency.toInt + 32)

      printf("VALID=%d, LAST=%d", dut.io.source.valid.toBigInt, dut.io.source.last.toBigInt)
      while (dut.io.source.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
          printf("VALID=%d, LAST=%d", dut.io.source.valid.toBigInt, dut.io.source.last.toBigInt)
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
      source.tdata := B"512'h040000900000000100010203040506074c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a20496620492063"
      source.tkeep := B"64'hFFFFFFFFFFFFFFFF"
      source.valid := True
    }
    .elsewhen (counter === 1) {
      source.tdata := B"512'h6f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069"
      source.tkeep := B"64'hFFFFFFFFFFFFFFFF"
    }.elsewhen (counter === 2) {
      source.tdata := B"512'h742e000102030405060708090a0b0c0d0e0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      //source.tkeep := B"64'hFFFFc00000000000"
      source.tkeep := B"64'hFFFFc00000000000"
      source.last  := True
    }
    counter := counter + 1
  }

  val sink = Stream Fragment(Bits(128 bits))

  dut.io.sink << source
  sink << dut.io.source
  sink.ready := True
}
