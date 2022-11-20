package corundum


import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._
import scala.util.Random

object CorundumFrameMatchHeaderSim {
  def main(args: Array[String]) {
    val dataWidth = 24
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8

    printf("keepWidth=%d\n", keepWidth)

    SimConfig
    .withFstWave
    .doSim(new CorundumFrameMatchHeader(dataWidth)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      var data0 = 0

      var first0 = true
      var last0 = false
      var valid0 = false
      var tkeep0 = 0

      dut.io.matchMask   #= 0xF00000
      dut.io.matchHeader #= 0x800000

      for (idx <- 0 to 499){

        // active beat
        if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) data0 += 1
        // active beat, or slave was not active yet?
        if ((dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) || !valid0) {
          valid0 = (Random.nextInt(8) > 6) | (idx > 300)
          last0 = (Random.nextInt(8) >= 4) & valid0 
        }
        tkeep0 = 0
        if (valid0) {
          var tkeep_len = if (!last0) keepWidth else 1 + Random.nextInt(keepWidth-1);
          for (i <- 0 until tkeep_len) {
            tkeep0 = (tkeep0 << 1) | 1
          }
        }
        if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean & dut.io.sink.last.toBoolean) {
          data0 = Random.nextInt(maxDataValue)
        }
        data0 &= scala.math.pow(2, dataWidth).intValue - 1

        dut.io.sink.valid #= valid0
        dut.io.sink.payload.tdata #= data0
        dut.io.sink.last #= last0
        dut.io.sink.payload.tkeep #= tkeep0

        dut.io.source.ready #= (Random.nextInt(8) > 1)

        //Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        //Check that the dut values match with the reference model ones
        //val modelFlag = modelState == 0 || dut.io.cond1.toBoolean
        //assert(dut.io.state.toInt == modelState)
        //assert(dut.io.flag.toBoolean == modelFlag)

        //Update the reference model value
        //if(dut.io.cond0.toBoolean) {
        //  modelState = (modelState + 1) & 0xFF
        //}
      }
    }
  }
}


object CorundumFrameMatchHeaderAxi4Sim {

  def main(args: Array[String]) {
    val dataWidth = 64
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth / 8

    printf("keepWidth=%d\n", keepWidth)

    var compiled = SimConfig
      .withFstWave
      .compile(new CorundumFrameMatchHeaderAxi4(dataWidth, Axi4Config(32, 32, 2, useQos = false, useRegion = false)))

    compiled.doSim { dut =>

      // 4 bits per printf hex nibble
      val dw = 32 / 4
      // one keep bit per byte, 4 bits per printf hex nibble
      val kw = 32 / 8 / 4

      // all our writes are single-beats
      dut.io.ctrlbus.w.last #= true
      dut.io.ctrlbus.r.ready #= false
      dut.io.ctrlbus.b.ready #= true
      dut.io.ctrlbus.ar.valid #= false
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      dut.io.ctrlbus.aw.payload.id.assignBigInt(0)
      dut.io.ctrlbus.aw.payload.lock.assignBigInt(0) // normal
      dut.io.ctrlbus.aw.payload.prot.assignBigInt(2) // normal non-secure data access
      dut.io.ctrlbus.aw.payload.burst.assignBigInt(1) // fixed address burst
      dut.io.ctrlbus.aw.payload.len.assignBigInt(0) // 1 beat per burst
      dut.io.ctrlbus.aw.payload.size.assignBigInt(2) // 4 bytes per beat

      dut.io.ctrlbus.w.payload.strb.assignBigInt(0xF) // 4 bytes active per beat

      dut.io.ctrlbus.ar.payload.id.assignBigInt(0)
      dut.io.ctrlbus.ar.payload.lock.assignBigInt(0) // normal
      dut.io.ctrlbus.ar.payload.prot.assignBigInt(2) // normal non-secure data access
      dut.io.ctrlbus.ar.payload.burst.assignBigInt(1) // fixed address burst
      dut.io.ctrlbus.ar.payload.len.assignBigInt(0) // 1 beat per burst
      dut.io.ctrlbus.ar.payload.size.assignBigInt(2) // 4 bytes per beat

      dut.io.sink.valid #= false
      dut.io.source.ready #= false


      //Create a new thread
      val myNewThread = fork {
        val dw = dataWidth / 4
        var in_packet_continuation = false
        var first_beat = false

        

        while (true) {
          dut.clockDomain.waitSamplingWhere(dut.io.source.valid.toBoolean & dut.io.source.ready.toBoolean)
          first_beat = !in_packet_continuation
          printf(s"DATA == 0x%0${dw}X %s%s\n", dut.io.source.payload.fragment.tdata.toBigInt,
            if (first_beat) "*" else s" ",
            if (dut.io.source.payload.last.toBoolean) "L" else s" ")
          in_packet_continuation = !dut.io.source.payload.last.toBoolean
        }
      }


      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge()
      dut.io.source.ready #= true

      // keep match
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x40) // driveFrom() stream 
      dut.io.ctrlbus.w.valid #= true
      // If it is bigger than a long, you can use "FFFFFF33".asHex
      dut.io.ctrlbus.w.payload.data.assignBigInt(0x00112277L)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      // keep match
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x44) // driveFrom() stream 
      dut.io.ctrlbus.w.valid #= true
      // If it is bigger than a long, you can use "FFFFFF33".asHex
      dut.io.ctrlbus.w.payload.data.assignBigInt(0x77665544L)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      // keep mask
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x80) // driveFrom() stream 
      dut.io.ctrlbus.w.valid #= true
      dut.io.ctrlbus.w.payload.data.assignBigInt(0x000000FFL)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      // drop mask
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x100)
      dut.io.ctrlbus.w.valid #= true
      dut.io.ctrlbus.w.payload.data.assignBigInt(0xFFFFFFFFL)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x104)
      dut.io.ctrlbus.w.valid #= true
      dut.io.ctrlbus.w.payload.data.assignBigInt(0xFFFFFFFFL)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false


      // push one word into FIFO
      dut.clockDomain.waitSamplingWhere(dut.io.sink.ready.toBoolean)
      dut.io.sink.payload.tdata.assignBigInt(0x0011223344556677L)
      dut.io.sink.payload.tkeep.assignBigInt((1 << keepWidth) - 1)
      dut.io.sink.payload.tuser.assignBigInt(0)
      dut.io.sink.valid #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.sink.valid #= false

      dut.io.ctrlbus.r.ready #= false
      // assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= true
      dut.io.ctrlbus.ar.payload.addr.assignBigInt(0x08) // read GIT build number 
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.ar.ready.toBoolean)
      // de-assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= false
      // accept read data on R channel
      dut.io.ctrlbus.r.ready #= true
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.r.valid.toBoolean)
      // no longer accept read data on R channel
      dut.io.ctrlbus.r.ready #= false
      printf(s"*TDATA == 0x%0${dw}X\n", dut.io.ctrlbus.r.payload.data.toBigInt)

      //Wait for a rising edge on the clock
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      
      dut.io.ctrlbus.r.ready #= false
      // assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= true
      dut.io.ctrlbus.ar.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      // wait for active beat on AR channel to take our read command
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.ar.ready.toBoolean)
      // de-assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= false
      // accept read data on R channel
      dut.io.ctrlbus.r.ready #= true
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.r.valid.toBoolean)
      // no longer accept read data on R channel
      dut.io.ctrlbus.r.ready #= false

      printf(s"*TDATA == 0x%0${dw}X\n", dut.io.ctrlbus.r.payload.data.toBigInt)
      //printf(s"*TKEEP == 0x%0${kw}X\n", dut.io.master0.payload.tkeep.toBigInt)

      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
    }
  }
}

object CorundumFrameMatchWireguardSim {
  def main(args: Array[String]) {
    val dataWidth = 512
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    SimConfig
    .withFstWave
    .doSim(CorundumFrameMatchWireguard()){dut =>

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
      while (dut.io.source.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge(8)
      dut.clockDomain.waitRisingEdge(8)
    }
  }
}

object CorundumFrameDemuxWireguardSim {
  def main(args: Array[String]) {
    val dataWidth = 512
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    SimConfig
    .withFstWave
    .doSim(new CorundumFrameDemuxWireguard()){dut =>

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

      // @TODO add Poly1305 tag
      val plaintext = Vector(
        Vector(
          //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------>
          BigInt("01 02 03 04 05 06 01 02 03 04 05 06 01 02 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16),
          BigInt("04 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63".split(" ").reverse.mkString(""), 16),
          BigInt("6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f 6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69".split(" ").reverse.mkString(""), 16),
          BigInt("74 2e 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        ),
        Vector(
          //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <----Wireguard Type 4 ------------------------>
          BigInt("01 02 03 04 05 06 01 02 03 04 05 06 01 02 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 02 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16),
          BigInt("04 00 00 00 11 22 33 44 c1 c2 c3 c4 c5 c6 c7 c8 4c 61 64 69 65 73 20 61 6e 64 20 47 65 6e 74 6c 65 6d 65 6e 20 6f 66 20 74 68 65 20 63 6c 61 73 73 20 6f 66 20 27 39 39 3a 20 49 66 20 49 20 63".split(" ").reverse.mkString(""), 16),
          BigInt("6f 75 6c 64 20 6f 66 66 65 72 20 79 6f 75 20 6f 6e 6c 79 20 6f 6e 65 20 74 69 70 20 66 6f 72 20 74 68 65 20 66 75 74 75 72 65 2c 20 73 75 6e 73 63 72 65 65 6e 20 77 6f 75 6c 64 20 62 65 20 69".split(" ").reverse.mkString(""), 16),
          BigInt("74 2e 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        )
      )
      var packet = 0
      while (packet < 2) {

      var packet_length = 192 + 2 // bytes
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
        dut.io.sink.payload.tdata #= plaintext(packet)(word_index)
        dut.io.sink.last #= last0
        dut.io.sink.payload.tkeep #= tkeep0

        dut.io.source_other.ready #= (Random.nextInt(8) > 1)
        dut.io.source_type4.ready #= (Random.nextInt(8) > 1)

        // Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
          remaining -= tkeep_len
          word_index += 1
        }
      }
      packet += 1
      }
      // flush
      dut.io.sink.valid #= false
      dut.io.source_other.ready #= true
      dut.io.source_type4.ready #= true
      while (dut.io.source_other.valid.toBoolean | dut.io.source_type4.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge(8)
      dut.clockDomain.waitRisingEdge(8)
    }
  }
}