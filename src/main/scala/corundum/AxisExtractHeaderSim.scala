package corundum

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.{ScoreboardInOrder, SimData}

import scala.util.Random

object AxisExtractHeaderSim {
  def main(args: Array[String]) {
    val dataWidth = 32
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val dataWidthBytes = dataWidth/8
    val headerWidthBytes = 3 //Random.nextInt(dataWidth/8)

    SimConfig
    .withFstWave
    .doSim(new AxisExtractHeader(dataWidth/*bits*/, headerWidthBytes)){dut =>

      SimTimeout(1000000 * 10)

      var maxFrameWords = 4
      var maxPacketSizeBytes = maxFrameWords * dataWidthBytes

      dut.io.sink.valid #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      val dataScoreboard = ScoreboardInOrder[BigInt]()

      // monitor output (source) of DUT
      val monitorThread = fork {
        // number of nibbles for printf formatters
        val dw = dataWidth / 4
        val hw = headerWidthBytes * 2
        // keep packet reception state
        var not_last_seen = false
        var in_packet_continuation = false
        var first_beat = false
        var packet_length = 0
        var remaining = 0
        while (true) {
          dut.clockDomain.waitSamplingWhere(dut.io.source.valid.toBoolean & dut.io.source.ready.toBoolean)
          // keep track of packet boundaries and remaining bytes
          first_beat = !in_packet_continuation
          if (first_beat) {
            packet_length = dut.io.source_length.toInt
            remaining = packet_length

            // store DUT output bytes from header into scoreboard
            var ref0 = dut.io.header.toBigInt
            printf(s"HEADER == 0x%0${hw}X %02d bytes\n", ref0, headerWidthBytes)
            for (i <- 0 until headerWidthBytes) {
              dataScoreboard.pushDut(ref0 & 0xff)
              ref0 = ref0 >> 8
            }
          }

          val bytes_in_beat = if (remaining >= dataWidth/8) dataWidth/8 else remaining

          printf(s"DATA == 0x%0${dw}X remaining/total=%02d/%02d B, beat=%02d B %s%s\n",
            dut.io.source.payload.fragment.toBigInt,
            remaining, packet_length, bytes_in_beat,
            if (first_beat) "F" else s" ",
            if (dut.io.source.payload.last.toBoolean) "L" else s" ")
          // store DUT output bytes from payload into scoreboard
          var data1 = dut.io.source.payload.fragment.toBigInt
          for (i <- 0 until bytes_in_beat) {
              dataScoreboard.pushDut(data1 & 0xff)
              data1 = data1 >> 8
          }
          // keep track of packet boundaries and remaining bytes
          in_packet_continuation = !dut.io.source.payload.last.toBoolean
          remaining = if (remaining >= dataWidth/8) remaining - dataWidth/8 else 0
        }
      }


      var data0 = BigInt(0)
      var last0 = false
      var valid0 = false
      var tkeep0 = 0
      var pause = false

      dut.clockDomain.waitSampling()
      val packets = 50000
      // iterate over all frames to generate
      for (packet_idx <- 1 until 50000) {
        // switch between 
        val max_packet_length = if ((packet_idx % 2) == 0) dataWidthBytes else maxPacketSizeBytes
        var packet_length = 1 + Random.nextInt(max_packet_length)

        // special cases, use packets == 1 to limit to only first case

        // packet is exactly header plus one full beat of payload (GitLab issue #1)
        if (packet_idx == 1) packet_length = headerWidthBytes + dataWidthBytes

        if ((packet_idx % 10) == 0) packet_length = headerWidthBytes + dataWidthBytes - 1 + Random.nextInt(4);

        printf("%d: packet_length = %d, max_packet_length = %d\n", packet_idx, packet_length, max_packet_length)
        assert(packet_length <= maxPacketSizeBytes)
        var remaining = packet_length
        var byte_counter = 0

        var clock_counter = 0

        // iterate over frame content
        while (remaining > 0) {
          val tkeep_len = if (remaining >= dataWidthBytes) dataWidthBytes else remaining;
          // simulate source not always valid
          valid0 = !pause

          //println(clock_counter + s" pause " + pause + s", valid " + valid0)
          clock_counter += 1

          // go VALID randomly, but once valid, stay
          if (pause) pause ^= (Random.nextInt(8) > 6)

          assert(tkeep_len <= dataWidthBytes)
          tkeep0 = 0
          data0 = 0
          last0 = false
          if (valid0) {
            last0 = (remaining <= dataWidthBytes)
            for (i <- 0 until tkeep_len) {
              tkeep0 = (tkeep0 << 1) | 1
            }
            // fill data with bytes
            // high nibble is packet index
            // low nibble is byte index (within packet)
            for (i <- 0 until tkeep_len) {
              data0 = (data0 << 8) | ((packet_idx % 16) * 0x10) | ((byte_counter + tkeep_len - i) % 16)
            }
          }

          dut.io.sink.valid #= valid0
          dut.io.sink.last #= last0
          dut.io.sink.payload.fragment #= data0
          dut.io.sink_length #= packet_length

          dut.io.source.ready #= (Random.nextInt(8) > 1) | (packet_idx > 4000)
          //dut.io.source.ready #= true

          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()

          if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
            remaining -= tkeep_len
            byte_counter += tkeep_len

            pause ^= (Random.nextInt(8) > 6)

            // only packets more than header length will pass
            if (packet_length > headerWidthBytes) {
              // keep track of bytes going into the DUT
              for (i <- 0 until tkeep_len) {
                  dataScoreboard.pushRef(data0 & 0xff)
                  data0 = data0 >> 8
              }
            }
          }
        }
        // after each packet, introduce delay for now
        dut.io.sink.valid #= false
        //dut.clockDomain.waitRisingEdge(16)
      }
      dut.io.sink.valid #= false
      while (dut.io.source.valid.toBoolean || dataScoreboard.dut.nonEmpty || dataScoreboard.ref.nonEmpty) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
          dut.io.source.ready #= (Random.nextInt(8) > 1)
      }
      dut.io.source.ready #= false
      dut.clockDomain.waitRisingEdge(8)
      printf("Scoreboard is empty = %b\n", dataScoreboard.checkEmptyness());
    }
  }
}
