package corundum

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.{ScoreboardInOrder, SimData}

import scala.util.Random

object StreamLatencySim {
  def main(args: Array[String]) : Unit = {
    // component parameters
    val dataWidth = 32
    val cycleCount = 20
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val dataWidthBytes = dataWidth/8

    SimConfig
    .withFstWave
    .doSim(new StreamLatency(Fragment(Bits(dataWidth bits)), cycleCount)){dut =>

      SimTimeout(1000000 * 10)

      var maxFrameWords = 32
      var maxPacketSizeBytes = maxFrameWords * dataWidthBytes

      dut.io.sink.valid #= false
      dut.io.source.ready #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      val dataScoreboard = ScoreboardInOrder[BigInt]()

      // monitor output (source) of DUT
      val monitorThread = fork {
        // number of nibbles for printf formatters
        val dw = dataWidth / 4
        // keep packet reception state
        var not_last_seen = false
        var in_packet_continuation = false
        var first_beat = false
        while (true) {
          dut.clockDomain.waitSamplingWhere(dut.io.source.valid.toBoolean & dut.io.source.ready.toBoolean)
          // keep track of packet boundaries and remaining bytes
          first_beat = !in_packet_continuation

          printf(s"DATA == 0x%0${dw}X %s%s\n",
            dut.io.source.payload.fragment.toBigInt,
            if (first_beat) "F" else s" ",
            if (dut.io.source.payload.last.toBoolean) "L" else s" ")
          // store DUT output bytes from payload into scoreboard
          var data1 = dut.io.source.payload.fragment.toBigInt
          for (i <- 0 until dataWidthBytes) {
              dataScoreboard.pushDut(data1 & 0xff)
              data1 = data1 >> 8
          }
          // keep track of packet boundaries and remaining bytes
          in_packet_continuation = !dut.io.source.payload.last.toBoolean
        }
      }

      var data0 = BigInt(0)
      var last0 = false
      var valid0 = false
      var tkeep0 = 0
      var pause = false

      dut.clockDomain.waitSampling()
      val packets = 50000
      var clock_counter = 0

      // iterate over all frames to generate
      for (packet_idx <- 1 to packets) {
        // switch between 
        val max_packet_beats = 9
        var packet_length = dataWidthBytes * Random.nextInt(max_packet_beats)

        printf("%d: packet_length = %d\n", packet_idx, packet_length)
        assert(packet_length <= maxPacketSizeBytes)
        var remaining = packet_length
        var byte_counter = 0

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

          //dut.io.source.ready #= (Random.nextInt(8) > 1) | (packet_idx > 4000)
          // no backpressure in cycles 40-80
          if (clock_counter < 80) {
            dut.io.source.ready #= true
          }
          // full backpressure tot cycle 40
          if (clock_counter < 40) {
            dut.io.source.ready #= false
          }

          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()

          if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
            remaining -= tkeep_len
            byte_counter += tkeep_len

            pause ^= (Random.nextInt(8) > 6)

            // keep track of bytes going into the DUT
            for (i <- 0 until tkeep_len) {
                dataScoreboard.pushRef(data0 & 0xff)
                data0 = data0 >> 8
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
