package corundum

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.sim.{ScoreboardInOrder, SimData}

import scala.util.Random

object AxisInsertHeaderSim {
  def main(args: Array[String]) {
    val dataWidth = 64
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    val headerWidthBytes = Random.nextInt(dataWidth/8)
    SimConfig
    .withFstWave
    .doSim(new AxisInsertHeader(dataWidth/*bits*/, headerWidthBytes)){dut =>

      SimTimeout(100000 * 10)

      var maxFrameWords = 34
      var maxPacketSizeBytes = (maxFrameWords + 2) * keepWidth + keepWidth - 1


      dut.io.sink.valid #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      val dataScoreboard = ScoreboardInOrder[BigInt]()

      val monitorThread = fork {
        val dw = dataWidth / 4
        var not_last_seen = false
        var in_packet_continuation = false
        var first_beat = false
        var packet_length = 0
        var remaining = 0
        while (true) {
          dut.clockDomain.waitSamplingWhere(dut.io.source.valid.toBoolean & dut.io.source.ready.toBoolean)
          first_beat = !in_packet_continuation
          if (first_beat) {
            packet_length = dut.io.source_length.toInt
            remaining = dut.io.source_length.toInt
          }
          val bytes_in_beat = if (remaining >= dataWidth/8) dataWidth/8 else remaining
          
          printf(s"DATA == 0x%0${dw}X %02d/%02d %02d bytes %s%s\n", dut.io.source.payload.fragment.toBigInt,
            remaining, packet_length, bytes_in_beat,
            if (first_beat) "*" else s" ",
            if (dut.io.source.payload.last.toBoolean) "L" else s" ")
          var data1 = dut.io.source.payload.fragment.toBigInt
          for (i <- 0 until bytes_in_beat) {
              dataScoreboard.pushDut(data1 & 0xff)
              data1 = data1 >> 8
          }
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

      // iterate over all frames to generate
      for (packet_idx <- 1 until maxPacketSizeBytes) {
        var packet_length = packet_idx //1 + Random.nextInt(if (packet_idx > 3400) keepWidth else maxPacketSizeBytes)
        assert(packet_length <= maxPacketSizeBytes)
        var remaining = packet_length
        var byte_counter = 0

        var clock_counter = 0
        var new_byte = new Array[Int](dataWidth/8)

        var header0 = BigInt(0)
        for (i <- 0 until headerWidthBytes) {
          header0 = (header0 << 8) | (0xA0 | i)
        }
        dut.io.header #= header0
        // keep track of bytes going into the DUT
        var ref0 = header0
        for (i <- 0 until headerWidthBytes) {
          dataScoreboard.pushRef(ref0 & 0xff)
          ref0 = ref0 >> 8
        }

        // iterate over frame content
        while (remaining > 0) {
          val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
          // simulate source not always valid
          val go_valid = true //(Random.nextInt(8) > 2) | (packet_idx > 3000)
          if (!valid0 & go_valid) {
            valid0 = true
          }

          clock_counter += 1

          if (pause) pause ^= (Random.nextInt(16) >= 15)
          else if (!pause) pause ^= (Random.nextInt(128) >= 127)
          // limit to single beat activities
          //pause = false

          assert(tkeep_len <= keepWidth)
          tkeep0 = 0
          data0 = 0
          last0 = false
          if (valid0) {
            last0 = (remaining <= keepWidth)
            for (i <- 0 until tkeep_len) {
              tkeep0 = (tkeep0 << 1) | 1
            }
            // fill data with bytes
            // high nibble is packet index
            // low nibble is byte index (within packet)
            for (i <- 0 until tkeep_len) {
              new_byte(i) = ((packet_idx % 16) * 0x10) | ((byte_counter + tkeep_len - i) % 16)
              data0 = (data0 << 8) | new_byte(i)
            }
          }

          dut.io.sink.valid #= valid0
          dut.io.sink.last #= last0
          dut.io.sink.payload.fragment #= data0
          dut.io.sink_length #= packet_length

          dut.io.source.ready #= (Random.nextInt(8) > 1) | (packet_idx > 4000)
          dut.io.source.ready #= true

          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()

          if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
            remaining -= tkeep_len
            byte_counter += tkeep_len
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
      dut.io.source.ready #= true
      dut.io.sink.valid #= false
      while (dut.io.source.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge(8)
    }
  }
}
