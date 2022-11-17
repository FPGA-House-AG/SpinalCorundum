package corundum

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object AxisToCorundumFrameSim {
  def main(args: Array[String]) {
    val dataWidth = 32
    val keepWidth = dataWidth/8
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1

    SimConfig
    .withFstWave
    .doSim(new AxisToCorundumFrame(dataWidth)){dut =>

      SimTimeout(100000 * 10)

      var maxFrameWords = 16
      var maxPacketSizeBytes = (maxFrameWords + 2) * keepWidth + keepWidth - 1

      dut.io.sink.valid #= false

      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      //Create a new thread
      val myNewThread = fork {
        // calculate printf formatter lengths
        val dw = (dataWidth/4).max(1)
        val tw = (keepWidth/4).max(1)
        var not_last_seen = false
        var in_packet_continuation = false
        var first_beat = false
        var packet_length = 0
        var remaining = 0
        while (true) {
          // wait for beat on DUT source
          dut.clockDomain.waitSamplingWhere(dut.io.source.valid.toBoolean & dut.io.source.ready.toBoolean)
          first_beat = !in_packet_continuation
          printf(s"DATA == 0x%0${dw}X 0x%0${tw}X %s%s\n",
            dut.io.source.payload.fragment.tdata.toBigInt,
            dut.io.source.payload.fragment.tkeep.toBigInt,
            if (first_beat) "*" else s" ",
            if (dut.io.source.payload.last.toBoolean) "L" else s" ")
          in_packet_continuation = !dut.io.source.payload.last.toBoolean
        }
      }

      val dwI = dataWidth / 4/*nibble*/

      var data0 = BigInt(0)
      var last0 = false
      var valid0 = false
      var tkeep0 = 0
      var pause = false

      dut.clockDomain.waitSampling()

      var packet_idx = 1
      // iterate over all frames to generate
      while (packet_idx < 31) {
        var packet_length = packet_idx //1 + Random.nextInt(if (packet_idx > 3400) keepWidth else maxPacketSizeBytes)
        //val packet_length = packet_idx match {
        //case (packet_idx > 3400): 1 + Random.nextInt(keepWidth)
        //case (packet_idx > 4800): 0
        //case _: maxPacketSizeBytes
        //}
        assert(packet_length <= maxPacketSizeBytes)
        var remaining = packet_length
        var byte_counter = 0

        var clock_counter = 0
        // iterate over frame content
        while (remaining > 0) {
          val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
          // simulate source not always valid
          valid0 = (Random.nextInt(8) > 2) | (packet_idx > 3000)
          valid0 &= !pause
          valid0 = true

          //println(clock_counter + s" pause " + pause + s", valid " + valid0)
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
              data0 = (data0 << 8) | ((packet_idx % 16) * 0x10) | ((byte_counter + tkeep_len - i) % 16)
            }
          }

          dut.io.sink.valid #= valid0
          dut.io.sink.last #= last0
          dut.io.sink.payload.fragment #= data0
          dut.io.sink_length #= packet_length

          dut.io.source.ready #= (Random.nextInt(8) > 1) | (packet_idx > 4000)
          dut.io.source.ready #= true

          // Wait for falling edge on the clock
          dut.clockDomain.waitFallingEdge()

          printf(s"SINK == 0x%0${dwI}X, %s %s %s remaining=%d\n",
            dut.io.sink.payload.fragment.toBigInt,
            if (dut.io.sink.ready.toBoolean) "READY" else "",
            if (dut.io.sink.valid.toBoolean) "VALID" else "",
            if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) "FIRE" else "",
            remaining)

          if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
            byte_counter += tkeep_len

            remaining -= tkeep_len
            if (remaining <= 0) {
              packet_idx += 1
            }
          }

          // Wait for rising edge on the clock
          dut.clockDomain.waitRisingEdge()

        }
        // after each packet, introduce delay for now
        dut.io.sink.valid #= false
        //dut.clockDomain.waitRisingEdge(16)
      }
      dut.io.sink.valid #= false
      dut.io.sink.ready #= true
      while (dut.io.source.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge(8)
    }
  }
}
