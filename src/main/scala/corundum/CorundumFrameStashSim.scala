package corundum

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object CorundumFrameStashSim {
  def main(args: Array[String]) {
    val dataWidth = 24
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    SimConfig.withFstWave.doSim(new CorundumFrameStash(dataWidth)){dut =>

      var maxFrameWords = 8
      var maxPacketSizeBytes = maxFrameWords * keepWidth

      dut.io.slave0.valid #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)


      var data0 = 0

      var last0 = false
      var valid0 = false
      var tkeep0 = 0
      var pause = false

      dut.clockDomain.waitSampling()

      // iterate over all frames to generate
      for (packet_idx <- 0 until 5000) {
        var packet_length = 1 + Random.nextInt(if (packet_idx > 3400) keepWidth else maxPacketSizeBytes)
        //val packet_length = packet_idx match {
        //case (packet_idx > 3400): 1 + Random.nextInt(keepWidth)
        //case (packet_idx > 4800): 0
        //case _: maxPacketSizeBytes
        //}
        assert(packet_length <= maxPacketSizeBytes)
        var remaining = packet_length
        var byte_counter = 0

        // iterate over frame content
        while (remaining > 0) {
          val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
          valid0 = (Random.nextInt(8) > 2) | (packet_idx > 3000)
          valid0 &= pause
          pause ^= (Random.nextInt(16) >= 15)

          assert(tkeep_len <= keepWidth)
          tkeep0 = 0
          data0 = 0
          if (valid0) {
            last0 = (remaining <= keepWidth)
            for (i <- 0 until tkeep_len) {
              tkeep0 = (tkeep0 << 1) | 1
            }
            for (i <- 0 until tkeep_len) {
              data0 = (data0 << 8) | (remaining - i)
            }
          }

          dut.io.slave0.valid #= valid0
          dut.io.slave0.payload.tdata #= data0
          dut.io.slave0.last #= last0
          dut.io.slave0.payload.tkeep #= tkeep0

          dut.io.master0.ready #= (Random.nextInt(8) > 1) | (packet_idx > 4000)

          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()

          if (dut.io.slave0.ready.toBoolean & dut.io.slave0.valid.toBoolean) {
            //byte_counter += tkeep_len
            remaining -= tkeep_len
          }


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
      dut.io.slave0.valid #= false
      while (dut.io.master0.valid.toBoolean) {
          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()
      }
      dut.clockDomain.waitRisingEdge(8)
    }
  }
}
