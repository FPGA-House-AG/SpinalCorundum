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

      var maxPacketSizeBytes = keepWidth * 8

      dut.io.slave0.valid #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)


      var data0 = 0

      var last0 = false
      var valid0 = false
      var tkeep0 = 0

      dut.clockDomain.waitSampling()

      // iterate over all frames to generate
      for (packet_idx <- 0 until 500) {
        val packet_length = Random.nextInt(maxPacketSizeBytes)
        var remaining = packet_length
        var byte_counter = 0
        // iterate over frame content
        while (remaining > 0) {
          val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
          // active beat, or slave was not active yet?
          //if ((dut.io.slave0.ready.toBoolean & dut.io.slave0.valid.toBoolean) || !valid0) {
            valid0 = (Random.nextInt(8) > 2) | (packet_idx > 300)
          //}
          tkeep0 = 0
          data0 = 0
          if (valid0) {
            last0 = (remaining <= keepWidth)
            for (i <- 0 until tkeep_len) {
              tkeep0 = (tkeep0 << 1) | 1
            }
            for (i <- 0 until tkeep_len) {
              data0 = (data0 << 8) | byte_counter
              byte_counter += 1
              
            }
            remaining -= tkeep_len
          }
          //if (dut.io.slave0.ready.toBoolean & dut.io.slave0.valid.toBoolean) data0 += 1
          //if (dut.io.slave0.ready.toBoolean & dut.io.slave0.valid.toBoolean & dut.io.slave0.last.toBoolean) data0 = 0


          dut.io.slave0.valid #= valid0
          dut.io.slave0.payload.tdata #= data0
          dut.io.slave0.last #= last0
          dut.io.slave0.payload.tkeep #= tkeep0

          dut.io.master0.ready #= (Random.nextInt(8) > 1) | (packet_idx > 400)

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
}
