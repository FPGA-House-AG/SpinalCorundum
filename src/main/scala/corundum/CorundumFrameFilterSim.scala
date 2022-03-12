package corundum

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object CorundumFrameFilterSim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new CorundumFrameFilter(8)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      var data0 = 0

      var first0 = true
      var last0 = false
      var valid0 = false
      var tkeep0 = 0

      dut.io.mask #= 0x0F
      dut.io.filter #= 0x03

      for (idx <- 0 to 499){

        // active beat
        if (dut.io.slave0.ready.toBoolean & dut.io.slave0.valid.toBoolean) data0 += 1
        // active beat, or slave was not active yet?
        if ((dut.io.slave0.ready.toBoolean & dut.io.slave0.valid.toBoolean) || !valid0) {
          valid0 = (Random.nextInt(8) > 6) | (idx > 300)
          last0 = (Random.nextInt(8) == 7) & valid0
        }
        tkeep0 = 0
        if (valid0) tkeep0 = 1

        if (dut.io.slave0.ready.toBoolean & dut.io.slave0.valid.toBoolean & dut.io.slave0.last.toBoolean) {
          data0 = Random.nextInt(8)
        }
        dut.io.slave0.valid #= valid0
        dut.io.slave0.payload.tdata #= data0
        dut.io.slave0.last #= last0
        dut.io.slave0.payload.tkeep #= tkeep0

        dut.io.master0.ready #= (Random.nextInt(8) > 1)

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
