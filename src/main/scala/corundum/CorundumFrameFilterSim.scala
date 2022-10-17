package corundum

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

object CorundumFrameFilterSim {
  def main(args: Array[String]) {
    val dataWidth = 24
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8

    printf("keepWidth=%d\n", keepWidth)

    SimConfig.withFstWave.doSim(new CorundumFrameFilter(dataWidth)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      var data0 = 0

      var first0 = true
      var last0 = false
      var valid0 = false
      var tkeep0 = 0

      dut.io.keepMask #= 0x800000
      dut.io.keepFilter #= 0x800000

      dut.io.dropMask #= 0x000100
      dut.io.dropFilter #= 0x000100


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
