package corundum

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random

//MyTopLevel's testbench
object MyTopLevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new MyTopLevel){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      var data0 = 0
      var data1 = 0

      for(idx <- 0 to 99){
        //Drive the dut inputs with random values
        val valid0 = Random.nextBoolean()
        val last0 = valid0 & (Random.nextInt(8) == 7)
        dut.io.slave0.valid #= valid0
        dut.io.slave0.payload.tdata #= data0
        dut.io.slave0.last #= last0
        if (valid0) data0 = data0 + 1
        if (last0) data0 = 0

        val valid1 = Random.nextBoolean()
        val last1 = valid1 & (Random.nextInt(8) == 7)
        dut.io.slave1.valid #= valid1
        dut.io.slave1.payload.tdata #= 16 + data1
        dut.io.slave1.last #= last1
        if (valid1) data1 = data1 + 1
        if (last1) data1 = 0

        dut.io.master0.ready #= true

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
