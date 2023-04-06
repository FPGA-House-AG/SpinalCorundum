package corundum

import spinal.core._
import spinal.lib._

import sourcecode._
import java.io._
import sys.process._


///////////////////////////////DEVELOPMENT//////////////////////////////////////

// companion object for case class
object PreventReplay {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val verilogReport = Config.spinal.generateVerilog({
      val toplevel = new PreventReplay(10, 32, 64)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplay(10, 32, 64)
      toplevel
    })
  }
}

case class PreventReplay(windowSize: Int,
                         sessionIdWidth: Int,
                         counterWidth: Int) extends Component {

  
  val io = new Bundle {
      val drop      = out Bool()
      val sessionId = in  UInt(sessionIdWidth bits)
      val counter   = in  UInt(counterWidth   bits)
  }

  var result = RegInit(False)
  var memory = Mem(Bits(counterWidth * windowSize bits), 1024)
  memory.addAttribute("ram_style", "block") 


  var data = memory.readSync(address = io.sessionId.resize(10 bits), 
                             enable  = True)
  
  for (j <- 0 until windowSize) {

    when(data(j * counterWidth, counterWidth bits) === io.counter.asBits) {
      
      //var shiftedData = data(0, j * counterWidth bits) ## io.counter ## data.take((windowSize - j - 1) * counterWidth)//data(j * counterWidth, (counterWidth - j - 1) * windowSize bits) 
      result := True
    }.elsewhen(Bool(j == windowSize - 1) && data(j * counterWidth, counterWidth bits) =/= io.counter.asBits) {

      var shiftedData = io.counter ## data(0, counterWidth * windowSize - counterWidth bits)
      memory.write(address = io.sessionId.resize(10 bits), 
                   data    = shiftedData,
                   enable  = True)

      result := False
    }
  }

  io.drop := result

}




///////////////////////////////SIMULATION//////////////////////////////////////
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object PreventReplaySim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplay(10, 32, 64)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge()
      dut.io.sessionId      #= 0
      dut.io.counter        #= 0
      dut.clockDomain.waitRisingEdge()
      
      dut.clockDomain.waitRisingEdge(10)
      //Wait for some cycles
      for(i <-0 until 20){
        dut.io.sessionId    #= 1
        dut.io.counter      #= i
        //Wait for some cycles
        dut.clockDomain.waitRisingEdge(5)
      }
      
      

    }
  }
}