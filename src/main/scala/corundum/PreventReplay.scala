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
      val read      = ReadBundle(log2Up(1024),  counterWidth * windowSize)
      val write     = WriteBundle(log2Up(1024), counterWidth * windowSize)
      val drop      = out Bool()
      val sessionId = in  UInt(sessionIdWidth bits)
      val counter   = in  UInt(counterWidth   bits)
  }

  val memory = Mem(Bits(counterWidth * windowSize bits), 1024)
  // UltraRAM (URAM) cannot be initialized
  memory.addAttribute("ram_style", "ultra")



  val result = RegInit(False)

  val data = memory.readSync(address = io.sessionId.resize(10 bits), 
                             enable  = True)
  
  for (j <- 0 until windowSize) {
    when(data(j * counterWidth, counterWidth bits) === io.counter.asBits) {
      result := True
    }
  }

  if(result == False) {
 
    val shiftedData = io.counter ## data.resize(data.getWidth - counterWidth)
    memory.write(address = io.sessionId.resize(10 bits), 
                 data    = shiftedData,
                 enable  = True)

    io.drop := False
  }else {
    io.drop := True
  }

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

      for (i <- 0 until 1024) {
        dut.io.write.enable  #= true
        dut.io.write.addr.assignBigInt(i)
        dut.io.write.data.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
        dut.io.write.enable  #= false
      }

      //Wait for some cycles
      dut.clockDomain.waitRisingEdge(10)


      // Read back the memory and check if it matches
      //for (i <- 0 until 1024) {
      //  val data = dut.memory.readSync(address = IntToUInt(i).resize(10 bits), 
      //                                 enable  = True)
        //assert(data == initData(i))
      //}


    }
  }
}