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
      val toplevel = new PreventReplay(32, 32, 64)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplay(32, 32, 64)
      toplevel
    })
  }
}

case class PreventReplay(windowSize:        Int,
                         sessionIdWidth:    Int,
                         counterWidth:      Int           
                         ) extends Component {

  
  class ReceiveWindow(windowSize : Int, sessionIdWidth : Int) extends Bundle {
    var wt          = UInt(sessionIdWidth bits)
    var bitmap      = Bits(windowSize bits)
  }

  val io = new Bundle {
      val drop      = out Bool()
      val sessionId = in  UInt(sessionIdWidth bits)
      val counter   = in  UInt(counterWidth   bits)
  }
  
  var state  = new ReceiveWindow(windowSize, sessionIdWidth)
  var result = RegInit(False)
  var memory = Mem(Bits(windowSize bits), 1024)

  io.drop := False

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


    }
  }
}