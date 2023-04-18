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
      val toplevel = new PreventReplay(64, 10, 64, 1024)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplay(64, 10, 64, 1024)
      toplevel
    })
  }
}

case class PreventReplay(windowSize:        Int,
                         sessionIdWidth:    Int,
                         counterWidth:      Int,
                         numberOfSessions:  Int         
                         ) extends Component {

  // This class is our per-session state. 
  case class ReceiveWindow(windowSize : Int, counterWidth : Int) extends Bundle {
    var wt          = UInt(counterWidth bits)
    var bitmap      = Bits(windowSize bits) 
  }

  val io = new Bundle {
      val drop      = out Bool()
      val sessionId = in  UInt(sessionIdWidth bits)
      val counter   = in  UInt(counterWidth   bits)
      val read_data = out Bits(windowSize     bits)
      val read      = ReadBundle(log2Up(numberOfSessions), counterWidth)
  }
  
  var state  = ReceiveWindow(windowSize, counterWidth)
  var result = RegInit(False)
  var memory = Mem(Bits(state.asBits.getWidth bits), numberOfSessions)


  io.read_data := RegNext(memory.readSync(
    enable  = io.read.enable,
    address = io.read.addr
  )(counterWidth-1 downto 0))
    
  io.drop      := False

}




///////////////////////////////SIMULATION//////////////////////////////////////
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object PreventReplaySim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplay(64, 10, 64, 1024)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge(20)
      dut.io.sessionId.assignBigInt(1)
      dut.io.counter.assignBigInt(1)
      dut.clockDomain.waitRisingEdge(20)
    }
  }
}