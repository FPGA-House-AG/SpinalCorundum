package corundum

import spinal.core._
import spinal.lib._

import sourcecode._
import java.io._
import sys.process._

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
      val drop      = out Bits(1 bits)
      val sessionId = in  Bits(sessionIdWidth bits)
      val counter   = in  Bits(counterWidth   bits)
  }

  val memory = Mem(Bits(counterWidth * windowSize bits), 1024)
  // UltraRAM (URAM) cannot be initialized
  memory.addAttribute("ram_style", "ultra")



  val result = RegInit(False)
  result := False
  
  for (i <- 0 until 1024) {
    
    val data = memory.readSync(address = IntToUInt(i).resize(10 bits), 
                               enable  = True)
    
    for (j <- 0 until windowSize) {
      
      when(data(j * counterWidth, counterWidth bits) === io.counter) {
        result := True
      }
    }
  }

  io.drop := result.asBits
}