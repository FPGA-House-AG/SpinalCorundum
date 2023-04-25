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
      val toplevel = new PreventReplay(10, 10, 64, 1024)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplay(10, 10, 64, 1024)
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
  memory.initBigInt(Seq.fill(numberOfSessions)(0))
  memory.addAttribute("ram_style", "block") 
  
  var data = memory.readSync(address = io.sessionId.resize(log2Up(numberOfSessions) bits), 
                             enable  = True)
  
  state.wt     := data(windowSize, counterWidth bits).asUInt
  state.bitmap := data(0, windowSize bits)

  var s_val     = io.counter
  var wb     = s_val-windowSize+1

  var wt_ptr = s_val % windowSize
  var wb_ptr = wb % windowSize



  when(s_val === U(0, counterWidth bits)){
    // Start of operation or wrapped
    io.drop := False
  }.elsewhen(s_val > state.wt){
    // New packet, slide the window
    var diff = s_val - state.wt - 1

    for(i <- 0 until windowSize){
      when((i > state.wt+1) & (i < s_val)){
        state.bitmap(i % windowSize)     := False
      }.elsewhen(i === state.wt+1){
        state.bitmap(i % windowSize)     := True
      }
    }
    
    data = s_val.asBits ## state.bitmap
    memory(io.sessionId.resize(log2Up(numberOfSessions) bits)) := data

    io.drop := False

  }.elsewhen(s_val + U(windowSize, counterWidth bits) <= state.wt){
    // Too old packet
    io.drop := True
  }.otherwise{
    
    val conditionalReg = Reg(Bool())
    conditionalReg := state.bitmap(wt_ptr.resize(log2Up(windowSize) bits))
    // S inside window, check the memory
    when(conditionalReg === False){
      state.bitmap(wt_ptr.resize(log2Up(windowSize) bits)) := True 
      data = s_val.asBits ## state.bitmap
      memory(io.sessionId.resize(log2Up(numberOfSessions) bits)) := data
      io.drop := False
      //We haven't seen this packet yet. We set the bit in memory, and don't update the window
    }.otherwise{
      //We've seen this packet already, we drop it, and we don't update the window.
      io.drop := True
    }
    
  }

  io.read_data := RegNext(memory.readSync(
    enable  = io.read.enable,
    address = io.read.addr)
  (counterWidth until counterWidth+windowSize))
}




///////////////////////////////SIMULATION//////////////////////////////////////
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object PreventReplaySim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplay(10, 10, 64, 1024)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      for(i <-0 until 20){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }

      for(i <-100 until 120){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }

      for(i <-100 until 110){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }

      //inside window
      dut.io.sessionId.assignBigInt(1)
      dut.io.counter.assignBigInt(157)
      dut.clockDomain.waitRisingEdge(2)
      //Window is now 157 - 150, but should not be dropped
      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }
      //Window is now 140 - 150, but should be dropped
      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }

      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }

      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }

      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge(2)
      }
    }
  }
}