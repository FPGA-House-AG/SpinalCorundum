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
      val toplevel = new PreventReplay(10, 10, 16, 1024)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplay(10, 10, 16, 1024)
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
      //val read_data = out Bits(windowSize     bits)
      //val read      = ReadBundle(log2Up(numberOfSessions), counterWidth)
  }
  
  var state  = ReceiveWindow(windowSize, counterWidth)
  var result = UInt(2 bits)
  result := 0
  var hazard_case = UInt(2 bits)
  hazard_case := 0
  var memory = Mem(Bits(state.asBits.getWidth bits), numberOfSessions)
  memory.initBigInt(Seq.fill(numberOfSessions)(0))

  //var addressReg = Reg(UInt(sessionIdWidth bits))
  //addressReg    := RegNext(io.sessionId.resize(log2Up(numberOfSessions) bits))
  /*mem read latency 2 cycles*/
  /*FIRST STAGE DELAY*/
  var d1_sessionId = RegNext(io.sessionId).init(0)
  var d1_counter   = RegNext(io.counter).init(0)
  /*SECOND STAGE DELAY*/
  var d2_data      = RegNext(memory.readSync(address = d1_sessionId.resize(log2Up(numberOfSessions) bits), enable  = True))
  var d2_sessionId = RegNext(d1_sessionId).init(0)
  var d2_counter   = RegNext(d1_counter).init(0)
  var d2_sop       = True
  var d2_valid     = RegNext(True) 

  /*THIRD STAGE DELAY*/ /*HERE WE DRIVE THE OUTPUT*/
  var d3_data      = RegNext(d2_data).init(0)
  var d3_sessionId = RegNext(d2_sessionId).init(0)
  var d3_counter   = RegNext(d2_counter).init(0)
  var d3_sop       = RegNext(d2_sop)
  var d3_valid     = RegNext(d2_valid)

  /*FOURTH STAGE DELAY*/ /*REGISTER THE STATE AND DATA ADDRESS FOR WRITING*/ 
  var d4_sessionId = RegNext(d3_sessionId).init(0)
  var d4_counter   = RegNext(d3_counter).init(0)
  var d4_sop       = RegNext(d3_sop)
  var d4_valid     = RegNext(d3_valid)
  var d4_data      = RegNext(d3_data)//


  /*FIFTH STAGE DELAY*/ /*REMEMBER WRITTEN DATA*/
  var d5_data      = RegNext(d4_data)
  var d5_sessionId = RegNext(d4_sessionId).init(0)
  var d5_counter   = RegNext(d4_counter).init(0)
  var d5_sop       = RegNext(d4_sop)
  var d5_valid     = RegNext(d4_valid)
  //io.drop         := d4_valid
  var start_of_ops = d2_valid & d3_valid & d4_valid & d5_valid
  memory.readWriteSync(d3_sessionId, data = d3_data, enable = True, write = True)
  /*SOLVE RACE CONDITIONS*/
  var take_from_d3 = False
  var take_from_d4 = False
  var take_from_d5 = False
  
  //var dataReg    = Reg(Bits(counterWidth+windowSize bits))
  var counterReg = d2_counter
  var sessionReg = d2_sessionId

  
  
  when(d5_data(windowSize, counterWidth bits).asUInt > d2_data(windowSize, counterWidth bits).asUInt){
    //dataReg    = d5_data
    state.wt     := d5_data(windowSize, counterWidth bits).asUInt
    state.bitmap := d5_data(0, windowSize bits)
    hazard_case := 3
  }.elsewhen(d4_data(windowSize, counterWidth bits).asUInt > d2_data(windowSize, counterWidth bits).asUInt){
    state.wt     := d4_data(windowSize, counterWidth bits).asUInt
    state.bitmap := d4_data(0, windowSize bits)
    hazard_case := 2
  }.elsewhen(d3_data(windowSize, counterWidth bits).asUInt > d2_data(windowSize, counterWidth bits).asUInt){
    state.wt     := d3_data(windowSize, counterWidth bits).asUInt
    state.bitmap := d3_data(0, windowSize bits)
    hazard_case := 1
  }.otherwise{
    state.wt     := d2_data(windowSize, counterWidth bits).asUInt
    state.bitmap := d2_data(0, windowSize bits)
    hazard_case := 0
  }
  
  //state.wt     := dataReg(windowSize, counterWidth bits).asUInt
  //state.bitmap := dataReg(0, windowSize bits)

  var s_val    = counterReg
  var s_ptr = s_val % windowSize

  when(start_of_ops === True){
    when(s_val === U(0, counterWidth bits)){
      // Start of operation or wrapped
      result := 0
      io.drop := False
    }.elsewhen(s_val > state.wt){
      // New packet, slide the window
      var diff = s_val - state.wt - 1
      result := 1

      for(i <- 0 until windowSize){
        when((i > state.wt+1) & (i < s_val)){
          state.bitmap(i % windowSize)     := False
        }.elsewhen(i === state.wt+1){
          state.bitmap(i % windowSize)     := True
        }
      }

      d3_data := s_val.asBits ## state.bitmap
      d2_data := s_val.asBits ## state.bitmap
      //memory(io.sessionId.resize(log2Up(numberOfSessions) bits)) := s_val.asBits ## state.bitmap //////////////////////////////////////////////

      io.drop := False

    }.elsewhen(s_val + U(windowSize, counterWidth bits) <= state.wt){
      // Too old packet
      result := 2
      io.drop := True
    }.otherwise{
      result := 3
      var conditionalReg = Reg(Bool())
      conditionalReg := state.bitmap(s_ptr.resize(log2Up(windowSize) bits)) === True// && d3_counter =/= d2_counter
      // S inside window, check the memory
      when(conditionalReg === False){
        //memory(io.sessionId.resize(log2Up(numberOfSessions) bits)) := s_val.asBits ## state.bitmap ////////////////////////////////////////////
        state.bitmap(s_ptr.resize(log2Up(windowSize) bits)) := True 
        when(state.bitmap.andR){
          io.drop := True
        }.otherwise{
          io.drop := False
        } 
        //We haven't seen this packet yet. We set the bit in memory, and don't update the window
      }.otherwise{
        //We've seen this packet already, we drop it, and we don't update the window.
        io.drop := True
        
      }
      //d4_data := state.wt ## state.bitmap
      d3_data := state.wt ## state.bitmap
      d2_data := state.wt ## state.bitmap
      
    }
  }.otherwise{
    io.drop := False
  }

}




///////////////////////////////SIMULATION//////////////////////////////////////
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object PreventReplaySim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplay(10, 10, 16, 1024)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)

      for(i <-0 until 20){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }

      for(i <-0 until 20){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }
      /*
      for(i <-100 until 110){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }

      //inside window
      dut.io.sessionId.assignBigInt(1)
      dut.io.counter.assignBigInt(157)
      dut.clockDomain.waitRisingEdge()
      //Window is now 157 - 150, but should not be dropped
      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }
      //Window is now 140 - 150, but should be dropped
      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }

      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }

      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }

      for(i <-147 until 157){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(i)
        dut.clockDomain.waitRisingEdge()
      }*/
    }
  }
}