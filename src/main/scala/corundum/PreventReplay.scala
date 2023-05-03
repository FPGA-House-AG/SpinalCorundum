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
      val toplevel = new PreventReplay(8, 10, 16, 1024)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplay(8, 10, 16, 1024)
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
  memory.initBigInt(Seq.fill(numberOfSessions)(1))

  /*mem read latency 2 cycles*/
  /*FIRST STAGE DELAY*/
  var d1_sessionId = RegNext(io.sessionId).init(0)
  var d1_counter   = RegNext(io.counter).init(0)
  /*SECOND STAGE DELAY*/
  var d2_data      = RegNext(memory.readSync(address = d1_sessionId.resize(log2Up(numberOfSessions) bits), enable  = True)).init(0)
  var d2_sessionId = RegNext(d1_sessionId).init(0)
  var d2_counter   = RegNext(d1_counter).init(0)
  var d2_sop       = True
  var d2_valid     = RegNext(True) 
  /*THIRD STAGE DELAY*/
  var d3_data      = RegNext(d2_data).init(0)
  var d3_sessionId = RegNext(d2_sessionId).init(0)
  var d3_counter   = RegNext(d2_counter).init(0)
  var d3_sop       = RegNext(d2_sop)
  var d3_valid     = RegNext(d2_valid).init(False)
  /*FOURTH STAGE DELAY*/ 
  var d4_sessionId = RegNext(d3_sessionId).init(0)
  var d4_counter   = RegNext(d3_counter).init(0)
  var d4_sop       = RegNext(d3_sop)
  var d4_valid     = RegNext(d3_valid).init(False)
  var d4_data      = RegNext(d3_data).init(0)
  /*FIFTH STAGE DELAY*/
  var d5_data      = RegNext(d4_data).init(0)
  var d5_sessionId = RegNext(d4_sessionId).init(0)
  var d5_counter   = RegNext(d4_counter).init(0)
  var d5_sop       = RegNext(d4_sop)
  var d5_valid     = RegNext(d4_valid).init(False)
  var start_of_ops = d2_valid & d3_valid & d4_valid & d5_valid | (d2_counter===0 & d1_counter===1)
  memory.readWriteSync(d3_sessionId, data = d3_data, enable = True, write = True)
  
  var counterReg = d2_counter
  var sessionReg = d2_sessionId

  when(d3_sessionId === d2_sessionId){
    when(d4_data(windowSize, counterWidth bits).asUInt >= d2_data(windowSize, counterWidth bits).asUInt){
      state.wt     := d4_data(windowSize, counterWidth bits).asUInt
      state.bitmap := d4_data(0, windowSize bits)
      hazard_case := 2
    }.elsewhen(d3_data(windowSize, counterWidth bits).asUInt >= d2_data(windowSize, counterWidth bits).asUInt){
      state.wt     := d3_data(windowSize, counterWidth bits).asUInt
      state.bitmap := d3_data(0, windowSize bits)
      hazard_case := 1
    }.otherwise{
      state.wt     := d2_data(windowSize, counterWidth bits).asUInt
      state.bitmap := d2_data(0, windowSize bits)
      hazard_case := 0
    }
  }.otherwise{
    state.wt     := d2_data(windowSize, counterWidth bits).asUInt
    state.bitmap := d2_data(0, windowSize bits)
    hazard_case := 0
  }
  
  var s_val    = counterReg
  var s_ptr = s_val % windowSize
  var gotten_zero = Reg(Bool())

  when(start_of_ops === True){
    when(s_val === U(0, counterWidth bits) & gotten_zero===False){
      // Start of operation or wrapped
      result := 0
      when(state.bitmap.andR===True){
        io.drop := True
      }.otherwise{
        io.drop := False
      }
      gotten_zero := True
      state.bitmap := B(1, windowSize bits)
      when(d4_data.asUInt < (s_val.asBits ## state.bitmap).asUInt){
        d4_data := s_val.asBits ## state.bitmap
        d3_data := s_val.asBits ## state.bitmap
        d2_data := s_val.asBits ## state.bitmap
      }
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

      d4_data := s_val.asBits ## state.bitmap
      d3_data := s_val.asBits ## state.bitmap
      d2_data := s_val.asBits ## state.bitmap

      io.drop := False

    }.elsewhen(s_val + U(windowSize, counterWidth bits) <= state.wt){
      // Too old packet
      result := 2
      io.drop := True
    }.otherwise{
      result := 3
      var conditionalReg = Reg(Bool())
      conditionalReg := state.bitmap(s_ptr.resize(log2Up(windowSize) bits)) === True
      // S inside window, check the memory
      when(conditionalReg === False){
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
      d4_data := state.wt ## state.bitmap
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
    SimConfig.withFstWave.doSim(new PreventReplay(8, 10, 16, 1024)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      val testValues = Array(52, 53, 57, 59, 60, 62, 63, 64, 67, 69, 71, 74, 75, 79, 80, 81, 82, 84, 85, 86, 87, 89, 90, 92, 93, 96, 97, 99, 125, 110, 118, 135, 127, 106, 128, 107, 101, 142, 115, 120, 112, 134, 114, 108, 136, 126, 100, 144, 130, 119, 122, 103, 138, 104, 123, 132, 102, 129, 133, 113, 149, 116, 143, 117, 146, 137, 131, 139, 109, 148, 140, 124, 121, 145, 111, 147, 105, 141)
      dut.clockDomain.forkStimulus(period = 2)
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)

      for(sample <- testValues){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(sample)
        dut.clockDomain.waitRisingEdge()
      }
      
      dut.clockDomain.waitRisingEdge(3)

    }
  }
}