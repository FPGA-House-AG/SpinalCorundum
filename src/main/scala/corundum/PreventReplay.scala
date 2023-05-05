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
      val valid     = in  Bool()
  }
  
  var state  = ReceiveWindow(windowSize, counterWidth)
  var result = UInt(2 bits)
  result := 0
  var hazard_case = UInt(2 bits)
  hazard_case := 0
  var memory = Mem(ReceiveWindow(windowSize, counterWidth), numberOfSessions)
  memory.initBigInt(Seq.fill(numberOfSessions)(1))

  /*mem read latency 2 cycles*/
  /*FIRST STAGE DELAY*/
  var d1_sessionId = RegNext(io.sessionId).init(0)
  var d1_counter   = RegNext(io.counter).init(0)
  var d1_valid     = RegNext(io.valid).init(False)
  /*SECOND STAGE DELAY*/
  var d2_sessionId = RegNext(d1_sessionId).init(0)
  var d2_counter   = RegNext(d1_counter).init(0)
  var d2_valid     = RegNext(d1_valid).init(False)
  var d2_data      = ReceiveWindow(windowSize, counterWidth)
  /*THIRD STAGE DELAY*/
  var d3_sessionId = RegNext(d2_sessionId).init(0)
  var d3_counter   = RegNext(d2_counter).init(0)
  var d3_valid     = RegNext(d2_valid).init(False)
  var d3_data      = RegNext(d2_data)
  //var d3_sop       = RegNext(d2_sop)
  /*FOURTH STAGE DELAY*/ 
  var d4_sessionId = RegNext(d3_sessionId).init(0)
  var d4_counter   = RegNext(d3_counter).init(0)
  var d4_valid     = RegNext(d3_valid).init(False)
  var d4_data      = RegNext(d3_data)
  //var d4_sop       = RegNext(d3_sop)
  /*FIFTH STAGE DELAY*/
  var d5_sessionId = RegNext(d4_sessionId).init(0)
  var d5_counter   = RegNext(d4_counter).init(0)
  var d5_valid     = RegNext(d4_valid).init(False)
  var d5_data      = RegNext(d4_data)
  //var d5_sop       = RegNext(d4_sop)
  
  var start_of_ops = d1_valid | d2_valid// | d4_valid | d5_valid// | (d2_counter===0 & d1_counter===1) | (d3_data.wt === d2_data.wt)
  
  var take_from_memory = True
  state := RegNext(memory.readSync(address = io.sessionId, enable = True))

  //var counterReg = UInt(counterWidth bits)//d2_counter
  //var sessionReg = Uint(sessionIdWidth bits)//d2_sessionId
  //     register all inputs, which includes: register the input address for the memory, (d1)
  //     register the memory data output, register the registered inputs (d2)
  //     drive the output, calculate new state based on memory data output and registered inputs (d2)
  //     register the state (d3)
  //     register data address for writing (d4)
  //     remember written data (d5)
  val take_from_d3 = False
  val take_from_d4 = False
  val take_from_d5 = False
  /*SOLVE HAZARDS*/
  when(d3_valid && (d3_sessionId === d2_sessionId)){
    // fetch state from d3_data instead of from memory, because the memory is not yet updated
    state            := d3_data
    take_from_memory := False
    take_from_d3     := d2_valid

  }.elsewhen(d4_valid && (d4_sessionId === d2_sessionId)){
    // fetch state from d4_data instead of from memory, because the memory is not yet updated
    state            := d4_data
    take_from_memory := False
    take_from_d4     := d2_valid

  }.elsewhen(d5_valid && (d5_sessionId === d2_sessionId)){
    // fetch state from d5_data instead of from memory, because the memory is not yet updated
    state            := d5_data
    take_from_memory := False
    take_from_d4     := d2_valid
  }
  d2_data := state

  var do_writeback = d3_valid
  memory.write(d3_sessionId, data = d3_data, enable = do_writeback)
  
  var s_val       = d2_counter
  var s_ptr       = (s_val % windowSize).resize(log2Up(windowSize) bits)
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
      state.wt     := U(0, counterWidth bits)
      d3_data      := state

    }.elsewhen(s_val > state.wt){
      // New packet, slide the window
      var diff = s_val - state.wt - 1
      result := 1
      
      for(i <- 0 until windowSize){
        when(diff >= windowSize){
          state.bitmap := B(0, windowSize bits)
        }.elsewhen(s_ptr === 0){
          when(i>((state.wt)%windowSize) /*&& (i<=((s_val)%windowSize))*/){
            state.bitmap(i) := False
          }
        }.otherwise{
          when(i>=((state.wt+1)%windowSize) && (i<=((s_val-1)%windowSize))){
            state.bitmap(i) := False
          }
        }
      }
      
      state.bitmap(s_ptr) := True
      d3_data.wt          := s_val
      d3_data.bitmap      := state.bitmap
      io.drop  := False
      
    }.elsewhen(s_val + U(windowSize, counterWidth bits) <= state.wt){
      // Too old packet
      result := 2
      io.drop := True

    }.otherwise{
      result := 3
      // S inside window, check the memory
      when(state.bitmap(s_ptr) === True){
        //We've seen this packet already, we drop it, and we don't update the window.
        io.drop := True
      }.otherwise{
        //We haven't seen this packet yet. We set the bit in memory, and don't update the window
        io.drop := False
      }
      state.bitmap(s_ptr) := True 
      d3_data := state

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
      val testValues = Array(52, 53, 57, 59, 60, 62, 60, 63, 64, 67, 69, 71, 74, 75, 79, 80, 81, 82, 84, 85, 86, 87, 89, 90, 92, 93, 96, 97, 99, 125, 110, 118, 135, 127, 106, 128, 107, 101, 142, 115, 120, 112, 134, 114, 108, 136, 126, 100, 144, 130, 119, 122, 103, 138, 104, 123, 132, 102, 129, 133, 113, 149, 116, 143, 117, 146, 137, 131, 139, 109, 148, 140, 124, 121, 145, 111, 147, 105, 141)
      dut.clockDomain.forkStimulus(period = 2)
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)
      dut.io.valid #= true

      for(sample <- testValues){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(sample)
        dut.io.valid #= true
        dut.clockDomain.waitRisingEdge()
      }

      dut.clockDomain.waitRisingEdge(3)

    }
  }
}