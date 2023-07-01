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
      val toplevel = new PreventReplay(128, 10, 16, 1024)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplay(128, 10, 16, 1024)
      toplevel
    })
  }
}

case class PreventReplay(windowSize:        Int,
                         sessionIdWidth:    Int,
                         counterWidth:      Int,
                         numberOfSessions:  Int         
                         ) extends Component {

  require(windowSize > 0)

  // This class is our per-session state. 
  case class ReceiveWindow(windowSize : Int, counterWidth : Int) extends Bundle {
    val wt          = UInt(counterWidth bits)
    val bitmap      = Bits(windowSize bits) 
  }

  val io = new Bundle {
      val drop      = out Bool()
      val sessionId = in  UInt(sessionIdWidth bits)
      val counter   = in  UInt(counterWidth   bits)
      val valid     = in  Bool()
  }
  
  val memory = Mem(ReceiveWindow(windowSize, counterWidth), numberOfSessions)
  memory.initBigInt(Seq.fill(numberOfSessions)(1))

  /*mem read latency 2 cycles*/
  
  val d1_sessionId = RegNext(io.sessionId).init(0)
  val d1_counter   = RegNext(io.counter).init(0)
  val d1_valid     = RegNext(io.valid).init(False)

  // d2 is the current state fetched from memory
  val d2_sessionId = RegNext(d1_sessionId).init(0)
  val d2_counter   = RegNext(d1_counter).init(0)
  val d2_valid     = RegNext(d1_valid).init(False)
  // the state is read from memory or from the write pipeline in case of a hazard
  val state        = ReceiveWindow(windowSize, counterWidth)

  // from state we define the next_state
  val next_state   = ReceiveWindow(windowSize, counterWidth)

  // d3 registers the new state
  val d3_sessionId = RegNext(d2_sessionId).init(0)
  val d3_counter   = RegNext(d2_counter).init(0)
  val d3_valid     = RegNext(d2_valid).init(False)
  val d3_data      = RegNext(next_state)

  // d4 and d5 remember what is written to memory, to resolve data hazards
  val d4_sessionId = RegNext(d3_sessionId).init(0)
  val d4_counter   = RegNext(d3_counter).init(0)
  val d4_valid     = RegNext(d3_valid).init(False)
  val d4_data      = RegNext(d3_data)

  val d5_sessionId = RegNext(d4_sessionId).init(0)
  val d5_counter   = RegNext(d4_counter).init(0)
  val d5_valid     = RegNext(d4_valid).init(False)
  val d5_data      = RegNext(d4_data)
  
  val take_from_memory = True
  state := RegNext(memory.readSync(address = io.sessionId, enable = True))

  val take_from_d3 = False
  val take_from_d4 = False
  val take_from_d5 = False
  /* solve data read/write hazards */
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

  val do_writeback = d3_valid
  memory.write(d3_sessionId, data = d3_data, enable = do_writeback)

  val ptr_bits    = log2Up(windowSize)
  val s_val       = d2_counter
  val s_ptr       = s_val.resize(ptr_bits bits)
  val wt_ptr      = state.wt.resize(ptr_bits bits)

  val higher = False
  val lower = False
  val inside = False

  next_state := state
  when (s_val > state.wt) {
    higher := True
    // slide the window WT to S, clear diff bits doing so
    val diff = s_val - state.wt
    // diff2 is (diff-1) min (windowSize-1), then modulo windowSize
    val diff2 = UInt(ptr_bits bits)
    diff2 := Mux(diff >= U(windowSize), U(windowSize - 1), (diff - 1).resize(ptr_bits))
    // first bit to clear
    val first = (wt_ptr + 1).resize(ptr_bits)
    // last bit to clear
    val last = (wt_ptr + diff2 + 1).resize(ptr_bits)
    val reversed = (first > last)

    // clear bits in the bitmask (approach #1 using generate loop starting from i == wr_ptr + 1)
    if (true) {
      for (i <- 0 until windowSize){
        // should we clear this bit?
        when (diff2 >= U(i, ptr_bits bits)) {
          next_state.bitmap((wt_ptr + U(i) + 1) % windowSize) := False
        }
      }
    // clear bits in the bitmask (approach #2 using generate loop starting from i == 0)
    // bits to clear are in range [first, last] but first/last are modulo, so also cover
    // the case where last appears reversed (before) first
    } else if (false) {
      for (i <- 0 until windowSize){
        // should we clear this bit?   within [first, last] or within [last, first]
        // & !(condition) clears the bits with the condition
        // the condition is either i is in [first,last]
        // or i is inside [0, last] or inside [first, windowSize-1] if last < first
        next_state.bitmap(i) := state.bitmap(i) & !(((i >= first) & (i <= last)) | (((i >= first) | (i <= last)) & reversed))
      }
    // clear bits in the bitmask (approach #3 using a bitmask)
    } else {
      // window mask with all bits set
      val mask = Bits(windowSize bits).setAll()
      // zero "diff" most significant bits
      val mask2 = mask |>> diff2
      // rotate to WT -- this does not scale for large windowSize?!
      val mask3 = mask2.rotateRight(state.wt.resize(ptr_bits bits))
      next_state.bitmap := state.bitmap & mask3
    }
    // set bit for received counter
    next_state.bitmap(s_ptr) := True
    next_state.wt            := s_val
    io.drop  := False
  }.elsewhen(s_val + U(windowSize, counterWidth bits) <= state.wt){
    lower := True
    // Too old packet
    io.drop := True
  // S inside window, check the memory
  }.otherwise{
    inside := True
    io.drop := state.bitmap(s_ptr)
    next_state.bitmap(s_ptr) := True 
  }
}

import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object PreventReplaySim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplay(128, 10, 16, 1024)){dut =>
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

object PreventReplayLinuxSim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplay(128, 10, 16, 1024)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      val testValues = Array(0, 1, 1, 9, 8, 7, 7, 128, 127, 127, 126, 2, 2, 144, 3, 144, 512, 385, 10, 384, 383, 386, 385, 0) 
      val retValues  = Array(0, 0, 1, 0, 0, 0, 1,   0,   0,   1,   0, 0, 1,   0, 1,   1,   0,   0,  1,   1,   1,   0,   1, 1)
      dut.clockDomain.forkStimulus(period = 2)
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)
      dut.io.valid #= true
      
      for(i <- 0 until testValues.size+2){

        if(i<testValues.size){
          dut.io.sessionId.assignBigInt(1)
          dut.io.counter.assignBigInt(testValues(i))
          dut.io.valid #= true
        }

        dut.clockDomain.waitRisingEdge()
        
        if(i>=2){
          var retVal = dut.io.drop.toBoolean
          println(retVal)
          println(retValues(i-2))
          
          if(retVal != retValues(i-2).toBoolean){
            throw new SimFailure("WRONG RESULT")
          }

        }
      }

      dut.clockDomain.waitRisingEdge()
      throw new SimSuccess
    }
  }
}