package corundum

import spinal.core._
import spinal.lib._

import sourcecode._
import java.io._
import sys.process._
import rfc6479.RFC6479_MN
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

// companion object for case class
object PreventReplayMN {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val verilogReport = Config.spinal.generateVerilog({
      val toplevel = new PreventReplayMN(10, 16, 1024, 32, 4)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplayMN(10, 16, 1024, 32, 4)
      toplevel
    })
  }
}

case class PreventReplayMN(sessionIdWidth:    Int,
                         counterWidth:      Int,
                         numberOfSessions:  Int,
                         n:                 Int,
                         m:                 Int         
                         ) extends Component {
                            
    
    val windowSize = m*n
    // This class is our per-session state. 
    case class ReceiveWindow(windowSizeValue : Int, counterWidthValue : Int) extends Bundle {
    val wt          = UInt(counterWidthValue bits)
    val bitmap      = Bits(windowSizeValue bits) 
    }

    val io = new Bundle {
        val drop      = out Bool()
        val sessionId = in  UInt(sessionIdWidth bits)
        val counter   = in  UInt(counterWidth   bits)
        val valid     = in  Bool()
        val clear     = in  Bool()
    }

    val memory = Mem(ReceiveWindow(windowSize, counterWidth), numberOfSessions)
    memory.initBigInt(Seq.fill(numberOfSessions)(1))

    /*mem read latency 2 cycles*/

    val d1_sessionId = RegNext(io.sessionId).init(0)
    val d1_counter   = RegNext(io.counter).init(0)
    val d1_valid     = RegNext(io.valid).init(False)
    val d1_clear     = RegNext(io.clear).init(False)

    // d2 is the current state fetched from memory
    val d2_sessionId = RegNext(d1_sessionId).init(0)
    val d2_counter   = RegNext(d1_counter).init(0)
    val d2_valid     = RegNext(d1_valid).init(False)
    val d2_clear     = RegNext(d1_clear).init(False)
    // the state is read from memory or from the write pipeline in case of a hazard
    val state        = ReceiveWindow(windowSize, counterWidth)

    // from state we define the next_state
    val next_state   = ReceiveWindow(windowSize, counterWidth)

    // d3 registers the new state
    val d3_sessionId = RegNext(d2_sessionId).init(0)
    val d3_counter   = RegNext(d2_counter).init(0)
    val d3_valid     = RegNext(d2_valid).init(False)
    val d3_clear     = RegNext(d2_clear).init(False)
    val d3_data      = RegNext(next_state)

    // d4 and d5 remember what is written to memory, to resolve data hazards
    val d4_sessionId = RegNext(d3_sessionId).init(0)
    val d4_counter   = RegNext(d3_counter).init(0)
    val d4_valid     = RegNext(d3_valid).init(False)
    val d4_clear     = RegNext(d3_clear).init(False)
    val d4_data      = RegNext(d3_data)

    val d5_sessionId = RegNext(d4_sessionId).init(0)
    val d5_counter   = RegNext(d4_counter).init(0)
    val d5_valid     = RegNext(d4_valid).init(False)
    val d5_clear     = RegNext(d4_clear).init(False)
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

    val window_bits    = log2Up(windowSize)
    val block_ptr_bits = log2Up(m)
    val bit_ptr_bits   = log2Up(n)
    val s_val          = d2_counter
    val s_ptr          = s_val.resize(window_bits bits)
    val wt_ptr         = state.wt.resize(window_bits bits)

    val higher = False
    val lower = False
    val inside = False

    next_state := state

    // their_counter === s_val === incomming new packet
    // counter->counter === state.wt == Wt === biggest encountered counter yet for this session.
    // COUNTER_WINDOW_SIZE === windowSize, power of 2
    // BITS_PER_LONG === n === 32 === bits per block
    // COUNTER_BITS_TOTAL === m*n === 128 bits === dimension of memory entry, size of state and memory(i)
    // COUNTER_BITS_TOTAL/BITS_PER_LONG === m === 4 === number of blocks
    // slide the window WT to S, clear diff blocks doing so
    val diff = (s_val - state.wt) / n
    val diff2 = UInt(block_ptr_bits bits)
    diff2 := Mux(diff >= U(m), U(m - 1), (diff - 1).resize(block_ptr_bits))
    // first block to clear
    val first = (wt_ptr / n) + 1
    // last block to clear
    val last = (wt_ptr / n) + diff2 + 1
    val reversed = (first > last)
    
    when (s_val > state.wt) {
      higher := True
      wt_ptr := state.wt.resize(window_bits bits)

      for (i <- 0 until windowSize){
        // should we clear this bit?   within [first, last] or within [last, first]
        // & !(condition) clears the bits with the condition
        // the condition is either i is in [first,last]
        // or i is inside [0, last] or inside [first, windowSize-1] if last < first
        val i_block = i >> log2Up(n)
        next_state.bitmap(i) := state.bitmap(i) & (!(((i_block >= first) & (i_block <= last)) | (((i_block >= first) | (i_block <= last)) & reversed)) /*| diff === 0*/)
      }

      // set bit for received counter
      next_state.bitmap(s_ptr) := True
      next_state.wt            := s_val
      io.drop  := False

    }.elsewhen(s_val + U(windowSize, counterWidth bits) <= state.wt & ((s_val + U(windowSize, counterWidth bits)) >= s_val) ){ //overflow detection since s_val i UInt
      lower := True
      // Too old packet
      io.drop := True
      // S inside window, check the memory
    
    }.elsewhen(d2_clear === True){
      next_state.wt     := 0
      next_state.bitmap := B(0, windowSize bits)
      io.drop := False
    
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

object PreventReplayMNSim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplayMN(10, 16, 1024, 32, 4)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      val testValues = Array(52, 53, 57, 59, 60, 62, 60, 63, 64, 67, 69, 71, 74, 75, 79, 80, 81, 82, 84, 85, 86, 87, 89, 90, 92, 93, 96, 97, 99, 125, 110, 118, 135, 127, 106, 128, 107, 101, 142, 115, 120, 112, 134, 114, 108, 136, 126, 100, 144, 130, 119, 122, 103, 138, 104, 123, 132, 102, 129, 133, 113, 149, 116, 143, 117, 146, 137, 131, 139, 109, 148, 140, 124, 121, 145, 111, 147, 105, 141)
      dut.clockDomain.forkStimulus(period = 2)
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)
      dut.io.valid #= true
      dut.io.clear #= false

      for(sample <- testValues){
        dut.io.sessionId.assignBigInt(1)
        dut.io.counter.assignBigInt(sample)
        dut.io.valid #= true
        dut.io.clear #= false
        dut.clockDomain.waitRisingEdge()
      }

      dut.clockDomain.waitRisingEdge(3)

    }
  }
}

object PreventReplayMNLinuxSim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplayMN(10, 16, 1024, 32, 4)){dut =>
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
          dut.io.clear #= false
        }

        dut.clockDomain.waitRisingEdge()
        
        if(i>=2){
          var retVal = dut.io.drop.toBoolean
          println(retVal)
          println(retValues(i-2))
          println("")
          
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


object PreventReplayRFC6479_MN {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplayMN(10, 16, 1024, 32, 4)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      val testValues   = ArrayBuffer[Int]() 
      val retValues    = ArrayBuffer[Boolean]()
      val whatHappened = ArrayBuffer[Int]()
      val seed:        Long = 123L
      val random:      Random = new Random(seed)
      val rfc:         RFC6479_MN = new RFC6479_MN()
      var first, second: Int = 0
      for(i <- 0 until 50000){
        var randValue = random.nextInt()
        randValue = randValue%65536
        randValue = randValue.abs
        var retValue  = rfc.counter_validate(randValue)
        var whatH     = rfc.resultArray.last
        /*if(randValue==65535){
          println(randValue)
          println(whatH)
          println(rfc.memory.mkString("")) 
        }*/

        testValues.append(randValue)
        retValues.append(retValue)
        whatHappened.append(whatH)
      }
      //println(testValues.mkString(" "))
      //println(retValues.mkString(" "))

      dut.clockDomain.forkStimulus(period = 2)
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)
      dut.io.valid #= true
      dut.io.clear #= false

      for(i <- 0 until testValues.size+2){

        if(i<testValues.size){
          dut.io.sessionId.assignBigInt(1)
          dut.io.counter.assignBigInt(testValues(i))
          dut.io.valid #= true
          dut.io.clear #= false
        }

        dut.clockDomain.waitRisingEdge()
        if(i>=2){
          /*if(testValues(i-2) == 65518){
            second = first
            first = i-2
            println(first)
            println(second)    
          }*/
          
          var retVal = dut.io.drop.toBoolean
          /*print(i+1)
          print("th value for input=")
          print(testValues(i-2))
          print(" ")
          print(" output of RFC6479_MN ")
          print(whatHappened(i-2))
          print(" ")
          print(retVal)
          print(" == ")
          print(retValues(i-2))
          print(" ?")
          println("")*/
          //00000001001010000001010000001000001000000001100100100000010010001101100000011010010001000000011010110101000111100111011001100101
          //00000001001010000001010000001000001000000001100100100000010010001101100000011010010001000000011010110101000111100111011001100101
          if(retVal != retValues(i-2)){
            print(i+1)
            print("th value for input=")
            print(testValues(i-2))
            print(" ")
            print(" output of RFC6479_MN ")
            print(whatHappened(i-2))
            print(" ")
            print(retVal)
            print(" == ")
            print(retValues(i-2))
            print(" ?")
            println("")   
            throw new SimFailure("WRONG RESULT")
          }

        }
      }
      
      dut.clockDomain.waitRisingEdge()
      throw new SimSuccess
    }
  }
}