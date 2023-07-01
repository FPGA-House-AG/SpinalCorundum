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
      val toplevel = new PreventReplayMN(10, 64, 64, 4) // 350  MHz
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new PreventReplayMN(10, 64, 64, 4)
      toplevel
    })
  }
}

// ~runMain corundum.PreventReplayRFC6479_MN

case class PreventReplayMN(
                         sessionIdWidth:    Int,
                         counterWidth:      Int,
                         n:                 Int,
                         m:                 Int,
                         initMem: Boolean = false
                         ) extends Component
{
  val windowSize = m * n
  val rejectAfterMessages = (BigInt(1) << counterWidth) - (m-1)*n - 1
  printf("rejectAfterMesssages = %s (0x%16x)\n", rejectAfterMessages.toString, rejectAfterMessages)
  val numberOfSessions = (1 << sessionIdWidth)

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

  val memory = Mem(ReceiveWindow(windowSize, counterWidth).asBits, numberOfSessions)
  memory.addAttribute(new AttributeString("RAM_STYLE", "block"))
  if (initMem) {
    memory.initBigInt(Seq.fill(numberOfSessions)(0))
  }

  /* memory read latency is 2 cycles */

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

  // from counter_d1 we derive if we should drop due to REJECT_AFTER_MESSAGES
  val reject_d2    = RegNext(d1_counter >= rejectAfterMessages)

  // d3 registers the new state
  val d3_sessionId = RegNext(d2_sessionId)
  val d3_counter   = RegNext(d2_counter)
  val d3_valid     = RegNext(d2_valid).init(False)
  val d3_data      = RegNext(next_state)

  // d4 and d5 remember what is written to memory, to resolve data hazards
  val d4_sessionId = RegNext(d3_sessionId)
  val d4_counter   = RegNext(d3_counter)
  val d4_valid     = RegNext(d3_valid).init(False)
  val d4_data      = RegNext(d3_data)

  val d5_sessionId = RegNext(d4_sessionId)
  val d5_counter   = RegNext(d4_counter)
  val d5_valid     = RegNext(d4_valid).init(False)
  val d5_data      = RegNext(d4_data)

  state.assignFromBits(RegNextWhen(memory.readSync(address = io.sessionId, enable = io.valid), d1_valid))

  val take_from_memory = False
  val take_from_d3 = False
  val take_from_d4 = False
  val take_from_d5 = False
  /* solve data read/write hazards */
  when (d3_valid && (d3_sessionId === d2_sessionId)) {
    // fetch state from d3_data instead of from memory, because the memory is not yet updated
    state            := d3_data
    take_from_d3     := d2_valid
  }
  .elsewhen (d4_valid && (d4_sessionId === d2_sessionId)) {
    // fetch state from d4_data instead of from memory, because the memory is not yet updated
    state            := d4_data
    take_from_d4     := d2_valid
  }
  .elsewhen (d5_valid && (d5_sessionId === d2_sessionId)) {
    // fetch state from d5_data instead of from memory, because the memory is not yet updated
    state            := d5_data
    take_from_d4     := d2_valid
  }
  .otherwise {
    take_from_memory := True
  }

  val do_writeback = d3_valid
  memory.write(d3_sessionId, data = d3_data.asBits, enable = d3_valid)

  val mn_bits  = log2Up(windowSize)
  val m_bits   = log2Up(m)
  val n_bits   = log2Up(n)

  val higher = False
  val lower = False
  val inside = False

  next_state := state

  val s_plus_1_d1         = RegNext(io.counter + 1)
  // S+1
  val s_plus_1            = RegNext(s_plus_1_d1)
  // N*(M-1) + S+1
  val s_plus_1_plus_win   = RegNext(s_plus_1_d1 + n*(m-1))
  // (S+1) % WS
  val s_plus_1_ptr        = s_plus_1.resize(mn_bits bits)
  // WT % WS
  val wt_ptr              = state.wt.resize(mn_bits bits)

  // (block index of (S+1)) >= m ?
  val index_s_plus_1_ge_m       = RegNext((s_plus_1_d1 >> n_bits) >= m)

  // (block index of (S+1)) - m
  val index_s_plus_1_minus_m    = RegNext((s_plus_1_d1 >> n_bits) - m)

  // block indexes (not modulo M!) for (S+1) and WT
  val index_s_plus_1      = s_plus_1 >> n_bits
  val index_current       = state.wt >> n_bits

  // clear all blocks if (index_s_plus_1 - index_current) >= m  (because we only have m blocks)
  // clear all blocks if index_s_plus_1 >= (index_current + m)
  //val clear_all_blocks    = (index_s_plus_1 >= (index_current + m))
  //val clear_all_blocks    = ((index_s_plus_1 - m) >= index_current) && (index_s_plus_1 >= m)
  val clear_all_blocks    = (index_s_plus_1_minus_m >= index_current) && index_s_plus_1_ge_m
  // clear no blocks if S+1 is in same block as where WT is
  val clear_no_blocks     = (index_s_plus_1 === index_current)
  // if some, but not all, blocks are cleared, just for debugging, not used
  val clear_some_blocks   = (!clear_all_blocks) & (!clear_no_blocks)

  // only if *some* blocks are cleared, calculate which blocks;
  // current WT is in block (WT / N) % M, and this block was cleared earlier
  // so start clearing after current block
  val current             = (wt_ptr >> n_bits).resize(m_bits)
  // last block to clear is block where S+1 is in
  val last                = (s_plus_1_ptr >> n_bits).resize(m_bits)
  val last_is_wrapped     = (current > last)

  // for each block, decide if to clear if (their_counter > counter->counter)
  val clear_block = Bits(m bits)
  for (i_block <- 0 until m) {
    clear_block(i_block) :=
      // clear no blocks?
      !clear_no_blocks &
      (
        // clear blocks in range (current, last] (exclusive, inclusive]
        (((i_block > current) & (i_block <= last)) & !last_is_wrapped) |
        // same, but now in case last is wrapped
        (((i_block > current) | (i_block <= last)) & last_is_wrapped) |
        // or clear all blocks
        clear_all_blocks
      )
  }

  // Linux WireGuard: if (their_counter >= REJECT_AFTER_MESSAGES)
  when (reject_d2) {
    io.drop := True
  }
  // Linux WireGuard: if (their_counter > counter->counter)
  .elsewhen (s_plus_1 > state.wt) {
    higher := True
    // iterate over blocks
    for (i_block <- 0 until m) {
      // iterate over bits in block
      for (i <- 0 until n) {
        // the condition to clear (all) the (bits in the) block is identical; !clear_block(i_block)
        next_state.bitmap(i_block * n + i) := state.bitmap(i_block * n + i) & !clear_block(i_block)
      }
    }
    // set bit for received counter
    next_state.bitmap(s_plus_1_ptr) := True
    next_state.wt := s_plus_1
    io.drop := False
  }
  // Linux WireGuard: if (unlikely((COUNTER_WINDOW_SIZE + their_counter) < counter->counter))
  .elsewhen (s_plus_1_plus_win < state.wt) {
    lower := True
    // Too old packet
    io.drop := True
  }
  .elsewhen (d2_clear === True) {
    next_state.wt := 0
    // @TODO should we clear all?
    next_state.bitmap := B(0, windowSize bits)
    io.drop := False
  }
  // S+1 inside window, check and set bitmap
  .otherwise {
    inside := True
    io.drop := state.bitmap(s_plus_1_ptr)
    next_state.bitmap(s_plus_1_ptr) := True 
  }
}

import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object PreventReplayMNSim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new PreventReplayMN(10, 64, 32, 4, initMem = true)){dut =>
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
    SimConfig.withFstWave.doSim(new PreventReplayMN(10, 64, 32, 4, initMem = true)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      val testValues = Array(0, 1, 1, 9, 8, 7, 7,  97,  96,  96,  95,  2,  2, 129, 3, 129, 388, 292, 10, 291, 290, 293, 292, 0) 
      val retValues  = Array(0, 0, 1, 0, 0, 0, 1,   0,   0,   1,   0,  0,  1,   0, 1,   1,   0,   0,  1,   1,   1,   0,   1, 1)
      dut.clockDomain.forkStimulus(period = 2)
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)
      dut.io.clear #= false
      dut.io.valid #= false

      dut.clockDomain.waitSampling()

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
    SimConfig.withFstWave.doSim(new PreventReplayMN(10, 64, 64, 4, initMem = true)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      val testValues   = ArrayBuffer[Int]() 
      val retValues    = ArrayBuffer[Boolean]()
      val whatHappened = ArrayBuffer[Int]()
      val seed:        Long = 123L
      val random:      Random = new Random(seed)
      val rfc:         RFC6479_MN = new RFC6479_MN(64, 4, 64)
      var first, second: Int = 0
      for(i <- 0 until 1000000){
        var randValue = random.nextInt()
        randValue = randValue%65535
        randValue = randValue.abs
        var retValue  = rfc.counter_validate(randValue)
        var whatH     = rfc.resultArray.last
        
        testValues.append(randValue)
        retValues.append(retValue)
        whatHappened.append(whatH)
      }

      dut.clockDomain.forkStimulus(period = 2)

      dut.clockDomain.waitSampling()
      dut.io.sessionId.assignBigInt(0)
      dut.io.counter.assignBigInt(0)
      dut.io.valid #= false
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

          var retVal = dut.io.drop.toBoolean
          
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