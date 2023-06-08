// CorundumFrameStash
//
// Passes frames through a Store-and-Forward FIFO to determine frame length.
//
// Frames are output together with their frame length on io.length and io.length_valid, these
// remain valid and stable throughout all packet cycles (active or not).
//
// When io.source.valid, io.length_valid is also always valid.
// Oversized frames are dropped. (Maximum size is currently hardcoded inside.)
// Oversized frames are internally marked, truncated to maximum FIFO length before
// entering the FIFO, and dropped after the FIFO.

package corundum

import spinal.core._
import spinal.lib._

import scala.util.Random

import spinal.core.GenerationFlags._
import spinal.core.formal._

// companion object
object CorundumFrameStash {
  def nextPowerofTwo2(x: Int): Int = {
    var y = x - 1
    for (z <- 1 to 16) y = y | (y >> z)
    y + 1
  }
  def apply(dataWidth : Int) : CorundumFrameStash = {
    val fifoSize = nextPowerofTwo2((1532 + (dataWidth/8) - 1) / (dataWidth/8))
    CorundumFrameStash(dataWidth, userWidth = 1, fifoSize)
  }
  def apply(dataWidth : Int, fifoSize : Int) : CorundumFrameStash = {
    new CorundumFrameStash(dataWidth, userWidth = 1, fifoSize)
  }
  // generate VHDL and Verilog, and SystemVerilog with Formal
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(CorundumFrameStash(Config.corundumDataWidth))
    val verilogReport = Config.spinal.generateVerilog(CorundumFrameStash(Config.corundumDataWidth))
    val formalReport = Config.spinal.includeFormal.generateSystemVerilog(CorundumFrameStash(Config.corundumDataWidth, fifoSize = 32))
  }
}

case class CorundumFrameStash(dataWidth : Int, userWidth : Int, fifoSize : Int) extends Component {
  val keepWidth = dataWidth/8
  val maxFrameBytes = 2048 //fifoSize * keepWidth
  printf("maxFrameBytes = %d\n", maxFrameBytes)
  val io = new Bundle {
    val sink = slave Stream new Fragment(CorundumFrame(dataWidth, userWidth))
    val source = master Stream new Fragment(CorundumFrame(dataWidth, userWidth))
    // worst case each packet is one beat
    val packets = out UInt(log2Up(fifoSize * 2/*@TODO why was this times 2 ?*/) bits)
    val length = out UInt(12 bits)
    val length_valid = out Bool()
    //val full = out Bool()
    //println(log2Up(fifoSize))
    val availability = out UInt((log2Up(fifoSize + 1) bits))
  }
  val length_and_truncated = new Bundle {
    val length = UInt(12 bits)
    val truncated = Bool()
  }

  val fifo = new StreamFifo(Fragment(CorundumFrame(dataWidth, userWidth)), fifoSize)

  io.availability := fifo.io.availability

  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth, userWidth))
  val w = Stream Fragment(CorundumFrame(dataWidth, userWidth))

  // skid buffer between input and x
  // at least 1 clock cycle latency
  // but full throughput
  w << io.sink.s2mPipe().m2sPipe()
  // one cycle to count leading zeroes
  x <-< w

//  //GenerationFlags.formal {
//    assume(!(io.sink.lastFire) | (io.sink.fragment.tkeep(0) === True))
//    when (w.lastFire) { assert(assertion = (w.fragment.tkeep(0) === True), message = "LAST cannot be empty") }
//    when (x.lastFire) { assert(assertion = (x.fragment.tkeep(0) === True), message = "LAST cannot be empty") }
//  //}

  val x2 = Stream Fragment(CorundumFrame(dataWidth, userWidth))
  val x3 = Stream Fragment(CorundumFrame(dataWidth, userWidth))
  // fifo source/master/pop port to component source/master port
  val y = Stream Fragment(CorundumFrame(dataWidth, userWidth))

  // track number of packets in the FIFO
  val packetsInFifoCounter = CounterUpDown(fifoSize * 2)

  // gather at least minPackets packet(s) in the FIFO before continuing the pop/output stream
  // however if the FIFO becomes full, also continue, to prevent corruption
  val fifo_holds_complete_packet = (packetsInFifoCounter.value >= 1)
  val z = y.continueWhen(fifo_holds_complete_packet).s2mPipe().m2sPipe()

  when (fifo.io.push.ready & fifo.io.push.valid & fifo.io.push.last) {
    packetsInFifoCounter.increment()
  }
  when (y.ready & y.valid & y.last) {
    packetsInFifoCounter.decrement()
  }

  // remember if current beat is not last; then next beat is not first
  val x_is_frame_continuation = RegNextWhen(!x.last, x.fire) init(False)
  val x_is_first_beat = x.fire & !x_is_frame_continuation // firstFire ?
  val x_is_last_beat = x.fire & x.last // lastFire ?
  val x_is_intermediate_beat = x.fire & x_is_frame_continuation & !x.last

  printf("%s log2Up(%d/8+1)=%d\n", sourcecode.File(), dataWidth, log2Up(dataWidth/8+1))

  // leading zeroes in 
  val tkeep_len = U(dataWidth/8)
  val w_leading_zeroes = LeadingZeroes(w.tkeep)
  val w_tkeep_count = UInt(/*log2Up(dataWidth/8 + 1) bits*/)
  w_tkeep_count := tkeep_len - w_leading_zeroes

  // measure frame length in x
  val x_tkeep_count = UInt(/*log2Up(dataWidth/8 + 1) bits*/)
  x_tkeep_count := RegNextWhen(w_tkeep_count, x.ready)

  val was_last = RegNextWhen(x.last, x.fire)
  val previous_beat_was_not_last = RegNext(x.fire & !x.last)

  // update frame length of current packet on x
  val frame_length = Reg(UInt(12 bits)) init(0)
  when (x.fire) {
    // first, but not last data beat?
    when (!x_is_frame_continuation & !x.last) {
      frame_length := keepWidth
    // first and last data beat?
    } elsewhen (!x_is_frame_continuation & x.last) {
      frame_length := x_tkeep_count.resize(12 bits)
    // non-first, non-last data beat?
    } elsewhen (x_is_frame_continuation & !x.last) {
      frame_length := frame_length + keepWidth
    // non-first, last data beat
    } elsewhen (x_is_frame_continuation & x.last) {
      frame_length := frame_length + x_tkeep_count.resize(12 bits)
    }
  }

  // the frame_length is max_words-1 due to the previous (non-last) beat (length result has 1 cycle latency)
  // and the current beat is an intermediate beat (resulting in maximum frame length)
  // thus the next beat will make the frame oversized
  val frame_going_oversize_event = (frame_length === (maxFrameBytes - keepWidth)) & !was_last & x_is_intermediate_beat
  
  // frame_too_large will go high on the last beat that fits in max_words
  // it indicates the frame has been truncated, and the remainder should be ignored
  val frame_too_large = Reg(Bool()) init (False)
  when (x_is_first_beat) {
    frame_too_large := False
  } elsewhen (frame_going_oversize_event) {
    frame_too_large := True 
  }

  // if any beat has tkeep(0) de-asserted, set the drop bit in the length FIFO
  // or if any beat has tuser(0) asserted, set the drop bit in the length FIFO
  val drop_this_frame = Reg(Bool()) //init (False)
  // when (x_is_first_beat) {
  when (x.firstFire) {
    drop_this_frame := x.fragment.tuser(0) | (!x.fragment.tkeep(0))
  } elsewhen (x.fire) {
    drop_this_frame := drop_this_frame | x.fragment.tuser(0) | (!x.fragment.tkeep(0))
  }

  // worst-case, a packet is a single beat, so this FIFO must be at least the size of
  // the data stream FIFO, plus extra 
  val length_fifo = new StreamFifo(UInt(12 bits), fifoSize + 4/*@TODO does this match registers? */)

  val push_length_on_last = RegNext(x_is_last_beat & !frame_too_large) init(False)
  length_fifo.io.push.valid := push_length_on_last | frame_going_oversize_event
  // bit 11 in the length FIFO indicates the frame must be dropped
  length_fifo.io.push.payload := frame_length | (frame_going_oversize_event.asUInt << 11) |
    (drop_this_frame.asUInt << 11)
  val length_pop = Bool()
  /* pop length from length FIFO on last packet word */
  length_pop := z.last & z.fire
  length_fifo.io.pop.ready := length_pop;

  // do not push data beyond truncation */
  x2.valid := x.valid & (!frame_too_large | frame_going_oversize_event)
  x2.payload.tuser := x.payload.tuser
  // clear out unused bytes to zero in x2
  x2.payload.tdata := 0
  // copy valid bytes from x into x2
  for (i <- 0 until keepWidth) {
    when (x2.payload.tkeep(i)) {
      x2.payload.tdata(i*8 + 7 downto i*8).assignFromBits(x.payload.tdata(i*8 + 7 downto i*8))
    }
    //.otherwise {
    //  x2.payload.tdata(i*8 + 7 downto i*8) := 0
    //}
  }
  //x2.payload.tdata := x.payload.tdata
  x2.payload.tkeep := x.payload.tkeep
  // @TODO is frame_going_oversize_event a pulse? then coincide with valid
  x2.last := (x.last & !frame_too_large) | frame_going_oversize_event
  x.ready := x2.ready
  /* one cycle latency to match length calculation, to ensure the frame
   * length is available together with the frame data on the FIFO outputs */
  x3 << x2.s2mPipe().m2sPipe()
  fifo.io.push << x3
  y << fifo.io.pop
  // highest bit indicates truncated packet or dropped packet
  val drop_on_truncate = ((length_fifo.io.pop.payload & U(0x800)) === U(0x800)) // length_fifo.io.pop.payload >= 0x800
  io.source << z.throwWhen(drop_on_truncate) //.haltWhen(!io.length.ready)

  // drive length in parallel to packet, unless packet dropped
  io.length := length_fifo.io.pop.payload & U(0x7FF)
  io.length_valid := length_fifo.io.pop.valid & !drop_on_truncate

  val diff = (io.source.valid =/= length_fifo.io.pop.valid)
  val missing = (io.source.valid & !length_fifo.io.pop.valid)

  io.packets := packetsInFifoCounter.value

  // Rename SpinalHDL library defaults to AXI naming convention
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))

  // formal verification
  GenerationFlags.formal {

    assumeInitial(clockDomain.isResetActive)

    // assert, assume and cover are only active during clocks
    assert(
      //assertion = !(io.source.valid & !length_fifo.io.pop.valid),
      assertion = !(io.source.valid & !io.length_valid),
      message   = "Frame length not available during frame data valid",
      severity  = ERROR
    )
    assert(
      assertion = !(length_fifo.io.push.valid & !length_fifo.io.push.ready),
      message   = "Pushing length into Length FIFO, but FIFO is not ready",
      severity  = ERROR
    )
    when (pastValidAfterReset) {
      assert(
        assertion = (
          (past(length_fifo.io.occupancy) === length_fifo.io.occupancy) |
          (past(length_fifo.io.occupancy) === length_fifo.io.occupancy - 1) |
          (past(length_fifo.io.occupancy) === length_fifo.io.occupancy + 1)
        ),
        message   = "Length FIFO occupancy should change by at most 1",
        severity  = ERROR
      )
      assert(
        assertion = (
          (past(fifo.io.occupancy) === fifo.io.occupancy) |
          (past(fifo.io.occupancy) === fifo.io.occupancy - 1) |
          (past(fifo.io.occupancy) === fifo.io.occupancy + 1)
        ),
        message   = "Frame FIFO occupancy should change by at most 1",
        severity  = ERROR
      )
      assert(
        assertion = (
          !( (past(fifo.io.occupancy === fifoSize, 1) & past(io.packets === 0, 1)) &&
            (past(fifo.io.occupancy === fifoSize, 2) & past(io.packets === 0, 2))
          )
        ),
        message   = "Frame FIFO full but holds no complete frame.",
        severity  = ERROR
      )
    }

    assert(
      assertion = !(frame_too_large & frame_going_oversize_event),
      message = "Truncated last beat and truncated tail do not overlap",
      severity  = ERROR
    )
    assert(
      assertion = (!io.length_valid | (io.length <= maxFrameBytes)),
      message = "Passed frame lengths are within maximum length bound.",
      severity  = ERROR
    )
    assert(
      assertion = (!io.source.valid | (!io.source.fragment.tuser(0))),
      message = "All packets with any tuser(0) asserted are dropped on out.",
      severity  = ERROR
    )    
    val formal_frame_length = Reg(UInt(12 bits)) init(0)
    val formal_is_frame_continuation = RegNextWhen(!io.sink.last, io.sink.fire) init(False)
    val formal_is_first_beat = io.sink.fire & !formal_is_frame_continuation
    when (io.sink.fire) {
      // first, but not last data beat?
      when (formal_is_first_beat) {
        formal_frame_length := 1
      } otherwise {
        formal_frame_length := formal_frame_length + 1
      }
    }
    //assume(
    //  ((formal_frame_length === (fifoSize - 1)) & ((!io.sink.valid | !io.sink.ready) | !io.sink.last)) |
    //  (formal_frame_length < (fifoSize - 1))
    //)
    assume(io.sink.tdata === 0x01)
    cover(io.packets === fifoSize)
    cover(io.packets === 0)
    cover(io.packets === 1)
    cover(fifo.io.occupancy === fifoSize)
    cover(fifo_holds_complete_packet)
    cover(frame_going_oversize_event)
    cover(frame_too_large)
    cover(push_length_on_last)
    cover(io.length_valid)
    cover(io.source.valid)
    cover(drop_on_truncate)
    cover(x_is_first_beat)
    cover(x_is_last_beat)
    cover(x_is_intermediate_beat)
    // leave following state commented out, it cannot be reached, expect failure when uncommented
    //cover((io.length === 0x7FF) & (io.length_valid))
  }
}

// companion object
object CorundumFrameFlowStash {
}

// Stash that never de-asserts sink_ready during a packet on sink
// in other words: does not apply backpressure during a packet on sink
// assuming packets are not bigger than maxPacketFifoWords
case class CorundumFrameFlowStash(dataWidth : Int, fifoSize : Int, maxPacketFifoWords: Int) extends Component {
  val keepWidth = dataWidth / 8
  val maxFrameBytes = fifoSize * keepWidth
  val io = new Bundle {
    val sink = slave Stream new Fragment(CorundumFrame(dataWidth))
    val source = master Stream new Fragment(CorundumFrame(dataWidth))

    // worst case each packet is one beat
    val packets = out UInt(log2Up(fifoSize * 2) bits)
    val length = out UInt(12 bits)
    val length_valid = out Bool()
    val availability = out UInt()
  }
  val stash = CorundumFrameStash(dataWidth, fifoSize)

  val x = Stream(Fragment(CorundumFrame(dataWidth)))
  
  x << io.sink

  // { io.sink.ready never de-asserts during the reception of a packet }
  // { fifo_too_full never    asserts during the reception of a packet }
  val fifo_too_full = Reg(Bool()) init (False)
  // enough space for full packet?
  when (stash.io.availability > maxPacketFifoWords) {
    fifo_too_full := False
  // just received a full packet?
  } elsewhen (x.lastFire) {
    // update only now
    fifo_too_full := stash.io.availability <= maxPacketFifoWords
  }
  // .haltWhen deasserts READY on condition
  stash.io.sink << x.haltWhen(fifo_too_full)

  io.source << stash.io.source

  io.packets := stash.io.packets
  io.length := stash.io.length
  io.length_valid := stash.io.length_valid
  io.availability := stash.io.availability

  // Rename SpinalHDL library defaults to AXI naming convention
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))

  // formal verification
  GenerationFlags.formal {
    import spinal.core.GenerationFlags._
    import spinal.core.formal._

    assumeInitial(clockDomain.isResetActive)

    // assert, assume and cover are only active during clocks
  } 
}
