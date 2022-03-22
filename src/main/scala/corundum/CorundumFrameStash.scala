// CorundumFrameStash
// Passes frames through a FIFO to determine frame length.
// Frames are output together with their frame length on io.length and io.length_valid.
// When io.master0.valid, io.length_valid is also always valid.
// Oversized frames are dropped. 
// Oversized frames are internally marked, truncated to maximum FIFO length before entering the FIFO,
// and dropped after the FIFO.

package corundum

import spinal.core._
import spinal.lib._

import scala.util.Random

// companion object
object CorundumFrameStash {
}

case class CorundumFrameStash(dataWidth : Int) extends Component {
  val maxFragmentWords = 8
  val minPackets = 1
  val fifoSize = minPackets * maxFragmentWords
  val keepWidth = dataWidth/8
  val fmaxFrameBytes = maxFragmentWords * keepWidth
  val io = new Bundle {
    val slave0 = slave Stream new Fragment(CorundumFrame(dataWidth))
    val master0 = master Stream new Fragment(CorundumFrame(dataWidth))
    // worst case each packet is one beat
    val packets = out UInt(log2Up(fifoSize * 2) bits)
    val length = out UInt(12 bits)
    val length_valid = out Bool()
    //val full = out Bool()
    //println(log2Up(fifoSize))
  }
  val length_and_truncated = new Bundle {
    val length = UInt(12 bits)
    val truncated = Bool()
  }

  val fifo = new StreamFifo(Fragment(CorundumFrame(dataWidth)), fifoSize)

  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
  val x2 = Stream Fragment(CorundumFrame(dataWidth))
  val x3 = Stream Fragment(CorundumFrame(dataWidth))
  // fifo source/master/pop port to component source/master port
  val y = Stream Fragment(CorundumFrame(dataWidth))

  // track number of packets in the FIFO
  val packetsInFifoCounter = CounterUpDown(fifoSize * 2)

  // gather at least minPackets packet(s) in the FIFO before continuing the pop/output stream
  // however if the FIFO becomes full, also continue, to prevent corruption
  val fifo_holds_complete_packet = (packetsInFifoCounter.value >= minPackets)
  val z = y.continueWhen(fifo_holds_complete_packet).m2sPipe().s2mPipe()

  when (fifo.io.push.ready & fifo.io.push.valid & fifo.io.push.last) {
    packetsInFifoCounter.increment()
  }
  when (y.ready & y.valid & y.last) {
    packetsInFifoCounter.decrement()
  }

  val is_frame_continuation = RegNextWhen(!x.last, x.valid & x.ready) init(False)
  val is_first_beat = x.valid & x.ready & !is_frame_continuation
  val is_last_beat = x.valid & x.ready & x.last
  val is_intermediate_beat = x.valid & x.ready & is_frame_continuation & !x.last

  printf("%s log2Up(%d/8+1)=%d\n", sourcecode.File(), dataWidth, log2Up(dataWidth/8+1))

  // measure frame length in x
  val tkeep_count = UInt(/*log2Up(dataWidth/8 + 1) bits*/)
  tkeep_count := U(dataWidth/8) - LeadingZeroes(x.tkeep)

  val frame_too_large = Reg(Bool()) init (False)

  val was_last = RegNextWhen(x.last, x.valid & x.ready)
  val previous_beat_was_not_last = RegNext(x.valid & x.ready & !x.last)

  val frame_length = Reg(UInt(12 bits)) init(0)
  when (x.valid & x.ready) {
    // first, but not last data beat?
    when (/*x.valid & x.ready & */!is_frame_continuation & !x.last) {
      frame_length := keepWidth
    // first and last data beat?
    } elsewhen (/*x.valid & x.ready & */!is_frame_continuation & x.last) {
      frame_length := tkeep_count.resize(12 bits)
    // non-first, non-last data beat?
    } elsewhen (/*x.valid & x.ready & */is_frame_continuation & !x.last) {
      frame_length := frame_length + keepWidth
    // non-first, last data beat
    } elsewhen (/*x.valid & x.ready & */is_frame_continuation & x.last) {
      frame_length := frame_length + tkeep_count.resize(12 bits)
    }
  }

  // the frame_length is max_words-1 due to the previous (non-last) beat (length result has 1 cycle latency)
  // and the current beat is an intermediate beat (resulting in maximum frame length)
  // thus the next beat will make the frame oversized
  val frame_going_oversize_event = (frame_length === (fmaxFrameBytes - keepWidth)) & !was_last & is_intermediate_beat/* & !frame_too_large LEON*/
  //val frame_going_oversize_event = (frame_length === (fmaxFrameBytes - keepWidth)) & previous_beat_was_not_last

  // frame_too_large will go high on the last beat that fits in max_words
  // it indicates the frame has been truncated, and the remainder should be ignored
  when (is_first_beat/*x.valid & x.ready & !is_frame_continuation*/) {
    frame_too_large := False
  } elsewhen (/*!frame_too_large & */frame_going_oversize_event) {
    frame_too_large := True 
  }

  val length_fifo = new StreamFifo(UInt(12 bits), fifoSize + 4/*@TODO does this match registers? */)

  val push_length_on_last = RegNext(is_last_beat & !frame_too_large) init(False)
  length_fifo.io.push.valid := push_length_on_last | frame_going_oversize_event
  length_fifo.io.push.payload := frame_length | (frame_going_oversize_event.asUInt << 11)
  val length_pop = Bool()
  length_pop := z.last & z.ready & z.valid
  length_fifo.io.pop.ready := length_pop;

  // skid buffer between input and x
  // at least 1 clock cycle latency
  // but full throughput
  x << io.slave0.m2sPipe().s2mPipe()
  // do not push data beyond truncation */
  x2.valid := x.valid & (!frame_too_large | frame_going_oversize_event)
  x2.payload.tuser := x.payload.tuser
  x2.payload.tdata := x.payload.tdata
  x2.payload.tkeep := x.payload.tkeep
  x2.last := (x.last & !frame_too_large) | frame_going_oversize_event
  x.ready := x2.ready
  /* one cycle latency to match length calculation, to ensure the frame
   * length is available together with the frame data on the FIFO outputs */
  x3 << x2.m2sPipe().s2mPipe()
  fifo.io.push << x3
  y << fifo.io.pop
  // highest bit indicates truncated packet
  val drop_on_truncate = length_fifo.io.pop.payload >= 0x800
  io.master0 << z.throwWhen(drop_on_truncate)

  io.length := length_fifo.io.pop.payload & U(0x7FF)
  io.length_valid := length_fifo.io.pop.valid & !((length_fifo.io.pop.payload & U(0x800)) === U(0x800))

  val diff = (io.master0.valid =/= length_fifo.io.pop.valid)
  val missing = (io.master0.valid & !length_fifo.io.pop.valid)

  io.packets := packetsInFifoCounter.value

//  assert(
//    assertion = !(io.master0.valid & !length_fifo.io.pop.valid),
//    message   = "Frame length not available during frame data valid",
//    severity  = ERROR
//  )
//
//  assert(
//    assertion = !(length_fifo.io.push.valid & !length_fifo.io.push.ready),
//    message   = "Pushing length into Length FIFO, but FIFO is not ready",
//    severity  = ERROR
//  )

  import spinal.core.GenerationFlags._
  import spinal.core.Formal._

  GenerationFlags.formal {
    when(initstate()) {
      assume(clockDomain.isResetActive)
      assume(io.slave0.ready === False)
    }.otherwise {
      assert(
        assertion = !(io.master0.valid & !length_fifo.io.pop.valid),
        message   = "Frame length not available during frame data valid",
        severity  = ERROR
      )

      assert(
        assertion = !(length_fifo.io.push.valid & !length_fifo.io.push.ready),
        message   = "Pushing length into Length FIFO, but FIFO is not ready",
        severity  = ERROR
      )

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

      assert(
        assertion = !(frame_too_large & frame_going_oversize_event),
        message = "Truncated last beat and truncated tail do not overlap",
        severity  = ERROR
      )

      assert(
        assertion = (!io.length_valid | io.length <= fmaxFrameBytes),
        message = "Passed frame lengths are within maximum length bound.",
        severity  = ERROR
      )

      val formal_frame_length = Reg(UInt(12 bits)) init(0)
      val formal_is_frame_continuation = RegNextWhen(!io.slave0.last, io.slave0.valid & io.slave0.ready) init(False)
      val formal_is_first_beat = io.slave0.valid & io.slave0.ready & !formal_is_frame_continuation
      when (io.slave0.valid & io.slave0.ready) {
        // first, but not last data beat?
        when (formal_is_first_beat) {
          formal_frame_length := 1
        } otherwise {
          formal_frame_length := formal_frame_length + 1
        }
      }
      //assume(
      //  ((formal_frame_length === (fifoSize - 1)) & ((!io.slave0.valid | !io.slave0.ready) | !io.slave0.last)) |
      //  (formal_frame_length < (fifoSize - 1))
      //)
      assume(io.slave0.tdata === 0x01)
      cover(io.packets === fifoSize)
      cover(io.packets === 0)
      cover(io.packets === minPackets)
      cover(fifo.io.occupancy === fifoSize)
      cover(fifo_holds_complete_packet)
      cover(frame_going_oversize_event)
      cover(frame_too_large)
      cover(push_length_on_last)
      cover(io.length_valid)
      cover(io.master0.valid)
      cover(drop_on_truncate)
      cover(is_first_beat)
      cover(is_last_beat)
      cover(is_intermediate_beat)
    }
  }
}

// @todo PacketStream FIFO using CounterUpDown(0, in.last & in.fire, out.last & out.fire)
// out.valid := (counter > 0)

//Generate the CorundumFrameStash's Verilog
object CorundumFrameStashVerilog {
//  def main(args: Array[String]) {
//    SpinalVerilog(new CorundumFrameStash)
//  }
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameStash(512)
      XilinxPatch(toplevel)
    })
  }
}
//Generate the CorundumFrameStashSystemVerilogWithFormal's Verilog
object CorundumFrameStashSystemVerilogWithFormal {
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.includeFormal.generateSystemVerilog({
      val toplevel = new CorundumFrameStash(16)
      XilinxPatch(toplevel)
    })
  }
}

//Generate the CorundumFrameStash's VHDL
object CorundumFrameStashVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameStash(512))
  }
}
