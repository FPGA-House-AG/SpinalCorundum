package corundum

import spinal.core._
import spinal.lib._

import scala.util.Random

// companion object
object CorundumFrameStash {
}

case class CorundumFrameStash(dataWidth : Int) extends Component {
  val maxFragmentWords = 16
  val minPackets = 1
  val fifoSize = minPackets * maxFragmentWords
  val io = new Bundle {
    val slave0 = slave Stream new Fragment(CorundumFrame(dataWidth))
    val master0 = master Stream new Fragment(CorundumFrame(dataWidth))
    // worst case each packet is one beat
    val packets = out UInt(log2Up(fifoSize * 2) bits)
    val length = out UInt(12 bits)
    val length_valid = out Bool()
    val full = out Bool()
    //println(log2Up(fifoSize))
  }
  val fifo = new StreamFifo(Fragment(CorundumFrame(dataWidth)), fifoSize)

    // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
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
  /*fifo.io.pop.ready & fifo.io.pop.valid & fifo.io.pop.last)*/
  when (y.ready & y.valid & y.last) {
    packetsInFifoCounter.decrement()
  }


  io.full := fifo.io.availability < 2

  val is_frame_continuation = RegNextWhen(!x.last, x.valid) init(False)
  val is_first_beat = x.valid & x.ready & !is_frame_continuation

  printf("%s log2Up(%d/8+1)=%d\n", sourcecode.File(), dataWidth, log2Up(dataWidth/8+1))

  // measure frame length in x
  val tkeep_count = UInt(/*log2Up(dataWidth/8 + 1) bits*/)
  tkeep_count := U(dataWidth/8) - LeadingZeroes(x.tkeep)

  val frame_length = Reg(UInt(12 bits))
  // first beat?
  when (x.valid & x.ready & !is_frame_continuation) {
    frame_length := tkeep_count.resize(12 bits)
  // non-first beat(s)
  /*} otherwise {*/
  } elsewhen (x.valid & x.ready) {
    frame_length := (frame_length + tkeep_count)/*.resize(12 bits)*/
  }
  val length_fifo = new StreamFifo(UInt(12 bits), fifoSize + 4/*@TODO does this match registers? */)
  length_fifo.io.push.valid := RegNext(x.last & x.ready & x.valid) init(False)
  length_fifo.io.push.payload := frame_length
  val length_pop = Bool()
  length_pop := z.last & z.ready & z.valid
  length_fifo.io.pop.ready := length_pop;

  // skid buffer between input and x
  // at least 1 clock cycle latency
  // but full throughput
  x << io.slave0.m2sPipe().s2mPipe()
  fifo.io.push << x.m2sPipe().s2mPipe()
  y << fifo.io.pop
  io.master0 << z

  io.length := length_fifo.io.pop.payload
  io.length_valid := length_fifo.io.pop.valid


  val diff = (io.master0.valid =/= length_fifo.io.pop.valid)
  val missing = (io.master0.valid & !length_fifo.io.pop.valid)

  io.packets := packetsInFifoCounter.value

  assert(
    assertion = !(io.master0.valid & !length_fifo.io.pop.valid),
    message   = "Frame length not available during frame data valid",
    severity  = ERROR
  )

  assert(
    assertion = !(length_fifo.io.push.valid & !length_fifo.io.push.ready),
    message   = "Pushing Length into Length FIFO, but FIFO is not ready",
    severity  = ERROR
  )

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
    config.generateVerilog({
      val toplevel = new CorundumFrameStash(512)
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
