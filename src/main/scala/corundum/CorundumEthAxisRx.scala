package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumEthAxisRx {
}

/* goal:
 * split fixed size (14 bytes) Ethernet header in the first bytes, pass the remaining payload
 * stream sink to payload as simple as possible
 * length is the input packet length in bytes, this packet arrives on the sink
 * source is the output packet (Ethernet payload)
 */

case class CorundumEthAxisRx(dataWidth : Int, headerWidthBytes: Int) extends Component {
  val headerWidth = headerWidthBytes * 8
  // currently only single beat headers are supported to be stripped off
  assert(headerWidth <= dataWidth)
  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidth bits)))
    val source = master Stream(Fragment(Bits(dataWidth bits)))
    val header = out Bits(headerWidth bits)
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
  }
  val x = Stream(Fragment(Bits(dataWidth bits)))

  // skid buffers the inputs, 1 clock latency
  x << io.sink.m2sPipe().s2mPipe()
  val x_length = RegNext(io.sink_length)

  // extract header at first beat
  val x_is_frame_continuation = RegNextWhen(!x.last, x.fire).init(False)
  val x_is_first_beat = x.fire & !x_is_frame_continuation
  val x_header = RegNextWhen(x.payload.resize(headerWidth), x_is_first_beat)

  val remaining_payload_length = Reg(SInt(13 bits))
  when (x_is_first_beat) {
    remaining_payload_length := x_length.asSInt.resize(13 bits) - headerWidthBytes
  // can be removed, only for clearity during development cycle
  } elsewhen (x.last) {
    remaining_payload_length := 0
  } elsewhen (x_is_frame_continuation && x.valid) {
    remaining_payload_length := remaining_payload_length - dataWidth / 8
  } otherwise {
    remaining_payload_length := remaining_payload_length
  }
// for production use this
//  } otherwise /* { not first beat } */ {
//    remaining_payload_length := remaining_payload_length - dataWidth / 8

  // y holds only valid x.payload but one clock delayed
  val y = RegNextWhen(x.payload, x.valid)

  val z = Stream(Fragment(Bits(dataWidth bits)))

  // { x holds most recent word (if any) }
  // { y holds previous valid word, if any }

  // take all but header from y, put in lsbs
  // take header sized part from x, put in msbs
  //z <-< x
  //  .toStreamOfFragment
    //.translateWith(x.payload.fragment(headerWidth - 1 downto  0) ## y(dataWidth - 1 downto headerWidth))
    //.swapPayload(x.payload.fragment(headerWidth - 1 downto  0) ## y(dataWidth - 1 downto headerWidth))
   // .transmuteWith(U(0, dataWidth bits))
  z.payload.fragment := RegNext(x.payload.fragment(headerWidth - 1 downto  0) ## y(dataWidth - 1 downto headerWidth))
  z.payload.last := RegNext((remaining_payload_length > 0) && (remaining_payload_length <= (dataWidth/8)))
  z.valid := RegNext((x.valid && x.last))
  x.ready := z.ready

    // drive payload
    //.translateWith(x.payload(headerWidth - 1 downto  0) ## y.payload(dataWidth - 1 downto headerWidth))
    // drive active
    //.throwWhen(remaining_payload_length <= 0)
  // drive last
  //io.source <-< z.addFragmentLast((remaining_payload_length > 0) && (remaining_payload_length <= (dataWidth/8)))
  io.source << z
  io.source_length := RegNext(Mux(remaining_payload_length < 0, U(0), remaining_payload_length.asUInt.resize(12)))
  io.header := x_header
}

//  y.ready := io.payload.ready
//  y.payload := RegNextWhen(x.payload, io.payload.ready)
//  y.payload.last := RegNextWhen(x.payload.last, io.payload.ready)
//  y.valid := RegNextWhen(x.valid, io.payload.ready)


//Generate the CorundumEthAxisRx's Verilog
object CorundumEthAxisRxVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new CorundumEthAxisRx(512, 14 * 8))
  }
}
