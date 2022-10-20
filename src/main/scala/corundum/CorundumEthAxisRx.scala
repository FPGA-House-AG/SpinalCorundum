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
  require(headerWidth <= dataWidth, s"CorundumEthAxisRx not support multibeat headers")
  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidth bits)))
    val source = master Stream(Fragment(Bits(dataWidth bits)))
    val header = out Bits(headerWidth bits)
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
    val source_remaining = out UInt(12 bits)
  }
  val x = Stream(Fragment(Bits(dataWidth bits)))

  // skid buffers the inputs, 1 clock latency
  x << io.sink.m2sPipe().s2mPipe()
  val x_length = RegNext(io.sink_length)

  // extract header at first beat
  val x_is_frame_continuation = RegNextWhen(!x.last, x.fire).init(False)
  val x_is_first_beat = x.fire & !x_is_frame_continuation
  val x_header = RegNextWhen(x.payload.resize(headerWidth), x_is_first_beat)
  val x_is_single_beat = x_is_first_beat && x.last

  val remaining = Reg(SInt(13 bits))
  val source_payload_length = Reg(SInt(13 bits))

// for production use this
//  } otherwise /* { not first beat } */ {
//    remaining := remaining - dataWidth / 8

  val z = Stream(Fragment(Bits(dataWidth bits)))

  val y_valid = Reg(Bool).init(False)
  val y_last = RegNextWhen(x.last, x.fire)
  // y holds previous valid x.payload, but only if x is non-last
  val y = RegNextWhen(x.payload |>> headerWidth, x.fire)
  val y_is_single_beat = RegNextWhen(x_is_single_beat, x.fire)


  when (x_is_first_beat) {
    remaining := x_length.asSInt.resize(13 bits) - headerWidthBytes
    source_payload_length := x_length.asSInt.resize(13 bits) - headerWidthBytes
  // can be removed, only for clearity during development cycle
  } elsewhen (y_last & z.fire) {
    remaining := 0
  } elsewhen (z.fire) {
    remaining := remaining - dataWidth / 8
  } otherwise {
    remaining := remaining
  }

  // determine when y becomes valid or invalid

  // x is valid last word, last word for z comes from x and y combined
  when (x.valid & x.last & (remaining <= 4)) {
    y_valid := False
  } elsewhen (x.fire & !z.fire) {
    y_valid := x.valid
  // z takes x, no new x
  } elsewhen (z.fire & !x.fire) {
    y_valid := False
  }

  val y_has_last_data = y_valid & y_last
  val x_has_last_data = x.valid & x.last


  z.payload.last := y_has_last_data | x_has_last_data
  //z.valid := (y_valid & z.payload.last) | (x.valid & y_valid)
  z.valid := (y_valid & z.payload.last) | (x.valid & y_valid)
  //z.valid := (y_valid & y_is_single_beat) | (x.valid & y_valid)
  z.payload.fragment := Mux(z.valid, x.payload.fragment(headerWidth - 1 downto  0) ## y(dataWidth - headerWidth - 1 downto 0), B(0))
  // z holds valid word when y is a single beat, or when we can combine x with a non-last y
  x.ready := z.ready

    // drive payload
    //.translateWith(x.payload(headerWidth - 1 downto  0) ## y.payload(dataWidth - 1 downto headerWidth))
    // drive active
    //.throwWhen(remaining <= 0)
  // drive last
  //io.source <-< z.addFragmentLast((remaining > 0) && (remaining <= (dataWidth/8)))
  io.source <-< z
  io.source_length := RegNext(Mux(source_payload_length < 0, U(0), source_payload_length.asUInt.resize(12)))
  io.source_remaining := RegNext(Mux(remaining < 0, U(0), remaining.asUInt.resize(12)))
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
