package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object AxisInsertHeader {
  def main(args: Array[String]) {
    SpinalVhdl(new AxisInsertHeader(128, 14/*Ethernet header size in bytes*/))
    SpinalVerilog(new AxisInsertHeader(128, 14/*Ethernet header size in bytes*/))
  }
}

/* Split off a fixed size header (for example 14 bytes Ethernet header in the first bytes), pass the remaining payload
 *
 * sink accepts AXIS frames (Ethernet packet)
 * sink_length is the input packet length in bytes, this packet arrives on the sink
 *
 * source is the output packet (Ethernet payload)
 * source_length is the output packet (Ethernet payload)
 */

case class AxisInsertHeader(dataWidth : Int, headerWidthBytes: Int) extends Component {
  val headerWidth = headerWidthBytes * 8
  // currently only single beat headers are supported to be stripped off
  require(headerWidth <= dataWidth, s"headerWidth <= dataWidth, needed because AxisInsertHeader does not support multibeat headers yet.")
  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidth bits)))
    val source = master Stream(Fragment(Bits(dataWidth bits)))
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)

    val header = in Bits(headerWidth bits)
    val source_length = out UInt(12 bits)
  }

  // translateWith() for Stream(Fragment())
  // (before this we needed to work-around this, see AxisUpSizer.scala commented out code)
  implicit class FragmentPimper[T <: Data](v: Fragment[T]) {
    def ~~[T2 <: Data](trans: T => T2) = {
      val that = trans(v.fragment)
      val res = Fragment(cloneOf(that))
      res.fragment := trans(v.fragment)
      res.last := v.last
      res
    }
  }

  // x1 is sink, but adds the sink_length as stream payload
  // such that both sink and sink_length are skid buffered
  val x1 = Stream(Fragment(Bits(dataWidth + 12 bits)))
  x1 << io.sink.~~(_.~~(io.sink_length.asBits ## _)).s2mPipe().m2sPipe()
   
  // y is input stream with original payload, but after the skid buffer
  val x = Stream(Fragment(Bits(dataWidth bits)))
  x << x1.~~(_.~~(_.resize(dataWidth)))
  val x_length = (x1.payload.fragment >> dataWidth).asUInt.resize(12)

  val buffer = RegNextWhen(x.payload.resizeLeft(headerWidth), x.fire)

  val y = Stream(Fragment(Bits(dataWidth bits)))

  val header_payload = x.payload(dataWidth - headerWidth - 1 downto 0) ## io.header
  val data_payload =   x.payload(dataWidth - headerWidth - 1 downto 0) ## buffer
  val y_payload = Mux(y.isFirst, header_payload, data_payload)
  val y_length = x_length + headerWidthBytes


  val dataWidthBytes = dataWidth/8
  val x_length_min = x_length.min(dataWidthBytes)
  val x_length_hdr = x_length.min(dataWidthBytes) + headerWidthBytes

  // only valid during x, whether we need an extra beat now that header is prefixed
  val have_extra_beat = (x_length.min(dataWidth/8) + headerWidthBytes) > (dataWidth / 8)
  // set after x.last fires, clears when y.last fires
  // is true if y has the extra (overflow) beat
  val in_extra_beat = RegInit(False) setWhen(x.lastFire && have_extra_beat) clearWhen(y.lastFire)

  // y last signal is either the original x.last or later beat
  // because have_extra_beat is only valid during x, we need to extend the Mux select
  // signal with the in_extra_beat
  val y_is_last = Mux(have_extra_beat | in_extra_beat, in_extra_beat, x.lastFire)

  y.payload.fragment := y_payload
  y.payload.last :=     y_is_last
  // y is also valid during the (possibly) newly added last beat
  y.valid := x.valid |  y_is_last

  io.source << y

  x.ready := y.ready
  when (in_extra_beat) {
    x.ready := False
  }

  io.source_length := y_length

   // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
