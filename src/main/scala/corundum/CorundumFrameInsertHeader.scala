package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameInsertHeader {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameInsertHeader(Config.corundumDataWidth, 1, 14))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameInsertHeader(Config.corundumDataWidth, 1, 14))
  }
}

case class CorundumFrameInsertHeader(dataWidth : Int, userWidth : Int, headerWidthBytes : Int) extends Component {

  val headerWidth = headerWidthBytes * 8

  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth, userWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth, userWidth))
    val header = in Bits(headerWidth bits)
  }

  // currently only single beat headers are supported to be stripped off
  require(headerWidth <= dataWidth, s"headerWidth <= dataWidth, needed because CorundumFrameInsertHeader does not support multibeat headers yet.")

  // 2 skid buffer
  val x = Stream Fragment(CorundumFrame(dataWidth, userWidth))
  x << io.sink.s2mPipe().m2sPipe()

  // remember the part that overflows on shift left by header width
  val tdata_buffer = RegNextWhen(x.payload.tdata.resizeLeft(headerWidth), x.fire)
  val tkeep_buffer = RegNextWhen(x.payload.tkeep.resizeLeft(headerWidth/8), x.fire)

  val y = Stream Fragment(CorundumFrame(dataWidth, userWidth))

  val have_extra_beat = x.payload.tkeep(dataWidth/8 - headerWidth/8) && x.lastFire
  val in_extra_beat = RegInit(False) setWhen(have_extra_beat) clearWhen(y.lastFire)

  val header_payload = x.payload.tdata(dataWidth - headerWidth - 1 downto 0) ## io.header
  val data_payload = x.payload.tdata(dataWidth - headerWidth - 1 downto 0) ## tdata_buffer
  val y_payload = Mux(y.isFirst, header_payload, data_payload)

  val all_tkeep   = x.payload.tkeep(dataWidth/8 - headerWidth/8 - 1 downto 0) ## B(headerWidth/8 bits, default -> True)
  val y_tkeep = Mux(in_extra_beat, tkeep_buffer.resize(dataWidth/8), all_tkeep)

  val y_is_last = Mux(have_extra_beat | in_extra_beat, in_extra_beat, x.lastFire)

  y.payload.fragment.tdata := y_payload
  y.payload.fragment.tkeep := y_tkeep
  y.payload.fragment.tuser := x.payload.tuser
  y.payload.last :=     y_is_last
  // y is also valid during the (possibly) newly added last beat
  y.valid := x.valid |  y_is_last

  x.ready := y.ready
  when (in_extra_beat) {
    x.ready := False
  }

  // @TODO dual pipeline skid buffer for Fmax
  io.source <-< y

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
