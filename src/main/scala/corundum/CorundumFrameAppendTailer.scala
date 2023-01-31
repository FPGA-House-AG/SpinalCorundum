package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameAppendTailer {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameAppendTailer(Config.corundumDataWidth, 14))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameAppendTailer(Config.corundumDataWidth, 14))
  }
}

// Appends bytes to the end of the packet, with undefined data
// Intended to add a checksum field that is not checked (or overwritten by another module)
case class CorundumFrameAppendTailer(dataWidth : Int, tailerWidthBytes : Int) extends Component {

  val tailerWidth = tailerWidthBytes * 8

  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    //val tailer = in Bits(tailerWidth bits)
  }

  // currently only single beat tailers are supported
  require(tailerWidth <= dataWidth, s"tailerWidth <= dataWidth, needed because CorundumFrameAppendTailer does not support multibeat tailers yet.")

  // 2 skid buffer
  val x = Stream Fragment(CorundumFrame(dataWidth))
  x << io.sink.s2mPipe().m2sPipe()

  // remember the part that overflows on shift left by tailer width
  val tkeep_buffer = RegNextWhen(x.payload.tkeep.resizeLeft(tailerWidth/8), x.fire)

  val y = Stream Fragment(CorundumFrame(dataWidth))

  val have_extra_beat = x.payload.tkeep(dataWidth/8 - tailerWidth/8) && x.lastFire
  val in_extra_beat = RegInit(False) setWhen(have_extra_beat) clearWhen(y.lastFire)

  val all_tkeep   = x.payload.tkeep(dataWidth/8 - tailerWidth/8 - 1 downto 0) ## B(tailerWidth/8 bits, default -> True)
  val y_tkeep = Mux(in_extra_beat, tkeep_buffer.resize(dataWidth/8), all_tkeep)

  val y_is_last = Mux(have_extra_beat | in_extra_beat, in_extra_beat, x.lastFire)

  y.payload.fragment.tdata := x.payload.tdata
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
  io.source << y

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
