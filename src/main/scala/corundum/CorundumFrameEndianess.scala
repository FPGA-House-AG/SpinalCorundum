package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameEndianess {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameEndianess(Config.corundumDataWidth))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameEndianess(Config.corundumDataWidth))
  }
}

case class CorundumFrameEndianess(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
  }
  val num_bytes = dataWidth / 8

  val x = Stream Fragment(CorundumFrame(dataWidth))
  x << io.sink

  // reverse endianess in tdata and tkeep
  when (True) {
    x.payload.fragment.tdata.assignFromBits(io.sink.payload.fragment.tdata.asBits.subdivideIn(8 bits).reverse.asBits)
    x.payload.fragment.tkeep.assignFromBits(io.sink.payload.fragment.tkeep.asBits.subdivideIn(8 bits).reverse.asBits)
  }
  io.source << x

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
