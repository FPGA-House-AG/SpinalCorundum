package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object for case class
object LinearFeedbackShiftRegister {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new LinearFeedbackShiftRegister(32))
    val verilogReport = Config.spinal.generateVerilog(new LinearFeedbackShiftRegister(32))
  }
}

case class LinearFeedbackShiftRegister(dataWidth : Int) extends Component {
  require(dataWidth == 32, "dataWidth == 32 currently only supported")
  val io = new Bundle {
    val enable = in Bool()
    val lfsr = out Bits(dataWidth bits)
  }

  val lfsr_reg = Reg(Bits(dataWidth bits)) init(0)
  //w_XNOR <= r_LFSR(32) xnor r_LFSR(22) xnor r_LFSR(2) xnor r_LFSR(1);
  val xnor = Bool()
  // even #operands => use ~
  // see bug in Verilog: http://fpgacpu.ca/fpga/Bit_Reducer.html
  xnor := ~(lfsr_reg(31) ^ lfsr_reg(21) ^ lfsr_reg(1) ^ lfsr_reg(0))

  //r_LFSR <= r_LFSR(r_LFSR'left-1 downto 1) & w_XNOR;
  when (io.enable) {
    // shift left losing msb, add XNOR feedback on bit #0
    lfsr_reg := lfsr_reg(30 downto 0) ## xnor
  }

  io.lfsr := lfsr_reg

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
