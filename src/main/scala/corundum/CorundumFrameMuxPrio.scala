package corundum

import spinal.core._
import spinal.lib._

import scala.util.Random

// companion object
object CorundumFrameMuxPrio {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameMuxPrio(Config.corundumDataWidth))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameMuxPrio(Config.corundumDataWidth))
  }
}

import corundum.CorundumFrameMuxPrio._

// multiplexes two packet streams (Stream(Fragment) with lock), first port has priority
case class CorundumFrameMuxPrio(dataWidth : Int) extends Component {
  val io = new Bundle {
    val slave0 = slave Stream Fragment(CorundumFrame(dataWidth))
    val slave1 = slave Stream Fragment(CorundumFrame(dataWidth))
    val master0 = master Stream Fragment(CorundumFrame(dataWidth))
  }

  val arbiter = StreamArbiterFactory.lowerFirst.fragmentLock.build(Fragment(CorundumFrame(dataWidth)), 2)

  arbiter.io.inputs(0) << io.slave0.s2mPipe().m2sPipe()
  arbiter.io.inputs(1) << io.slave1.s2mPipe().m2sPipe()
  io.master0 << arbiter.io.output.s2mPipe().m2sPipe()

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
