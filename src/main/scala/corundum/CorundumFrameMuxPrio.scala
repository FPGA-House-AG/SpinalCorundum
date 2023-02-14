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

// multiplexes two packet streams (Stream(Fragment) with lock), first port has priority
case class CorundumFrameMuxPrio(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink0 = slave Stream Fragment(CorundumFrame(dataWidth))
    val sink1 = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
  }

  val arbiter = StreamArbiterFactory.lowerFirst.fragmentLock.build(Fragment(CorundumFrame(dataWidth)), 2)

  arbiter.io.inputs(0) << io.sink0.s2mPipe().m2sPipe()
  arbiter.io.inputs(1) << io.sink1.s2mPipe().m2sPipe()
  io.source <-< arbiter.io.output

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
