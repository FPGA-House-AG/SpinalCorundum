package corundum

import spinal.core._
import spinal.lib._

import scala.util.Random

// companion object
object CorundumFrameMuxPrio {
}

import corundum.CorundumFrameMuxPrio._

//val source = Stream(RGB(8))
//val sink   = Stream(RGB(8))
//sink <-< source

//Hardware definition

// multiplexes two packet streams (Stream(Fragment) with lock), first port has priority
case class CorundumFrameMuxPrio(dataWidth : Int = 8) extends Component {
  val io = new Bundle {
    val slave0 = slave Stream Fragment(CorundumFrame(dataWidth))
    val slave1 = slave Stream Fragment(CorundumFrame(dataWidth))
    val master0 = master Stream Fragment(CorundumFrame(dataWidth))
  }

  val arbiter = StreamArbiterFactory.lowerFirst.fragmentLock.build(Fragment(CorundumFrame(dataWidth)), 2)

  arbiter.io.inputs(0) << io.slave0.s2mPipe().m2sPipe()
  arbiter.io.inputs(1) << io.slave1.s2mPipe().m2sPipe()
  io.master0 << arbiter.io.output.s2mPipe().m2sPipe()
  noIoPrefix()
}

object FrameSpecRenamer{
  def apply[T <: Bundle with CorundumFrame](that : T): T ={
    def doIt = {
      that.flatten.foreach((bt) => {
        println(bt.getName())
        bt.setName(bt.getName().replace("_payload_",""))
        bt.setName(bt.getName().replace("_valid","valid"))
        bt.setName(bt.getName().replace("_ready","ready"))
        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
      })
    }
    if(Component.current == that.component)
      that.component.addPrePopTask(() => {doIt})
    else
      doIt

    that
  }
}

// https://gitter.im/SpinalHDL/SpinalHDL?at=5c2297c28d31aa78b1f8c969
object XilinxPatch {
  def apply[T <: Component](c : T) : T = {
    //Get the io bundle via java reflection
    val m = c.getClass.getMethod("io")
    val io = m.invoke(c).asInstanceOf[Bundle]
    println("getClass %s", m);

    //Patch things
    io.elements.map(_._2).foreach {
      //case axi : AxiLite4 => AxiLite4SpecRenamer(axi)
      //case axi : Axi4 => Axi4SpecRenamer(axi)
      case axi : CorundumFrame => FrameSpecRenamer(axi)
      case _ => println("unknown")
    }

    //Builder pattern return the input argument
    c 
  }
}

//Generate the CorundumFrameMuxPrio's Verilog
object CorundumFrameMuxPrioVerilog {
//  def main(args: Array[String]) {
//    SpinalVerilog(new CorundumFrameMuxPrio)
//  }
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameMuxPrio(8)
      XilinxPatch(toplevel)
    })
  }
}

//Generate the CorundumFrameMuxPrio's VHDL
object CorundumFrameMuxPrioVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameMuxPrio(8))
  }
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the CorundumFrameMuxPrio's Verilog using the above custom configuration.
object CorundumFrameMuxPrioVerilogWithCustomConfig {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new CorundumFrameMuxPrio(8))
  }
}
