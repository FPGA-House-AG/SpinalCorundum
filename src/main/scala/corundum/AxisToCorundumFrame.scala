package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object for case class
object AxisToCorundumFrame {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new AxisToCorundumFrame(Config.corundumWidth))
    val verilogReport = Config.spinal.generateVerilog(new AxisToCorundumFrame(Config.corundumWidth))
  }
}

/* Converts from TDATA+length to Corundum TDATA+TKEEP */
case class AxisToCorundumFrame(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink = slave Stream Fragment(Bits(dataWidth bits))
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    val sink_drop = in Bool()
    val source = master Stream Fragment(CorundumFrame(dataWidth))
  }
  val tkeep_width = dataWidth / 8

  // xx is sink, but adds the sink_length to the stream payload
  val xx = Stream(Fragment(Bits(dataWidth + 12 + 1 bits)))
  val ff = Fragment(io.sink_drop ## io.sink_length.asBits ## io.sink.payload.fragment)
  ff.fragment := io.sink_drop ## io.sink_length.asBits ## io.sink.payload.fragment
  ff.last := io.sink.payload.last
  // such that both data and length go through a pipelined skid buffer (or elastic buffer)
  xx << io.sink.translateWith(ff).s2mPipe().m2sPipe()
  
  // x is the original stream after the skid buffer
  val x = Stream Fragment(Bits(dataWidth bits))
  // translate payload of x back to data bits only
  val fff = Fragment(Bits(dataWidth bits))
  fff.last := xx.payload.last
  fff.fragment := xx.payload.fragment.resize(dataWidth)
  x << xx.translateWith(fff)
  // and extract the length
  val x_length = (xx.payload.fragment >> dataWidth).resize(12).asUInt
  val x_drop = (xx.payload.fragment >> dataWidth)(12)

  // y will create a CorundumFrame from x
  val y = Stream Fragment(CorundumFrame(dataWidth))
  y.payload.fragment.tdata := x.payload.fragment
  // can be in range [0, tkeep_width] but if last then should be in range [1, tkeep_width]
  val x_last_enabled_bytes = x_length % tkeep_width + ((x_length % tkeep_width) === 0).asUInt * tkeep_width
  val x_last = x.payload.last
  // { valid & last => at least one active byte }
  assert((!x.valid || !x_last) || (x_last_enabled_bytes > 0))
  // @TODO we could also do this with a shift register (with [0-64] possible shifts), check what's optimal
  for (i <- 0 until tkeep_width) {
    y.payload.fragment.tkeep(i) := !x_last | (x_last_enabled_bytes > i)
  }
  //y.payload.fragment.tkeep(tkeep_width - 1) := !x_last | (x_length.resize(log2Up(tkeep_width) + 1) > (tkeep_width - 1))

  // put drop flag into tuser(0)
  y.payload.fragment.tuser := 0
  when (True) { y.payload.fragment.tuser(0) := x_drop }
  y.payload.last := x.payload.last
  y.valid := x.valid
  x.ready := y.ready

  io.source <-< y

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
