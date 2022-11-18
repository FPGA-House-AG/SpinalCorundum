package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameDrop {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    SpinalVerilog(new CorundumFrameDrop(512))
    SpinalVhdl(new CorundumFrameDrop(512))
  }
}

case class CorundumFrameDrop(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val drop = in Bool()
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

  // x is sink, but adds the sink_length as stream payload
  // such that both sink and sink_length are skid buffered
  val x = Stream Fragment(CorundumFrame(dataWidth))
  x << io.sink
  when (io.drop) {
    x.payload.fragment.tuser(0) := True
  }
   
  val y = Stream Fragment(CorundumFrame(dataWidth))
  y << x.s2mPipe().m2sPipe()
  val y_drop = y.payload.fragment.tuser(0)

  // capture the drop flag on first
  val drop_y_packet = RegNextWhen(y_drop, y.isFirst, False)

  // component sink/slave port to fifo push/sink/slave port
  val z = Stream Fragment(CorundumFrame(dataWidth))
  // drop packet conditionally
  z << y.throwWhen(drop_y_packet)
  io.source << z

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
