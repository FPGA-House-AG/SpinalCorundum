package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object for case class
object AxisDownSizer {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new AxisDownSizer(Config.corundumDataWidth, Config.cryptoDataWidth))
    val verilogReport = Config.spinal.generateVerilog(new AxisDownSizer(Config.corundumDataWidth, Config.cryptoDataWidth))
  }
}

/* Down size the AXIS data width with a factor of 2 or 4
 *
 * sink accepts AXIS frames (Ethernet packet)
 * sink_length is the input packet length in bytes, this packet arrives on the sink
 *
 * source is the output packet (Ethernet payload)
 * source_length is the output packet (Ethernet payload)
 */

case class AxisDownSizer(dataWidthIn : Int, dataWidthOut: Int) extends Component {
  /* upsizing is not supported yet */
  require(dataWidthIn > dataWidthOut, "dataWidthOut must be an integer division of dataWidthIn")
  /* non-integer width factors are not supported */
  require(dataWidthIn % dataWidthOut == 0, "dataWidthOut must be an integer division of dataWidthIn")

  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidthIn bits)))
    val source = master Stream(Fragment(Bits(dataWidthOut bits)))
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
  }

  // x is sink
  val x = Stream(Fragment(Bits(dataWidthIn bits)))
  val x_length = UInt(12 bits)

  x << io.sink
  x_length := io.sink_length
   
  // y is input stream with original payload, but staged
  val y = Stream(Fragment(Bits(dataWidthIn bits)))
  y <-< x
  val y_length = RegNextWhen(x_length, x.ready)

  val z = Stream(Fragment(Bits(dataWidthOut bits)))

  // calculate number of output beats based on input length
  val y_out_beat_last = RegNextWhen((x_length + dataWidthOut / 8 - 1) / (dataWidthOut / 8) - 1, x.ready)
  //val y_out_beat_last = (y_length - 1) / (dataWidthOut / 8)
  val y_out_beat_num = Reg(UInt(12 bits)) init(0)

  // current output beat in z?
  when (z.fire) {
    // last output beat for this input frame?
    when (y_out_beat_num === y_out_beat_last) {
      y_out_beat_num := 0
    } otherwise {
      y_out_beat_num := y_out_beat_num + 1
    }
  }
  val factor = dataWidthIn / dataWidthOut
  // when the input can take the next beat
  val next_input = ((y_out_beat_num % factor) === (factor - 1)) |
    (y_out_beat_num === y_out_beat_last)

  val counter = y_out_beat_num.resize(log2Up(factor))
  z.valid := y.valid
  //endianness match {
  //  case `LITTLE` =>
    z.fragment.assignFromBits(y.fragment.asBits.resize(dataWidthIn).subdivideIn(factor slices).read(counter))
  //  case `BIG`    => output.fragment.assignFromBits(y.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).reverse.read(counter))
  // }
  z.last := y.last && (y_out_beat_num === y_out_beat_last)
  y.ready := z.ready && next_input

  // register outputs
  io.source <-< z
  io.source_length := RegNextWhen(y_length, z.ready)

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}
