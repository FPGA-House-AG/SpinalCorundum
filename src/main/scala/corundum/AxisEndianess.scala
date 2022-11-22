package corundum

import spinal.core._
import spinal.lib._

import scala.math._

// companion object
object AxisEndianess {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    SpinalVerilog(new AxisEndianess(512))
    SpinalVhdl(new AxisEndianess(512))
  }
}

case class AxisEndianess(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink = slave Stream Fragment(Bits(dataWidth bits))
    val source = master Stream Fragment(Bits(dataWidth bits))
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
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

  val num_bytes = dataWidth / 8
  io.source << io.sink.~~(_.~~(_.asBits.subdivideIn(num_bytes slices).reverse.asBits()))
  //io.source << io.sink.translateWith(io.sink.fragment.asBits.subdivideIn(num_bytes slices).reverse.asBits())
  //io.source.fragment.assignFromBits(io.sink.fragment.asBits.resize(dataWidth).subdivideIn(num_bytes slices).reverse.asBits())
  io.source_length := io.sink_length
  val x = RegNext(io.sink_length)

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

//This two VHDL functions change the endianess. This enables to change from Big to Little Endian and vice versa. It is assumed that 1 byte are 8 bits.
//
//  -- changes the endianess BIG <-> LITTLE for std_ulogic_vector
//  function ChangeEndian(vec : std_ulogic_vector) return std_ulogic_vector is
//    variable vRet      : std_ulogic_vector(vec'range);
//    constant cNumBytes : natural := vec'length / 8;
//  begin
//
//    for i in 0 to cNumBytes-1 loop
//      for j in 7 downto 0 loop
//        vRet(8*i + j) := vec(8*(cNumBytes-1-i) + j);
//      end loop; -- for j
//    end loop; -- for i
//
//    return vRet;
//  end function ChangeEndian;
//
//  -- changes the endianess BIG <-> LITTLE for std_logic_vector
//  function ChangeEndian(vec : std_logic_vector) return std_logic_vector is
//  begin
//    return std_logic_vector(ChangeEndian(std_ulogic_vector(vec)));
//  end function ChangeEndian;