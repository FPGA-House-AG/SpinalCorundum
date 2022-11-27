package blackwire

import spinal.core._
import spinal.lib._

// Define ChaCha20Poly1305Decrypt
// This is called AEAD_decryption_wrapper by Maxim, renamed here
class ChaCha20Poly1305DecryptBlackbox() extends BlackBox {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val axi_tvalid_in_msg  = in Bool()
    val axi_tlast_in_msg   = in Bool()
    val axi_tdata_in_msg   = in UInt(128 bits)
    val axi_tready_in_msg  = out Bool()

    val axi_tdata_in_key   = in UInt(256 bits)

    val axi_tvalid_out  = out Bool()
    val axi_tlast_out   = out Bool()
    val axi_tdata_out   = out UInt(128 bits)
    val axi_tready_out  = in Bool()

    val tag_valid       = out Bool()
  }

  // Map the current clock domain to the io.clk and io.rst pins
  mapClockDomain(clock = io.clk, reset = io.rst)

  noIoPrefix()
}

// companion object
object BlackwireChaCha20Poly1305DecryptSpinal {
  //def main(args: Array[String]) {
  //  SpinalVerilog(new ChaCha20Poly1305DecryptSpinal())
  //  SpinalVhdl(new ChaCha20Poly1305DecryptSpinal())
  //}
}


// Define ChaCha20Poly1305Decrypt
case class ChaCha20Poly1305DecryptSpinal() extends Component {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val sink   = slave Stream(Fragment(Bits(128 bits)))
    val source = master Stream(Fragment(Bits(128 bits)))
    val key    = in Bits (256 bit)
    val tag_valid = out Bool()
  }
  val vhdl = new ChaCha20Poly1305DecryptBlackbox()
  vhdl.io.axi_tvalid_in_msg := io.sink.valid
  vhdl.io.axi_tdata_in_msg  := U(io.sink.payload.fragment)
  vhdl.io.axi_tlast_in_msg  := io.sink.payload.last
  io.sink.ready             := vhdl.io.axi_tready_in_msg
  vhdl.io.axi_tdata_in_key  := U(io.key)

  io.source.valid            := vhdl.io.axi_tvalid_out
  io.source.payload.fragment := B(vhdl.io.axi_tdata_out)
  io.source.payload.last     := vhdl.io.axi_tlast_out
  vhdl.io.axi_tready_out     := io.source.ready

  io.tag_valid               := vhdl.io.tag_valid
}