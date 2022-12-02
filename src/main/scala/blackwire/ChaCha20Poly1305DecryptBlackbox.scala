package blackwire

import corundum._

import spinal.core._
import spinal.lib._

// Define ChaCha20Poly1305Decrypt / AEAD_decryption_wrapper
// This is called AEAD_decryption_wrapper by Maxim, renamed here
class AEAD_decryption_wrapper() extends BlackBox {
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
  val vhdl = new AEAD_decryption_wrapper()

  // decrypted output
  val d = Stream(Fragment(Bits(128 bits)))

  // enforce one idle cycle after last beat, this is
  // required by VHDL ChaCha20Poly1305
  val after_last = RegNext(io.sink.lastFire)

  vhdl.io.axi_tvalid_in_msg := io.sink.valid & !after_last
  vhdl.io.axi_tdata_in_msg  := U(io.sink.payload.fragment)
  vhdl.io.axi_tlast_in_msg  := io.sink.payload.last
  // pass-through READY outside of the VHDL block, not READY after LAST
  io.sink.ready             := d.ready & !after_last
  vhdl.io.axi_tdata_in_key  := U(io.key)

  d.valid                := vhdl.io.axi_tvalid_out
  d.payload.fragment     := B(vhdl.io.axi_tdata_out)
  d.payload.last         := vhdl.io.axi_tlast_out
  vhdl.io.axi_tready_out := d.ready

  // one stage delay, such that tag_valid coincides with io.last
  io.source <-< d
  
  io.tag_valid           := vhdl.io.tag_valid

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}