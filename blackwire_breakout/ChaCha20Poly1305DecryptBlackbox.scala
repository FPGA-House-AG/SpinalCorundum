package blackwire

import corundum._

import spinal.core._
import spinal.lib._

// Define ChaCha20Poly1305Decrypt / AEAD_decryption_wrapper
// This is called AEAD_decryption_wrapper by Maxim, renamed here
class AEAD_decryption_wrapper_kar() extends BlackBox {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val sink_tvalid  = in Bool()
    val sink_tlast   = in Bool()
    val sink_tdata   = in UInt(128 bits)
    val sink_tready  = out Bool()

    val in_key   = in UInt(256 bits)

    val source_tvalid  = out Bool()
    val source_tlast   = out Bool()
    val source_tdata   = out UInt(128 bits)
    val source_tready  = in Bool()

    val tag_valid       = out Bool()
    val tag_pulse       = out Bool()
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
    val tag_pulse = out Bool()
  }
  val vhdl = new AEAD_decryption_wrapper_kar()

  // decrypted output
  val d = Stream(Fragment(Bits(128 bits)))

  // enforce one idle cycle after last beat, this is
  // required by VHDL ChaCha20Poly1305
  val after_last = RegNext(io.sink.lastFire)

  vhdl.io.sink_tvalid := io.sink.valid & !after_last
  vhdl.io.sink_tdata  := U(io.sink.payload.fragment.subdivideIn(8 bits).reverse.asBits)
  vhdl.io.sink_tlast  := io.sink.payload.last
  // pass-through READY outside of the VHDL block, not READY after LAST
  io.sink.ready             := d.ready & !after_last
  vhdl.io.in_key  := U(io.key)

  d.valid                := vhdl.io.source_tvalid
  d.payload.fragment     := B(vhdl.io.source_tdata).subdivideIn(8 bits).reverse.asBits
  d.payload.last         := vhdl.io.source_tlast
  vhdl.io.source_tready  := d.ready

  // one stage delay, such that tag_valid coincides with io.last
  io.source <-< d
  
  io.tag_valid           := vhdl.io.tag_valid
  io.tag_pulse           := vhdl.io.tag_pulse

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}