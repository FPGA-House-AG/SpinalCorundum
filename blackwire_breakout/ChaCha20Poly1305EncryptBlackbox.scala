package blackwire

import corundum._

import spinal.core._
import spinal.lib._

// Define ChaCha20Poly1305Encrypt / AEAD_encryption_wrapper
// This is called AEAD_encryption_wrapper by Maxim, renamed here
class AEAD_encryption_wrapper_kar() extends BlackBox {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()

    val sink_tvalid = in Bool()
    val sink_tlast  = in Bool()
    val sink_tdata  = in UInt(128 bits)
    val sink_tready = out Bool()

    val in_key   = in UInt(256 bits)

    val source_tvalid  = out Bool()
    val source_tlast   = out Bool()
    val source_tdata   = out UInt(128 bits)
    val source_tready  = in Bool()

    val header_out     = out UInt(128 bits)
  }

  // Map the current clock domain to the io.clk and io.rst pins
  mapClockDomain(clock = io.clk, reset = io.rst)

  noIoPrefix()
}

// companion object
object BlackwireChaCha20Poly1305EncryptSpinal {
  def main(args: Array[String]) {
    SpinalVerilog(new ChaCha20Poly1305EncryptSpinal())
    SpinalVhdl(new ChaCha20Poly1305EncryptSpinal())
  }
}

// Define ChaCha20Poly1305Encrypt
case class ChaCha20Poly1305EncryptSpinal() extends Component {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val sink   = slave Stream(Fragment(Bits(128 bits)))
    val source = master Stream(Fragment(Bits(128 bits)))
    val key    = in Bits(256 bit)
    val header_out = out Bits(128 bits)
  }
  val vhdl = new AEAD_encryption_wrapper_kar()

  // encrypted output
  val e = Stream(Fragment(Bits(128 bits)))

  // enforce one idle cycle after last beat, this is
  // required by VHDL ChaCha20Poly1305
  val after_last = RegNext(io.sink.lastFire)

  vhdl.io.sink_tvalid := io.sink.valid & !after_last
  vhdl.io.sink_tdata  := U(io.sink.payload.fragment.subdivideIn(8 bits).reverse.asBits)
  vhdl.io.sink_tlast  := io.sink.payload.last
  // pass-through READY outside of the VHDL block, not READY after LAST
  io.sink.ready       := e.ready & !after_last
  vhdl.io.in_key      := U(io.key)

  e.valid                := vhdl.io.source_tvalid
  e.payload.fragment     := B(vhdl.io.source_tdata).subdivideIn(8 bits).reverse.asBits
  e.payload.last         := vhdl.io.source_tlast
  vhdl.io.source_tready  := e.ready

  io.header_out := RegNext(B(vhdl.io.header_out).subdivideIn(8 bits).reverse.asBits)

  io.source <-< e

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}