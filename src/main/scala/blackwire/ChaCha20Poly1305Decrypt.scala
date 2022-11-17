package blackwire

import spinal.core._
import spinal.lib._

// Define ChaCha20Poly1305Decrypt
class ChaCha20Poly1305Decrypt() extends BlackBox {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val clk = in Bool()
    val sink_tready  = out Bool()
    val sink_tvalid  = in Bool()
    val sink_tlast   = in Bool()
    val sink_tdata   = in Bits (128 bit)
    val key          = in Bits (256 bit)

    val source_tready  = in Bool()
    val source_tvalid  = out Bool()
    val source_tlast   = out Bool()
    val source_tdata   = out Bits (128 bit)
    val tag_valid      = out Bool()
  }

  // Map the current clock domain to the io.clk pin
  mapClockDomain(clock = io.clk)
}

// companion object
object BlackwireChaCha20Poly1305DecryptSpinalReceive {
  def main(args: Array[String]) {
    SpinalVerilog(new ChaCha20Poly1305DecryptSpinal())
    SpinalVhdl(new ChaCha20Poly1305DecryptSpinal())
  }
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
  val vhdl = new ChaCha20Poly1305Decrypt()
  vhdl.io.sink_tvalid := io.sink.valid
  vhdl.io.sink_tdata  := io.sink.payload.fragment
  vhdl.io.sink_tlast  := io.sink.payload.last
  io.sink.ready       := vhdl.io.sink_tready
  vhdl.io.key         := io.key

  io.source.valid            := vhdl.io.source_tvalid
  io.source.payload.fragment := vhdl.io.source_tdata
  io.source.payload.last     := vhdl.io.source_tlast
  vhdl.io.source_tready      := io.source.ready
}