package blackwire

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

import corundum._

// companion object
object BlackwireReceiveFmax {
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new BlackwireReceiveFmax())
  }
}

// composition of the RX data flow towards ChaCha20-Poly1305
// stash -> downsizer -> key lookup ->
case class BlackwireReceiveFmax() extends Component {
  val io = new Bundle {
    val result = out Bool()
  }

  val lfsr = LinearFeedbackShiftRegister(32)
  lfsr.io.enable := True

  val source = Reg(Stream(Fragment(CorundumFrame(512))))

  for (i <- 0 until 512 / 32) {
      source.fragment.tdata(i * 32 + 31 downto i * 32) := lfsr.io.lfsr
  }

  for (i <- 0 until (512 / 8) / 32) {
      source.fragment.tkeep(i * 32 + 31 downto i * 32) := lfsr.io.lfsr
  }
  source.valid := lfsr.io.lfsr(0)
  source.last  := lfsr.io.lfsr(1)

  source.fragment.tuser := 0

  val rx = BlackwireReceive()

  rx.io.sink << source
  rx.io.source.ready := lfsr.io.lfsr(1)

  io.result := rx.io.source.fragment.tdata.xorR ^ 
    rx.io.source.fragment.tkeep.xorR ^ rx.io.source.valid
}
