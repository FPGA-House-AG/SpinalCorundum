package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameWriter {
}

// write a stream of fragments (i.e. generate a packet stream)
case class CorundumFrameWriter(dataWidth : Int) extends Component {
  require(dataWidth == 32)
  val io = new Bundle {
    // this is where driveFrom() drives into
    val input = slave(Stream(Fragment(CorundumFrame(dataWidth))))
    // and we simply pass it on the output
    val output = master(Stream(Fragment(CorundumFrame(dataWidth))))
  }
  io.output << io.input

  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {
    require(busCtrl.busDataWidth == 32)

    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    // will be used for identification purposes
    busCtrlWrapped.read(B"32'b00010001", 0x00, documentation = null)
    // 16'b version and 16'b revision
    busCtrlWrapped.read(B"32'b00010001", 0x04, documentation = null)
    // some strictly increasing (not per se incrementing) build number
    val gitCommits = B(BigInt(SourceCodeGitCommits()), 32 bits)
    busCtrlWrapped.read(gitCommits, 0x08, 0, null)
    // GIT hash
    val gitHash = B(BigInt(SourceCodeGitHash(), 16), 160 bits)
    busCtrlWrapped.readMultiWord(gitHash, 0x0c, documentation = null)

    // drive stream from 0x100
    val outputStreamLogic = new Area {
      val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Fragment(CorundumFrame(widthOf(io.input.fragment.tdata))), address = 0x100).toStream
      val (streamBuffered, fifoAvailability) = streamUnbuffered.queueWithAvailability(4)
      io.input << streamBuffered
    }
  }
}

//Generate the CorundumFrameWriter's Verilog
object CorundumFrameWriterVerilog {
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameWriter(512)
      XilinxPatch(toplevel)
    })
  }
}

//Generate the CorundumFrameWriter's VHDL
object CorundumFrameWriterVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameWriter(512))
  }
}
