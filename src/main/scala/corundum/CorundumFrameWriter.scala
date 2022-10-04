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
  require(dataWidth == 64)
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
    //val outputStreamLogic = new Area {
    //  val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Fragment(CorundumFrame(widthOf(io.input.fragment.tdata))), address = 0x100).toStream
    //  val (streamBuffered, fifoAvailability) = streamUnbuffered.queueWithAvailability(4)
    //  io.input << streamBuffered
    //}

    val corundum = Stream Fragment(CorundumFrame(dataWidth))

    // 0x100.. write into wide (512-bits?) register
    val stream_word = Reg(Bits(dataWidth bits))
    busCtrlWrapped.writeMultiWord(stream_word, 0x000100, documentation = null)

    // match a range of addresses using mask
    import spinal.lib.bus.misc.MaskMapping

    // writing packet word content?
    def isAddressed(): Bool = {
      val mask_mapping = MaskMapping(0xFFFFC0L/*64 addresses, 16 32-bit regs*/, 0x000100L)
      val size_mapping = SizeMapping(0x100, dataWidth / 8)
      val ret = False
      busCtrlWrapped.onWritePrimitive(address = size_mapping, false, ""){ ret := True }
      ret
    }

    val commit2 = RegNext(isAddressed())

    // set (per-byte) tkeep bits for all 32-bit registers being written
    val tkeep = Reg(Bits(dataWidth / 8 bits)) init (B"10101010")
    val valid = Reg(Bool) init(False)
    val tlast = Reg(Bool) init(False)

    val reg_idx = ((busCtrlWrapped.writeAddress - 0x100) / 4)

    valid := False

    tkeep := tkeep
    when (isAddressed()) {
      val reg_idx = (busCtrlWrapped.writeAddress - 0x100) / 4
      when (reg_idx === 0) {
        tkeep := 0
      }
      // this is the last write
      when (tlast || (reg_idx === (dataWidth / busCtrl.busDataWidth - 1))) {
         valid := True
      }
      //tkeep(reg_idx * 4, 4 bits) := tkeep(reg_idx * 4, 4 bits) | ctrl.writeByteEnable
      tkeep(reg_idx * 4, 4 bits) := B"1111"
    }

    //corundum.valid := False
    // 0x140 VALID=1, TLAST=0
    //busCtrlWrapped.onWrite(0x000100L + 4 * (dataWidth / busCtrl.busDataWidth - 1), null){
    //  valid := True
    //}

    //tlast := False
    //corundum.last := False
    // 0x144 VALID=1, TLAST=1
    when (valid) {
       tlast := False
    }
    busCtrlWrapped.onWrite(0x000140L, null){
      //valid := True
      tlast := True
    }
    //val strb = RegNext(busCtrlWrapped.writeByteEnable);

    corundum.last := tlast
    corundum.valid := valid
    corundum.payload.tdata := stream_word
    corundum.payload.tkeep := tkeep
    corundum.payload.tuser := 0
    //val fifo = new StreamFifo(Fragment(CorundumFrame(dataWidth)), 4)
    //fifo.io.push << corundum
    //io.input << fifo.io.pop
    io.input << corundum
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
