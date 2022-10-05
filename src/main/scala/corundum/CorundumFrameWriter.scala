package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameWriter {
}

// Write a stream of fragments (i.e. generate a packet stream)

// 0x000   Identifier ("PKWR")
// 0x004   [31:16] Version and [15:0] Revision
// 0x020   Width in bits of CorundumFrame TDATA words
// 0x040   Available space (in number of TDATA words) in the output FIFO
// 0x080   After writing (any value) to this register, the next write to 0x100.. completes the packet
// 0x100.. Write (partial) 32-bit words of the packet word (which can be 512 bits)
//
// The bus data widthe width must be 32-bit currently, and the packet size is a multiple of 32-bits.
// For our purpose, not yet a limitation. Limitation is mostly due to tkeep updates.

case class CorundumFrameWriter(dataWidth : Int) extends Component {
  //require(dataWidth == 64)
  val io = new Bundle {
    // this is where driveFrom() drives into
    val input = slave(Stream(Fragment(CorundumFrame(dataWidth))))
    // and we simply pass it on the output
    val output = master(Stream(Fragment(CorundumFrame(dataWidth))))
  }
  io.output << io.input

  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {
    // mostly tkeep is still hardcoded for 32-bit bus controller
    require(busCtrl.busDataWidth == 32)

    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    // will be used for identification purposes
    busCtrlWrapped.read(B"32'hAABBCCDD", 0x000, documentation = null)
    // 16'b version and 16'b revision
    busCtrlWrapped.read(B"32'b00010001", 0x004, documentation = null)
    // some strictly increasing (not per se incrementing) build number
    val gitCommits = B(BigInt(SourceCodeGitCommits()), 32 bits)
    busCtrlWrapped.read(gitCommits, 0x008, 0, null)
    // GIT hash
    val gitHash = B(BigInt(SourceCodeGitHash(), 16), 160 bits)
    busCtrlWrapped.readMultiWord(gitHash, 0x00c, documentation = null)
    // dataWidth
    val instanceDataWidth = B(BigInt(dataWidth), 32 bits)
    busCtrlWrapped.read(instanceDataWidth, 0x020, 0, null)

    // 0x100.. write into wide (512-bits?) register
    val stream_word = Reg(Bits(dataWidth bits))
    busCtrlWrapped.writeMultiWord(stream_word, 0x100, documentation = null)

    // match a range of addresses using mask
    import spinal.lib.bus.misc.MaskMapping

    // writing packet word content?
    def isAddressed(): Bool = {
      //val mask_mapping = MaskMapping(0xFFFFC0L/*64 addresses, 16 32-bit regs*/, 0x000100L)
      val size_mapping = SizeMapping(0x100, dataWidth / 8)
      val ret = False
      busCtrlWrapped.onWritePrimitive(address = size_mapping, false, ""){ ret := True }
      ret
    }

    val commit2 = RegNext(isAddressed())

    // set (per-byte) tkeep bits for all 32-bit registers being written
    val tkeep = Reg(Bits(dataWidth / 8 bits)) //init (0)
    // valid will push the current packet word out
    val valid = Reg(Bool) init(False)
    // tlast indicates if the next write on the bus completes the packet
    val tlast = Reg(Bool) init(False)

    val reg_idx = ((busCtrlWrapped.writeAddress - 0x100) / 4)

    valid := False

    when (isAddressed()) {
      val reg_idx = (busCtrlWrapped.writeAddress - 0x100) / 4
      // indicated last write, or writing last word?
      when (tlast || (reg_idx === (dataWidth / busCtrl.busDataWidth - 1))) {
         valid := True
      }
      when (reg_idx === 0) {
        tkeep := 0
      }
      //tkeep(reg_idx * 4, 4 bits) := tkeep(reg_idx * 4, 4 bits) | ctrl.writeByteEnable
      tkeep(reg_idx * 4, 4 bits) := B"1111"
    }
    // reset TLAST after VALID
    when (valid) {
       tlast := False
    }
    busCtrlWrapped.onWrite(0x80, null){
      tlast := True
    }
    //val strb = RegNext(busCtrlWrapped.writeByteEnable);

    val corundum = Stream Fragment(CorundumFrame(dataWidth))
    corundum.last := tlast
    corundum.valid := valid
    corundum.payload.tdata := stream_word
    corundum.payload.tkeep := tkeep
    corundum.payload.tuser := 0

    val fifoDepth = 1
    // @TODO must be push size Availability
    val (fifo, fifoOccupancy) = corundum.queueWithOccupancy(fifoDepth) //.init(0)
    busCtrlWrapped.read(fifoDepth - fifoOccupancy, address = 0x40)
    io.input << fifo

    // drive stream from 0x100
    //val outputStreamLogic = new Area {
    //  val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Fragment(CorundumFrame(widthOf(io.input.fragment.tdata))), address = 0x100).toStream
    //  val (streamBuffered, fifoAvailability) = streamUnbuffered.queueWithAvailability(4)
    //  io.input << streamBuffered
    //}


    //val fifo = new StreamFifo(Fragment(CorundumFrame(dataWidth)), 4)
    //fifo.io.push << corundum
    //io.input << fifo.io.pop
    //io.input << corundum
  }
}

// companion object
object CorundumFrameWriterAxi4 {
}

// axi4Cfg = Axi4Config(32, 32, 2, useQos = false, useRegion = false)
case class CorundumFrameWriterAxi4(dataWidth : Int, axi4Cfg : Axi4Config, baseAddress : BigInt) extends Component {
  val io = new Bundle {
    val output = master(Stream(Fragment(CorundumFrame(dataWidth))))
    val slave0 = slave(Axi4(axi4Cfg))
  }

  val ctrl = new Axi4SlaveFactory(io.slave0)
  val writer = CorundumFrameWriter(dataWidth)
  val bridge = writer.driveFrom(ctrl, baseAddress)
  io.output << writer.io.output
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

//Generate the CorundumFrameWriter's Verilog
object CorundumFrameWriterAxi4Verilog {
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameWriterAxi4(512, Axi4Config(32, 32, 2, useQos = false, useRegion = false), 0 )
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
