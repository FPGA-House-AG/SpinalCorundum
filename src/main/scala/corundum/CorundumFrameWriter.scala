package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameWriter {
  val addressWidth = 10
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
  //val slaveAddressWidth = 10

  //require(dataWidth == 64)
  val io = new Bundle {
    // this is where driveFrom() drives into
    val input = slave(Stream(Fragment(CorundumFrame(dataWidth))))
    // and we simply pass it on the output
    val output = master(Stream(Fragment(CorundumFrame(dataWidth))))

    //val slaveAddrWidth = 10
  }
  io.output << io.input

  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {
    // address decoding assumes slave-local addresses
    //assert(busCtrl.busAddressWidth == addressWidth)
    // mostly tkeep is still hardcoded for 32-bit bus controller
    assert(busCtrl.busDataWidth == 32)

    //val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val busCtrlWrapped = busCtrl

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

    //val tkeep_last_bits = Reg(Bits(32 bits))
    //busCtrlWrapped.writeMultiWord(tkeep_last_bits, 0x100, documentation = null)

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
    val (fifo, fifoAvailability) = corundum.queueWithAvailability(fifoDepth) //.init(0)
    busCtrlWrapped.read(fifoAvailability, address = 0x40)
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

// slave must be naturally aligned
case class CorundumFrameWriterAxi4(dataWidth : Int, busCfg : Axi4Config) extends Component {

  // copy AXI4 properties from bus, but override address with from slave
  //val slaveCfg = busCfg.copy(addressWidth = writer.slaveAddressWidth/*writer.io.slaveAddrWidth*/)
  val slaveCfg = busCfg.copy(addressWidth = CorundumFrameWriter.addressWidth)
  
  val io = new Bundle {
    val output = master(Stream(Fragment(CorundumFrame(dataWidth))))
    val ctrlbus = slave(Axi4(slaveCfg))
  }

  val writer = CorundumFrameWriter(dataWidth)
  val ctrl = new Axi4SlaveFactory(io.ctrlbus)
  val bridge = writer.driveFrom(ctrl, 0)
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
      val toplevel = new CorundumFrameWriterAxi4(512, Axi4Config(CorundumFrameWriter.addressWidth, 32, 2, useQos = false, useRegion = false))
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
