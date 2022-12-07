package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object CorundumFrameReader {
}

// Write a stream of fragments (i.e. generate a packet stream)

// 0x000   Identifier ("PKWR")
// 0x004   [31:16] Version and [15:0] Revision
// 0x020   Width in bits of CorundumFrame TDATA words
// 0x040   Available words (in number of TDATA words) in the input FIFO
// 0x080   bit 31: valid, bit 30: last, bits [15:0] empty_bytes
// 0x100.. Write (partial) 32-bit words of the packet word (which can be 512 bits)
//
// The bus data widthe width must be 32-bit currently, and the packet size is a multiple of 32-bits.
// For our purpose, not yet a limitation. Limitation is mostly due to tkeep updates.

case class CorundumFrameReader(dataWidth : Int) extends Component {
  //require(dataWidth == 64)
  val io = new Bundle {
    // external stream input
    val input = slave(Stream(Fragment(CorundumFrame(dataWidth))))
    // this is where driveFrom() reads from
    val output = master(Stream(Fragment(CorundumFrame(dataWidth))))
  }
  // pass component input to output towards bus controller
  // (this is a recommended workaround for bus controllers in SpinalHDL)
  io.output << io.input

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))

  def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {
    // address decoding assumes slave-local addresses
    //assert(busCtrl.busAddressWidth == addressWidth)
    // mostly tkeep is still hardcoded for 32-bit bus controller
    assert(busCtrl.busDataWidth == 32)

    //val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val busCtrlWrapped = busCtrl

    val keepWidth = dataWidth / 8

    // will be used for identification purposes
    busCtrlWrapped.read(B"32'hBBBBBBBB", 0x000, documentation = null)
    // 16'b version and 16'b revision
    busCtrlWrapped.read(B"32'b00010001", 0x004, documentation = null)
    // some strictly increasing (not per se incrementing) build number
    val gitCommits = B(BigInt(SourceCodeGitCommits()), 32 bits)
    busCtrlWrapped.read(gitCommits, 0x008, 0, null)
    // GIT hash
    val gitHash = B(BigInt(SourceCodeGitHash(), 16), 160 bits)
    busCtrlWrapped.readMultiWord(gitHash, 0x00c, documentation = null)
    // dataWidth
    val instanceDataWidth = U(dataWidth, 32 bits)
    busCtrlWrapped.read(instanceDataWidth, 0x020, 0, null)

    // read the packet through a FIFO (@TODO maybe remove, not much use...)
    val fifoDepth = 4
    val (fifo, fifoOccupancy) = io.output.queueWithOccupancy(fifoDepth) //.init(0)
    busCtrlWrapped.read(fifoOccupancy, address = 0x40)

    val empty_bytes = LeadingZeroes(fifo.payload.tkeep)
    val tkeep_empty = RegNext(fifo.valid.asUInt ## fifo.last.asUInt ## empty_bytes.resize(30))
    busCtrlWrapped.read(tkeep_empty, 0x080, documentation = null)

    // 0x100.. write into wide (512-bits?) register
    //val stream_word = RegNext(io.input.payload.tdata)
    busCtrlWrapped.readMultiWord(fifo.payload.tdata, 0x100, documentation = null)

    // match a range of addresses using mask
    import spinal.lib.bus.misc.MaskMapping

    // writing packet word content?
    def isAddressed(): Bool = {
      //val mask_mapping = MaskMapping(0xFFFFC0L/*64 addresses, 16 32-bit regs*/, 0x000100L)
      val size_mapping = SizeMapping(0x100, dataWidth / 8)
      val ret = False
      busCtrlWrapped.onReadPrimitive(address = size_mapping, false, ""){ ret := True }
      ret
    }

    val addressed = isAddressed()

    // valid will push the current packet word out
    val valid = Reg(Bool) init(False)

    val reg_idx = busCtrlWrapped.readAddress.resize(log2Up(dataWidth / 8)) / (busCtrl.busDataWidth / 8)
    val full_bytes = keepWidth - empty_bytes
    val full_words = (full_bytes + busCtrl.busDataWidth / 8 - 1) / (busCtrl.busDataWidth / 8)

    valid := False

    fifo.ready := False
    when (isAddressed()) {
      // reading last CPU word with byte enables of stream word?
      when (reg_idx === (full_words - 1)) {
        fifo.ready := True
      }
      // reading last CPU word of stream word?
      //when (reg_idx === (dataWidth / busCtrl.busDataWidth - 1)) {
      //  fifo.ready := True
      //}
    }
  }
}

// companion object
object CorundumFrameReaderAxi4 {
  final val slaveAddressWidth = 10
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new CorundumFrameReaderAxi4(Config.corundumDataWidth, Axi4Config(32, 32, 2, useQos = false, useRegion = false)))
    val verilogReport = Config.spinal.generateVerilog(new CorundumFrameReaderAxi4(Config.corundumDataWidth, Axi4Config(32, 32, 2, useQos = false, useRegion = false)))
  }
}

// slave must be naturally aligned
case class CorundumFrameReaderAxi4(dataWidth : Int, busCfg : Axi4Config) extends Component {

  // copy AXI4 properties from bus, but override address width for slave
  val slaveCfg = busCfg.copy(addressWidth = CorundumFrameReaderAxi4.slaveAddressWidth)
  
  val io = new Bundle {
    val input = slave(Stream(Fragment(CorundumFrame(dataWidth))))
    val ctrlbus = slave(Axi4(slaveCfg))
  }

  val reader = CorundumFrameReader(dataWidth)
  val ctrl = new Axi4SlaveFactory(io.ctrlbus)
  val bridge = reader.driveFrom(ctrl, 0)
  reader.io.input << io.input

  addPrePopTask(() => CorundumFrame.renameAxiIO(io))  
}
