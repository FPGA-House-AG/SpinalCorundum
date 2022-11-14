package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object AxisWireguardKeyLookup {
}

/* Extract and remove the Wireguard Type 4 header, output receiver for key lookup
 * Delay output to match key lookup latency of two clock cycles.
 *
 * sink accepts AXIS frames (Ethernet packet)
 * sink_length is the input packet length in bytes, this packet arrives on the sink
 *
 * source is the output packet (Ethernet payload)
 * source_length is the output packet (Ethernet payload)
 */

case class AxisWireguardKeyLookup(dataWidth : Int) extends Component {
  require(dataWidth == 128, "Assuming Wireguard Type 4 header is one word")
  val io = new Bundle {
    // I/O is only the Corundum Frame tdata payload
    val sink = slave Stream(Fragment(Bits(dataWidth bits)))
    val source = master Stream(Fragment(Bits(dataWidth bits)))
    // sink_length is given in bytes
    val sink_length = in UInt(12 bits)
    val source_length = out UInt(12 bits)
    // outgoing lookup index for the key
    val receiver = out UInt(32 bits)
    val counter = out UInt(32 bits)
    // key will have 2 cycles latency relative to receiver
    val key = out Bits(16 bits)
  }

  // register Wireguard Type 4 header fields
  val receiver = RegNextWhen(io.sink.payload(4 * 8, 32 bits), io.sink.isFirst)
  val counter = RegNextWhen(io.sink.payload(8 * 8, 32 bits), io.sink.isFirst)

  io.receiver := receiver.asUInt
  io.counter := counter.asUInt

  val sink_is_first = io.sink.isFirst

  val y = Stream(Fragment(Bits(dataWidth bits)))

  // remove 128 bits Wireguard Type 4 header on output by clearing valid
  // register to achieve one pipeline stage
  y.valid := RegNextWhen(io.sink.valid & !io.sink.isFirst, io.sink.ready) init(False)
  y.payload := RegNextWhen(io.sink.payload, io.sink.ready)
  y.last := RegNextWhen(io.sink.last, io.sink.ready)
  io.sink.ready := y.ready

  // add one extra pipeline stage
  io.source <-< y

  // remove 128 bits Wireguard Type 4 header from output length
  val unpadded_length_out = RegNextWhen(io.sink_length - 128/8, io.sink.isFirst)
  // round up to next 16 bytes (should we always do this? -- Ethernet MTU?)
  val padded16_length_out = RegNext(((unpadded_length_out + 15) >> 4) << 4)
  // during testing, do not pad, to verify correct propagation delay in simulator
  //val padded16_length_out = RegNext(unpadded_length_out)
  io.source_length := RegNext(padded16_length_out)

  val has_internal_test_lut = true

  if (has_internal_test_lut) {
    val keys_num = 256
    val lut = LookupTable(16, keys_num)
    lut.mem.initBigInt(Seq.range(0xA000, 0xA000 + keys_num))
    lut.io.portA.en := True
    lut.io.portA.wr := False
    lut.io.portA.wrData := 0
    lut.io.portA.addr := receiver.resize(log2Up(keys_num)).asUInt

    io.key := lut.io.portA.rdData

    lut.io.portB.clk := ClockDomain.current.readClockWire
    lut.io.portB.rst := False
    lut.io.portB.en := True
    lut.io.portB.wr := False
    lut.io.portB.wrData := 0
    lut.io.portB.addr := 0
  } else {
    io.key := 0
  }
  val source_is_first = io.source.isFirst


  // Rename SpinalHDL library defaults to AXI naming convention
  private def renameIO(): Unit = {
    io.flatten.foreach(bt => {
      if(bt.getName().contains("_payload_fragment")) bt.setName(bt.getName().replace("_payload_fragment", "_tdata"))
      if(bt.getName().contains("_payload_last")) bt.setName(bt.getName().replace("_payload_last", "_tlast"))
      if(bt.getName().contains("_payload"))  bt.setName(bt.getName().replace("_payload",  ""))
      if(bt.getName().contains("_fragment")) bt.setName(bt.getName().replace("_fragment", ""))
      if(bt.getName().contains("_valid"))    bt.setName(bt.getName().replace("_valid",    "_tvalid"))
      if(bt.getName().contains("_ready"))    bt.setName(bt.getName().replace("_ready",    "_tready"))
      if(bt.getName().contains("_last"))     bt.setName(bt.getName().replace("_last",     "_tlast"))
      if(bt.getName().contains("reset"))     bt.setName(bt.getName().replace("reset",     "rst"))
    })
  }
  // Remove io_ prefix
  noIoPrefix()

  // Execute the function renameIO after the creation of the component
  addPrePopTask(() => renameIO())
}

//Generate the AxisWireguardKeyLookup's Verilog
object AxisWireguardKeyLookupVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new AxisWireguardKeyLookup(128))
  }
}

//Generate the AxisWireguardKeyLookup's VHDL
object AxisWireguardKeyLookupVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new AxisWireguardKeyLookup(128))
  }
}
