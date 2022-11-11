package corundum

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

// companion object
object AxisWireguardKeyLookup {
}

/* Down size the AXIS data width with a factor of 2 or 4
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
    val key = in Bits(128 bits)
  }

  val receiver = RegNextWhen(io.sink.payload(4 * 8, 32 bits), io.sink.isFirst)
  val counter = RegNextWhen(io.sink.payload(8 * 8, 32 bits), io.sink.isFirst)
  io.receiver := receiver.asUInt
  io.counter := counter.asUInt

  val sink_is_first = io.sink.isFirst

  //io.source <-< io.sink
  // strip off first word - @TODO does this work?
  // AND what if packet is zero size, i.e. nothing behind header?
  io.source.valid := RegNextWhen(io.sink.valid & !io.sink.isFirst, io.sink.ready) init(False)
  io.source.payload := RegNextWhen(io.sink.payload, io.sink.ready)
  io.source.last := RegNextWhen(io.sink.last, io.sink.ready)
  io.sink.ready := io.source.ready

  // @TODO round up to next 16 bytes
  io.source_length := RegNext(io.sink_length - 128/8)

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
