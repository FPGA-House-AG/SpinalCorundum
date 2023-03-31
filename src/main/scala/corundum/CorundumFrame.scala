package corundum

import spinal.core._
import spinal.lib._

// CorundumFrame defines the AXIS bus signals TDATA, TKEEP, TUSER but wrapped
// in the SpinalHDL concept of Stream(), and specifically Stream(Fragment()),
// which is compatible with AXIS.
//
// SpinalHDL library Stream implements ready, valid with a (data) payload.
// The ready and valid signals equal that of AXI TREADY and TVALID.
// Stream(Fragment()) implements framing using a last signal, equalling AXIS TLAST.
// Where SpinalHDL uses Fragment, AXIS uses the 'frame' naming. Sometimes 'packet'
// is used interchangeably.
//
// The signals and interface properties match that of the Corundum PCIe/Ethernet project.
// Each AXIS frame holds one Ethernet frame at the Data Link Layer (known as Layer 2 or L2).
// The total frame size is in the range [64, 1522].
//
// For clearity, a L2 frame means the first field in the frame is the Ethernet destination MAC
// address and the last field is the frame check sequence (FSC).
//
// tdata[TDATA_WIDTH]
// tkeep[TKEEP_WIDTH] where TKEEP_WIDTH === (TDATA_WIDTH / 8)
// tuser[1]
//
// tlast indicates the last word of the AXIS packet (if valid).
// tvalid indicates this word contains valid data (on tdata, tkeep, tuser, tlast)
// tuser indicates the packet must be dropped, this may occur anywhere in a packet (on valid)
// tready is an input indicating the downstream is ready to takes a valid transfer
//
// In accordance with the AXI specification, both source and sink (master and slave) agree that
// when tvalid and tready are both asserted, the AXI word is transferred from source to sink.
//
// The first octet (byte) of the Ethernet frame is transferred in the least significant byte
// of tdata, i.e. tdata[7:0], the second byte in tdata[15:0], etc.
// For each byte that is present in tdata[], a bit is set 1 in tkeep[].
// tkeep[0] corresponds with tdata[7:0], tkeep[1] with tdata[15:8], etc.
//
// A packet is transmitted as one or more AXIS beats, where only the last beat (tlast=1) can
// contain a partially filled data word, where only the most significant bytes are not enabled,
// and corresponding tkeep bits 0.
//
// tkeep thus always starts with tkeep[0]=1, followed by zero or more consecutive tkeep[]=1 bits,
// then possibly followed by tkeep[]=0 bits but only for the tlast=1 word. If tvalid & tlast=0,
// all tkeep[] bits are 1.
//

// Frame or Packet is a bit the same here, often it is called Ethernet Frame
// (because it carries no length field, and is "framed" between special signals)
// and Internet Packet (because the header) holds the length.

// Packet boundaries (TLAST) last signal is added by the Fragment() class
// AXI Stream handshake valid and ready are added by the Stream() class

object CorundumFrame {
  // common methods for building the CorundumFrame bundle
  def apply(dataWidth : Int, userWidth : Int): CorundumFrame = new CorundumFrame(dataWidth, userWidth)
  def apply(dataWidth : Int): CorundumFrame = {
    val frame = CorundumFrame(dataWidth = dataWidth, userWidth = 1)
    frame
  }
  def apply(): CorundumFrame = {
    val frame = CorundumFrame(dataWidth = 512)
    frame
  }

  // Rename SpinalHDL library defaults to AXI naming convention
  def renameAxiIO(io: Bundle): Unit = {
    io.flatten.foreach(bt => {
      if(bt.getName().contains("_payload_fragment")) bt.setName(bt.getName().replace("_payload_fragment", "_tdata"))
      if(bt.getName().contains("_payload_last")) bt.setName(bt.getName().replace("_payload_last", "_tlast"))
      if(bt.getName().contains("_payload"))  bt.setName(bt.getName().replace("_payload",  ""))
      if(bt.getName().contains("_fragment")) bt.setName(bt.getName().replace("_fragment", ""))
      if(bt.getName().contains("_valid"))    bt.setName(bt.getName().replace("_valid",    "_tvalid"))
      if(bt.getName().contains("_ready"))    bt.setName(bt.getName().replace("_ready",    "_tready"))
      if(bt.getName().contains("_last"))     bt.setName(bt.getName().replace("_last",     "_tlast"))
      if(bt.getName().contains("_tdata_"))   bt.setName(bt.getName().replace("_tdata_",     "_"))
      if(bt.getName().contains("reset"))     bt.setName(bt.getName().replace("reset",     "rst"))
      if(bt.getName().startsWith("io_"))     bt.setName(bt.getName().replaceFirst("io_",""))
    })
  }
}

class CorundumFrame(dataWidth : Int, userWidth : Int) extends Bundle {
  val tkeep = Bits(dataWidth/8 bits)
  val tdata = Bits(dataWidth bits)
  val tuser = Bits(userWidth bits)
}

class CorundumPacket(dataWidth : Int, lengthWidth : Int) extends Bundle {
  val payload = Fragment(Bits(dataWidth bits))
  val length = UInt(lengthWidth bits)
}

object CorundumPacket {
  def apply(dataWidth : Int, lengthWidth : Int): CorundumPacket = new CorundumPacket(dataWidth, lengthWidth)
}
