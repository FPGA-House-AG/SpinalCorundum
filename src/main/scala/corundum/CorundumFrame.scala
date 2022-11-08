package corundum

import spinal.core._
import spinal.lib._

// this is actually a frame word, intended as part of AXI Stream
// packet boundaries (TLAST) last signal is added by the Fragment() class
// AXI Stream handschake valid and ready are added by the Stream() class
// @TODO rename

object CorundumFrame {
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
      if(bt.getName().contains("_tdata_"))     bt.setName(bt.getName().replace("_tdata_",     "_"))
      if(bt.getName().contains("reset"))     bt.setName(bt.getName().replace("reset",     "rst"))
      if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
    })
  }
}

class CorundumFrame(dataWidth : Int, userWidth : Int) extends Bundle {
  val tkeep = Bits(dataWidth/8 bits)
  val tdata = Bits(dataWidth bits)
  val tuser = Bits(userWidth bits)
}
