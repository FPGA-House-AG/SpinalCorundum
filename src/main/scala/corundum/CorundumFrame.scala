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
}

class CorundumFrame(dataWidth : Int, userWidth : Int) extends Bundle {
  val tkeep = Bits(dataWidth/8 bits)
  val tdata = Bits(dataWidth bits)
  val tuser = Bits(userWidth bits)
}
