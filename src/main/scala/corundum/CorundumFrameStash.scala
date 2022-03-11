/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package corundum

import spinal.core._
import spinal.lib._

import scala.util.Random

// companion object
object CorundumFrameStash {
}

case class CorundumFrameStash(dataWidth : Int) extends Component {
  val maxFragmentSize = 16
  val minPackets = 1
  val fifoSize = minPackets * maxFragmentSize
  val io = new Bundle {
    val slave0 = slave Stream new Fragment(CorundumFrame(dataWidth))
    val master0 = master Stream new Fragment(CorundumFrame(dataWidth))
    // worst case each packet is one beat
    val packets = out UInt(log2Up(fifoSize) bit)
    val full = out Bool()
    println(log2Up(fifoSize))
  }
  val fifo = new StreamFifo(Fragment(CorundumFrame(dataWidth)), minPackets * maxFragmentSize)

  // track number of packets in the FIFO
  val packetsInFifoCounter = CounterUpDown(fifoSize, fifo.io.push.ready & fifo.io.push.valid & fifo.io.push.last, fifo.io.pop.ready & fifo.io.pop.valid & fifo.io.pop.last)

  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(dataWidth))
  // fifo source/master/pop port to component source/master port
  val y = Stream Fragment(CorundumFrame(dataWidth))
  // gather at least minPackets packet(s) in the FIFO before continuing the pop/output stream
  // however if the FIFO becomes full, also continue, to prevent corruption
  val z = y.continueWhen((packetsInFifoCounter.value >= minPackets) || (fifo.io.availability < 2)).s2mPipe().m2sPipe()

  io.full := fifo.io.availability < 2

  //fifo.io.push << io.slave0 // @TODO we can remove x, but might want to add stages later
  x << io.slave0
  fifo.io.push << x
  y << fifo.io.pop
  io.master0 << z

  io.packets := packetsInFifoCounter.value
}

// @todo PacketStream FIFO using CounterUpDown(0, in.last & in.fire, out.last & out.fire)
// out.valid := (counter > 0)

//Generate the CorundumFrameStash's Verilog
object CorundumFrameStashVerilog {
//  def main(args: Array[String]) {
//    SpinalVerilog(new CorundumFrameStash)
//  }
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameStash(512)
      XilinxPatch(toplevel)
    })
    config.generateVerilog({
      val toplevel = new CorundumFrameStash(512)
      XilinxPatch(toplevel)
    })
  }
}

//Generate the CorundumFrameStash's VHDL
object CorundumFrameStashVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new CorundumFrameStash(512))
  }
}
