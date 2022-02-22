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

case class CorundumFrame(dataWidth : Int) extends Bundle {
  val tkeep = UInt(dataWidth/8 bit)
  val tdata = UInt(dataWidth bit)
}

// companion object
object MyTopLevel {
//  case class BundleA(widthBits : Int) extends Bundle {
//    val tkeep = UInt(aaa/8 bit)
//    val tdata = UInt(aaa bit)
//    val b = Bool()
//  }

}

import corundum.MyTopLevel._

//val source = Stream(RGB(8))
//val sink   = Stream(RGB(8))
//sink <-< source

//Hardware definition

// multiplexes two packet streams (Stream(Fragment) with lock), first port has priority
class MyTopLevel extends Component {
  val io = new Bundle {
    val slave0 = slave Stream Fragment(CorundumFrame(8))
    val slave1 = slave Stream Fragment(CorundumFrame(8))
    val master0 = master Stream Fragment(CorundumFrame(8))
  }
//    val xslave = slave Stream(BundleA(8))
//    val xmaster = master Stream(BundleA(8))

  //val source = Stream(Fragment(CorundumFrame(8)))
  //val sink   = Stream(Fragment(CorundumFrame(8)))
  // skid buffer
  //source << sink.s2mPipe().m2sPipe()

  //io.slave0 <> sink
  //io.master0 <> source

  //io.slave0 <> xslave
  //io.master0 <> xmaster
  //slave << master.s2m()

  val arbiterLowIdPortFirstFragmentLockInputs =  Vec(io.slave0.s2mPipe().m2sPipe(), io.slave1.s2mPipe().m2sPipe())
  //val arbiterLowIdPortFirstFragmentLockOutput =  master Stream(CorundumFrame(RGB(8)))
  io.master0 << StreamArbiterFactory.lowerFirst.fragmentLock.on(arbiterLowIdPortFirstFragmentLockInputs)

  noIoPrefix()
}

object FrameSpecRenamer{
  def apply[T <: Bundle with CorundumFrame](that : T): T ={
    def doIt = {
      that.flatten.foreach((bt) => {
        bt.setName(bt.getName().replace("_payload_",""))
        bt.setName(bt.getName().replace("_valid","valid"))
        bt.setName(bt.getName().replace("_ready","ready"))
        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
      })
    }
    if(Component.current == that.component)
      that.component.addPrePopTask(() => {doIt})
    else
      doIt

    that
  }
}

// https://gitter.im/SpinalHDL/SpinalHDL?at=5c2297c28d31aa78b1f8c969
object XilinxPatch {
  def apply[T <: Component](c : T) : T = {
    //Get the io bundle via java reflection
    val m = c.getClass.getMethod("io")
    val io = m.invoke(c).asInstanceOf[Bundle]
    println(m);

    //Patch things
    io.elements.map(_._2).foreach {
      
      //case axi : AxiLite4 => AxiLite4SpecRenamer(axi)
      //case axi : Axi4 => Axi4SpecRenamer(axi)
      case axi : CorundumFrame => FrameSpecRenamer(axi)
      case _ => println("unknown")
    }

    //Builder pattern return the input argument
    c 
  }
}

//Generate the MyTopLevel's Verilog
object MyTopLevelVerilog {
//  def main(args: Array[String]) {
//    SpinalVerilog(new MyTopLevel)
//  }
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new MyTopLevel
      XilinxPatch(toplevel)
    })
  }
}

//Generate the MyTopLevel's VHDL
object MyTopLevelVhdl {
  def main(args: Array[String]) {

    SpinalVhdl(new MyTopLevel)
  }
}


//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the MyTopLevel's Verilog using the above custom configuration.
object MyTopLevelVerilogWithCustomConfig {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new MyTopLevel)
  }
}

class FragmentStash extends Component {
  val maxFragmentSize = 16
  val minPackets = 2
  val fifoSize = minPackets * maxFragmentSize
  val io = new Bundle {
    val slave0 = slave Stream new Fragment(CorundumFrame(8))
    val master0 = master Stream new Fragment(CorundumFrame(8))
    // worst case each packet is one beat
    val packets = out UInt(log2Up(fifoSize) bit)
    println(log2Up(fifoSize))
  }
  val fifo = new StreamFifo(Fragment(CorundumFrame(8)), minPackets * maxFragmentSize)
  // track number of packets in the FIFO
  val packetsInFifoCounter = CounterUpDown(fifoSize, fifo.io.push.ready & fifo.io.push.valid & fifo.io.push.last, fifo.io.pop.ready & fifo.io.pop.valid & fifo.io.pop.last)

  // component sink/slave port to fifo push/sink/slave port
  val x = Stream Fragment(CorundumFrame(8))
  // fifo source/master/pop port to component source/master port
  val y = Stream Fragment(CorundumFrame(8))
  val z = y.continueWhen(packetsInFifoCounter.value >= minPackets).stage()

  x << io.slave0
  fifo.io.push << x
  fifo.io.pop >> y
  z >> io.master0

  io.packets := packetsInFifoCounter.value
}

// @todo PacketStream FIFO using CounterUpDown(0, in.last & in.fire, out.last & out.fire)
// out.valid := (counter > 0)
