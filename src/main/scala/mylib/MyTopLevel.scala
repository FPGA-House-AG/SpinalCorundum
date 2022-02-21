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

package mylib

import spinal.core._
import spinal.lib._

import scala.util.Random

object MyTopLevel {
  case class BundleA(aaa : Int) extends Bundle{
    val a = UInt(8 bit)
    val b = Bool()
  }
}

import mylib.MyTopLevel._

case class RGB(channelWidth : Int) extends Bundle{
  val x   = Bool()
}

//val source = Stream(RGB(8))
//val sink   = Stream(RGB(8))
//sink <-< source

//Hardware definition
class MyTopLevel extends Component {
  val io = new Bundle {
    val slave0 = slave Stream new RGB(8)
    val slave1 = slave Stream new RGB(8)
    val master0 = master Stream new RGB(8)
  }
//    val xslave = slave Stream(BundleA(8))
//    val xmaster = master Stream(BundleA(8))

  val source = Stream(RGB(8))
  val sink   = Stream(RGB(8))
  source << sink.s2mPipe().m2sPipe()

  io.slave0 <> sink
  io.master0 <> source

  //io.slave0 <> xslave
  //io.master0 <> xmaster
  //slave << master.s2m()

  //val arbiterLowIdPortFirstFragmentLockInputs =  Vec(slave Stream(Fragment(RGB(8))),3)
  //val arbiterLowIdPortFirstFragmentLockOutput =  master Stream(Fragment(RGB(8)))
  //arbiterLowIdPortFirstFragmentLockOutput << StreamArbiterFactory.lowerFirst.fragmentLock.on(arbiterLowIdPortFirstFragmentLockInputs)
}

//Generate the MyTopLevel's Verilog
object MyTopLevelVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new MyTopLevel)
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