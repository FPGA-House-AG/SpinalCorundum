package blackwire

import corundum._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._
//import spinal.lib.bus.bram._

import spinal.core._
import spinal.lib._

import scala.math.pow
import java.util.Base64

// companion object
object PacketHeaderConfigure {
  def main(args: Array[String]) : Unit = {
    SpinalVerilog(new PacketHeaderConfigure())
    SpinalVhdl(new PacketHeaderConfigure())
  }
}

case class PacketHeaderConfigure() extends Component {
  final val wordWidth = (14 + 20 + 8) * 8
  val io = new Bundle {
    val header      = out Bits(wordWidth bits)
    val drive_in = in Bits(wordWidth bits)
  }

  io.header := io.drive_in;

  def driveFrom(busCtrl : BusSlaveFactory) = new Area {
    assert(busCtrl.busDataWidth == 32)

    val word = Reg(Bits(wordWidth bits))
    word.init((
        B("112'xaabbcc222222000a3506a3be0800") ##
        B("16'x4500") ## B("16'x0000") ## B("32'x00000000") ## B("32'x08110000") ## B("32'xac100032") ## B("32'xac100001") ##
        B("16'x15b3") ## B("16'x159a") ## B("16'x0000") ## B("16'x0000"/*checksum==unused*/))
        .subdivideIn((14 + 20 + 8) slices).reverse.asBits
    )

    busCtrl.readMultiWord(word, 0x080, documentation = null)
    busCtrl.writeMultiWord(word, 0x080, documentation = null)

    io.drive_in := word
  }
}

// companion object
object PacketHeaderConfigureAxi4 {
  final val slaveAddressWidth = 10
  // generate VHDL and Verilog
  def main(args: Array[String]) : Unit = {
    val vhdlReport = Config.spinal.generateVhdl(new PacketHeaderConfigureAxi4(Axi4Config(32, 32, 2, useQos = false, useRegion = false)))
    val verilogReport = Config.spinal.generateVerilog(new PacketHeaderConfigureAxi4(Axi4Config(32, 32, 2, useQos = false, useRegion = false)))
  }
  def slave_width(busCfg : Axi4Config) : Int = {
    slaveAddressWidth
  }
}

// slave must be naturally aligned
case class PacketHeaderConfigureAxi4(busCfg : Axi4Config) extends Component {
  final val wordWidth = (14 + 20 + 8) * 8

  // copy AXI4 properties from bus, but override address width for slave
  val slaveCfg = busCfg.copy(addressWidth = PacketHeaderConfigureAxi4.slave_width(busCfg))
  
  val io = new Bundle {
    val ctrlbus = slave(Axi4(slaveCfg))
    val header  = out Bits(wordWidth bits)
  }

  val phdr_cfg =  PacketHeaderConfigure()
  val ctrl = new Axi4SlaveFactory(io.ctrlbus)
  val bridge = phdr_cfg.driveFrom(ctrl)

  io.header := phdr_cfg.io.header

  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}


import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._

object PacketHeaderConfigureAxi4Sim {
  def main(args: Array[String]) : Unit = {
    SimConfig
    // GHDL can simulate VHDL
    .withGhdl.withWave
    //.addRunFlag support is now in SpinalHDL dev branch
    .addRunFlag("--unbuffered") //.addRunFlag("--disp-tree=inst")
    .addRunFlag("--ieee-asserts=disable").addRunFlag("--assert-level=none")
    .addRunFlag("--backtrace-severity=warning")
    
    //.withXSim.withXilinxDevice("xcu50-fsvh2104-2-e")
    //.addSimulatorFlag("--ieee=standard")
    //.addSimulatorFlag("-v")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unisim/v93")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unimacro/v93") 
    // these define bus_pkg and bus_pkg1
    .compile {
      val dut = new PacketHeaderConfigureAxi4(Axi4Config(32, 32, 2, useQos = false, useRegion = false))
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    .doSim { dut =>

      dut.io.ctrlbus.w.last #= true
      dut.io.ctrlbus.r.ready #= false
      dut.io.ctrlbus.b.ready #= true
      dut.io.ctrlbus.ar.valid #= false
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      dut.io.ctrlbus.aw.payload.id.assignBigInt(0)
      dut.io.ctrlbus.aw.payload.lock.assignBigInt(0) // normal
      dut.io.ctrlbus.aw.payload.prot.assignBigInt(2) // normal non-secure data access
      dut.io.ctrlbus.aw.payload.burst.assignBigInt(1) // fixed address burst
      dut.io.ctrlbus.aw.payload.len.assignBigInt(0) // 1 beat per burst
      dut.io.ctrlbus.aw.payload.size.assignBigInt(2) // 4 bytes per beat

      dut.io.ctrlbus.ar.payload.id.assignBigInt(0)
      dut.io.ctrlbus.ar.payload.lock.assignBigInt(0) // normal
      dut.io.ctrlbus.ar.payload.prot.assignBigInt(2) // normal non-secure data access
      dut.io.ctrlbus.ar.payload.burst.assignBigInt(1) // fixed address burst
      dut.io.ctrlbus.ar.payload.len.assignBigInt(0) // 1 beat per burst
      dut.io.ctrlbus.ar.payload.size.assignBigInt(2) // 4 bytes per beat

      dut.io.ctrlbus.w.payload.strb.assignBigInt(0xF) // 4 bytes active per beat

      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitSampling()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()

    for (i <- 0 until 11) {
        val address = 0x080 + i * 4
        dut.io.ctrlbus.aw.valid #= true
        dut.io.ctrlbus.aw.payload.addr.assignBigInt(address)
        dut.io.ctrlbus.w.valid #= true
        dut.io.ctrlbus.w.payload.data.assignBigInt(BigInt(address))
        dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
        dut.io.ctrlbus.aw.valid #= false
        dut.io.ctrlbus.w.valid #= false
        dut.clockDomain.waitRisingEdge()
    }
      dut.clockDomain.waitRisingEdge(100)
    }
  }
}
