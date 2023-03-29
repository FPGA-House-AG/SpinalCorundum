package corundum

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._

object LookupEndpointAxi4Sim {
  def main(args: Array[String]) : Unit = {
    val memDataWidth = 32
    val wordCount = 32//1024
    val startValue = 1
    val initRAM = false
    SimConfig
    .withFstWave
    // GHDL can simulate VHDL
    .withGhdl.withFstWave
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
      val dut = new LookupEndpointAxi4(memDataWidth, wordCount, Axi4Config(32, 32, 2, useQos = false, useRegion = false))
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

      val iterations = 2048
      var remaining = wordCount * iterations
      var address = Random.nextInt(wordCount)
      var address_d1 = 0
      var address_d2 = 0
      var lookup_d1 = 0
      var lookup_d2 = 0
      while (remaining > 0) {
        if (Random.nextInt(8) > 6) {
          address = Random.nextInt(wordCount)
        }
        val lookup = (Random.nextInt(8) > 1)
        val update = (Random.nextInt(iterations) >= (iterations - 1))
        dut.io.lookup_addr #= address
        dut.io.lookup #= lookup
        dut.clockDomain.waitRisingEdge()
        remaining -= 1
      }

      dut.io.update #= false
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
    }
  }
}