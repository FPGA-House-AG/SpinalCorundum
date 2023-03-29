package corundum

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._

object LookupEndpointAxi4Sim {
  def main(args: Array[String]) : Unit = {
    val memDataWidth = 32/*IPv4 address*/ + 16/*UDP port number*/
    val wordCount = 4
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

      val lookupThread = fork {

        val iterations = 512
        var remaining = wordCount * iterations
        var address = Random.nextInt(wordCount)
        var address_d1 = 0
        var address_d2 = 0
        var lookup = false
        var lookup_d1 = 0
        var lookup_d2 = 0
        while (remaining > 0) {
            if (Random.nextInt(8) > 6) {
              lookup = (Random.nextInt(8) > 4)
              address = Random.nextInt(wordCount)
            }
            dut.io.lookup_addr #= address
            dut.io.lookup #= lookup
            dut.clockDomain.waitRisingEdge()
            remaining -= 1
        }
      }

      val updateThread = fork {

        val iterations = 512
        var remaining = wordCount * iterations
        var address = Random.nextInt(wordCount)
        var address_d1 = 0
        var address_d2 = 0
        var lookup_d1 = 0
        var lookup_d2 = 0
        var update = false
        while (remaining > 0) {
            if (Random.nextInt(8) > 6) {
              update = (Random.nextInt(8) > 6)
              address = Random.nextInt(wordCount)
            }
            dut.io.update #= update
            dut.io.update_addr #= address
            dut.io.update_data #= address
            dut.clockDomain.waitRisingEdge()
            remaining -= 1
        }
      }

      val writeThread = fork {

        val iterations = 512
        var remaining = wordCount * iterations
        var address = Random.nextInt(wordCount)
        var address_d1 = 0
        var address_d2 = 0
        var lookup_d1 = 0
        var lookup_d2 = 0
        while (remaining > 0) {
            if (Random.nextInt(8) > 6) {
              address = Random.nextInt(wordCount)
              dut.io.ctrlbus.aw.valid #= true
              dut.io.ctrlbus.aw.payload.addr.assignBigInt(address * 8)
              dut.io.ctrlbus.w.valid #= true
              dut.io.ctrlbus.w.payload.data.assignBigInt(BigInt("0AABB0000", 16) + address)
              dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
              dut.io.ctrlbus.aw.valid #= false
              dut.io.ctrlbus.w.valid #= false

              dut.io.ctrlbus.aw.valid #= true
              dut.io.ctrlbus.aw.payload.addr.assignBigInt(address * 8 + 4)
              dut.io.ctrlbus.w.valid #= true
              dut.io.ctrlbus.w.payload.data.assignBigInt(address)
              dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
              dut.io.ctrlbus.aw.valid #= false
              dut.io.ctrlbus.w.valid #= false
            }
            dut.clockDomain.waitRisingEdge()
            remaining -= 1
        }
      }

      val readThread = fork {

        val iterations = 512
        var remaining = wordCount * iterations
        var address = Random.nextInt(wordCount)
        var address_d1 = 0
        var address_d2 = 0
        var lookup_d1 = 0
        var lookup_d2 = 0
        while (remaining > 0) {
            if (Random.nextInt(8) > 6) {
              address = Random.nextInt(wordCount)
              dut.io.ctrlbus.r.ready #= true
              dut.io.ctrlbus.ar.valid #= true
              dut.io.ctrlbus.ar.payload.addr.assignBigInt(address * 8)
              dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.ar.ready.toBoolean)
              dut.io.ctrlbus.ar.valid #= false

              dut.io.ctrlbus.ar.valid #= true
              dut.io.ctrlbus.ar.payload.addr.assignBigInt(address * 8 + 4)
              dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.ar.ready.toBoolean)
              dut.io.ctrlbus.ar.valid #= false

            }
            dut.clockDomain.waitRisingEdge()
            remaining -= 1
        }
      }


      dut.io.update #= false
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge(2048)
    }
  }
}