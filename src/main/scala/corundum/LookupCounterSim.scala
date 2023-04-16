
package corundum

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import spinal.core._
import spinal.lib._

import scala.math.pow

import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object LookupCounterSim {
  def main(args: Array[String]) : Unit = {
    val wordCount = 32//1024
    val startValue = 1
    val endValue = (BigInt(1) << 8) - 1
    val restart = false
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
      val dut = new LookupCounter(wordCount, startValue, endValue, restart, initRAM)
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    .doSim { dut =>

      var model = collection.mutable.ArrayBuffer.fill(wordCount)(BigInt(startValue))

      dut.io.increment #= false
      dut.io.clear #= false
      dut.io.address #= 1

      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      // in case RAM is not initialized with the startValue, all addresses
      // must be cleared explicitly by the user
      if (!initRAM) {
        printf("Initializing each RAM entry with start value %d.\n", startValue)
        // initialize counters
        dut.io.clear #= true
        dut.io.lookup #= true
        for (address <- 0 until wordCount.toInt) {
            dut.io.address #= address
            model(address) = startValue
            dut.clockDomain.waitRisingEdge()
        }
        dut.io.clear #= false
        dut.io.lookup #= false
        dut.clockDomain.waitRisingEdge(10)
      } else {
        printf("Assuming full table RAM is pre-initialized with start value %d.\n", startValue)
      }

      val iterations = 2048 * 8
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
        val increment = (Random.nextInt(8) > 1)
        var clear = (Random.nextInt(iterations) >= (iterations * 50 / 100))
        // never clear on last quarter of counters, to test reaching endValue
        clear = clear & (address < ((wordCount * 3) / 4))
        dut.io.address #= address
        dut.io.increment #= increment
        dut.io.clear #= clear
        dut.io.lookup #= increment | clear | (Random.nextInt(8) > 3)

        // update model
        if (clear) {
            model(address) = startValue
        }
        else if (increment) {
            if (model(address) < endValue) { 
              model(address) += 1
            } else {
              if (restart) {
                model(address) = startValue
              }
            }
        }

        dut.clockDomain.waitRisingEdge()
        remaining -= 1
      }
      dut.io.clear #= false
      dut.io.increment #= false
      dut.io.lookup #= false
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()

      for (address <- 0 until wordCount.toInt) {
        dut.clockDomain.waitRisingEdge()
        dut.io.increment #= false
        dut.io.lookup #= true
        dut.io.address #= address
        dut.clockDomain.waitRisingEdge()
        dut.io.increment #= false
        dut.io.lookup #= false
        dut.clockDomain.waitRisingEdge()
        dut.clockDomain.waitRisingEdge()
        //model(address) %= (BigInt(1) << memDataWidth)
        if (dut.io.counter.toBigInt != model(address)) {
            printf("address = %d, counter = %d, expected = %d (FAIL)\n",
                address, dut.io.counter.toBigInt, model(address))
        }
        else if ((address % (wordCount.toInt / 8)) == 0) {
            printf("address = %d, counter = %d, expected = %d (SUCCESS)\n",
                address, dut.io.counter.toBigInt, model(address))
        }
        assert(dut.io.counter.toBigInt == model(address))
      }
    }
  }
}

object LookupCounterAxi4Sim {
  def main(args: Array[String]) : Unit = {
    val wordCount = 32//1024
    val startValue = 0
    val endValue = 1 // (BigInt(1) << 12) - 1
    val restart = false
    val initRAM = false
    printf("LookupCounterAxi4Sim: endValue = %d\n", endValue)
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
      val dut = new LookupCounterAxi4(wordCount,
        startValue, endValue, restart, initRAM, Axi4Config(32, 32, 2, useQos = false, useRegion = false))
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    .doSim { dut =>

      var model = collection.mutable.ArrayBuffer.fill(wordCount)(BigInt(startValue))

      // initialize AXI4 bus
      dut.io.ctrlbus.w.last #= true
      dut.io.ctrlbus.r.ready #= false
      dut.io.ctrlbus.b.ready #= true
      dut.io.ctrlbus.ar.valid #= false
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      // initialize AXI4 bus with static values
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

      dut.io.increment #= false
      dut.io.clear #= false
      dut.io.address #= 1

      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      // in case RAM is not initialized with the startValue, all addresses
      // must be cleared explicitly by the user
      if (!initRAM) {
        printf("Initializing each RAM entry with start value %d.\n", startValue)
        // initialize counters
        dut.io.clear #= true
        dut.io.lookup #= true
        for (address <- 0 until wordCount.toInt) {
            dut.io.address #= address
            model(address) = startValue
            dut.clockDomain.waitRisingEdge()
        }
        dut.io.clear #= false
        dut.io.lookup #= false
        dut.clockDomain.waitRisingEdge(10)
      } else {
        printf("Assuming full table RAM is pre-initialized with start value %d.\n", startValue)
      }

      printf("Starting torture test...\n");

      // prevent write thread from interfering with read-back of final counters, use done
      var lookups_done = false;

      val iterations = 2048 * 4

      // writes to component to clear counters
      val writeThread = fork {
        var remaining = wordCount * iterations
        var address = Random.nextInt(wordCount)
        var writes = 0;
        while ((remaining > 0) && !lookups_done/* && (writes < 1)*/) {
            if ((Random.nextInt(iterations) >= (iterations * 15 / 16))) {
              address = Random.nextInt(wordCount * 3 / 4)
              dut.io.ctrlbus.aw.valid #= true
              dut.io.ctrlbus.aw.payload.addr.assignBigInt(address * 4)
              dut.io.ctrlbus.w.valid #= true
              dut.io.ctrlbus.w.payload.data.assignBigInt(BigInt("0DEADBEEF", 16) + address)
              dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
              // update model
              model(address) = startValue
              dut.io.ctrlbus.aw.valid #= false
              dut.io.ctrlbus.w.valid #= false
              writes += 1
            }
            dut.clockDomain.waitRisingEdge()
            remaining -= 1
        }
      }

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
        val increment = (Random.nextInt(8) > 1)
        var clear = (Random.nextInt(iterations) >= (iterations * 9 / 10))
        clear = clear & (address < ((wordCount * 3) / 4))
        clear = false

        dut.io.address #= address
        dut.io.increment #= increment
        dut.io.clear #= clear
        dut.io.lookup #= increment | clear | (Random.nextInt(8) > 3)
        if (clear) {
            model(address) = startValue
        }
        else if (increment) {
            if (model(address) < endValue) { 
              model(address) += 1
            } else {
              if (restart) {
                model(address) = startValue
              }
            }
        }
        dut.clockDomain.waitRisingEdge()
        remaining -= 1
      }
      // to signal write-thread to stop
      lookups_done = true

      // stop lookup activity
      dut.io.clear #= false
      dut.io.increment #= false
      dut.io.lookup #= false
      // allow last bus controller write to complete
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()

      // read back final counters and verify against model
      for (address <- 0 until wordCount.toInt) {
        dut.clockDomain.waitRisingEdge()
        //dut.io.increment #= true
        dut.io.address #= address
        dut.clockDomain.waitRisingEdge()
        dut.io.increment #= false
        dut.clockDomain.waitRisingEdge()
        dut.clockDomain.waitRisingEdge()
        //model(address) %= (BigInt(1) << memDataWidth)
        if (dut.io.counter.toBigInt != model(address)) {
            printf("address = %d, counter = %d, expected = %d (FAIL)\n",
                address, dut.io.counter.toBigInt, model(address))
        }
        else if ((address % (wordCount.toInt / 8)) == 0) {
            printf("address = %d, counter = %d, expected = %d (SUCCESS)\n",
                address, dut.io.counter.toBigInt, model(address))
        }
        assert(dut.io.counter.toBigInt == model(address))
      }
    }
  }
}