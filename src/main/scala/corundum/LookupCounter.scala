package corundum

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import spinal.core._
import spinal.lib._

import scala.math.pow

// companion object for case class
object LookupCounter {
  // generate VHDL and Verilog
  def main(args: Array[String]) : Unit = {
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new LookupCounter(64, 1024, valueOnClear = 1, initRAM = true)
      // return this
      toplevel
    })
    val verilogReport = Config.spinal.generateVerilog(new LookupCounter(64, 1024, valueOnClear = 1, initRAM = false))
  }
}

// A lookup table with counters, each counter increments after lookup.
// - By setting 'clear' a counter is reset to valueOnClear, which will
// be returned on the next lookup.
// - If both 'lookup' and 'clear' are set, the current lookup results in
// an incremented counter, the next lookup will result in valueOnClear.
// - Overflowing is not yet supported. Overflow will cycle through zero.
// 
// This data structure is similar to a histogram.

// Took 4 hours to design implementation and simulation, re-using
// LookupTable() as a sub component. ~1 LOC/min, Verilog ~equals LOC.
case class LookupCounter(memDataWidth : Int,
                         wordCount : Int,
                         valueOnClear : BigInt = 0,
                         initRAM : Boolean = false
                       /*,lookupCD: ClockDomain*/) extends Component {
  val memAddressWidth = log2Up(wordCount)
  val io = new Bundle {
    val lookup = in Bool()
    val clear = in Bool()
    val address = in UInt(memAddressWidth bits)
    val counter = out UInt(memDataWidth bits)
  }

  val lut = LookupTable(memDataWidth, wordCount)
  if (initRAM == true) {
    // initialize memory to zero
    lut.mem.initBigInt(Seq.fill(wordCount)(valueOnClear))
  }
  /* portA is used as read-only port */
  lut.io.portA.en := True
  lut.io.portA.wr := False
  lut.io.portA.addr := io.address
  lut.io.portA.wrData := 0

  /* memory read latency 2 cycles */
  val d1_lookup  = RegNext(io.lookup).init(False)
  val d1_clear   = RegNext(io.clear).init(False)
  val d1_address = RegNext(io.address)

  /* lookup result is available from memory for d2_address */
  val d2_clear   = RegNext(d1_clear)
  val d2_lookup  = RegNext(d1_lookup)
  val d2_valid   = d2_lookup | d2_clear
  val d2_address = RegNext(d1_address)
  /* d2 is taken from either memory or the to-memory-pipeline */
  val d2_counter = UInt(memDataWidth bits)

  val d3_valid  =  RegNext(d2_valid)
  val d3_address = RegNext(d2_address)
  val d3_counter = RegNext(d2_counter)

  val d4_valid  =  RegNext(d3_valid)
  val d4_address = RegNext(d3_address)
  val d4_counter = RegNext(d3_counter)

  val d5_valid  =  RegNext(d4_valid)
  val d5_address = RegNext(d4_address)
  val d5_counter = RegNext(d4_counter)

  val take_from_memory = True
  // return counter from memory
  io.counter := U(lut.io.portA.rdData)

  val take_from_d3 = False
  val take_from_d4 = False
  val take_from_d5 = False
  // resolve for hazards due to memory read and write latency
  when (d3_valid && (d3_address === d2_address)) {
    io.counter := d3_counter
    take_from_memory := False
    take_from_d3 := d2_valid
  } elsewhen (d4_valid && (d4_address === d2_address)) {
    io.counter := d4_counter
    take_from_memory := False
    take_from_d4 := d2_valid
  } elsewhen (d5_valid && (d5_address === d2_address)) {
    io.counter := d5_counter
    take_from_memory := False
    take_from_d5 := d2_valid
  }
  when (!d2_valid) {
    take_from_memory := False
  }
  /* increment counter to write back */
  d2_counter := io.counter + 1
  /* or clear counter to write back */
  when (d2_clear) {
    d2_counter := valueOnClear
  }

  // d3 has the registered incremented counter written back
  lut.io.portB.en := True
  lut.io.portB.wr := d3_valid
  lut.io.portB.addr := d3_address
  lut.io.portB.wrData := d3_counter.asBits
}

import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object LookupCounterSim {
  def main(args: Array[String]) : Unit = {
    val memDataWidth = 64
    val wordCount = 1024
    val startValue = 1
    val initRAM = true //false
    SimConfig
    .withFstWave
    // GHDL can simulate VHDL
    //.withGhdl.withFstWave
    //.addRunFlag support is now in SpinalHDL dev branch
    //.addRunFlag("--unbuffered") //.addRunFlag("--disp-tree=inst")
    //.addRunFlag("--ieee-asserts=disable").addRunFlag("--assert-level=none")
    //.addRunFlag("--backtrace-severity=warning")
    
    //.withXSim.withXilinxDevice("xcu50-fsvh2104-2-e")
    //.addSimulatorFlag("--ieee=standard")
    //.addSimulatorFlag("-v")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unisim/v93")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unimacro/v93") 
    // these define bus_pkg and bus_pkg1
    .compile {
      val dut = new LookupCounter(memDataWidth, wordCount,
        valueOnClear = startValue, initRAM = initRAM)
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    .doSim { dut =>

      val initValue = if (initRAM) startValue else 0
      var model = collection.mutable.ArrayBuffer.fill(wordCount)(BigInt(initValue))

      dut.io.lookup #= false
      dut.io.clear #= false
      dut.io.address #= 1

      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      // in case RAM is not initialized with the startValue, all addresses
      // must be cleared explicitly by the user
      if ((!initRAM) & (startValue > 0)) {
        printf("Initializing full table with start value %d.\n", startValue)
        // initialize counters
        dut.io.clear #= true
        for (address <- 0 until wordCount.toInt) {
            dut.io.address #= address
            model(address) = startValue
            dut.clockDomain.waitRisingEdge()
        }
        dut.io.clear #= false
        dut.clockDomain.waitRisingEdge()
      } else {
        printf("Assuming full table RAM is pre-initialized with start value %d.\n", startValue)
      }

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
        val clear = (Random.nextInt(iterations) >= (iterations - 1))
        dut.io.address #= address
        dut.io.lookup #= lookup
        dut.io.clear #= clear
        if (lookup) {
            model(address) += 1
            //model(address) %= (BigInt(1) << memDataWidth)
            //printf("value = %d\n", model(address))
        }
        if (clear) {
            model(address) = startValue
        }

        dut.clockDomain.waitRisingEdge()
        remaining -= 1
      }
      dut.io.clear #= false
      dut.io.lookup #= false
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()

      for (address <- 0 until wordCount.toInt) {
        dut.clockDomain.waitRisingEdge()
        dut.io.lookup #= true
        dut.io.address #= address
        dut.clockDomain.waitRisingEdge()
        dut.io.lookup #= false
        dut.clockDomain.waitRisingEdge()
        dut.clockDomain.waitRisingEdge()
        model(address) %= (BigInt(1) << memDataWidth)
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