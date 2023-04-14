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
// - If both 'increment' and 'clear' are set, the current lookup results in
// the current counter value, the next lookup will result in valueOnClear.
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
    // must be set together on high priority (non-bus) lookups, with or without increment and clear
    val lookup = in Bool()
    val increment = in Bool()
    val clear = in Bool()
    val address = in UInt(memAddressWidth bits)
    val counter = out UInt(memDataWidth bits)
  }

  val mem = Mem(Bits(memDataWidth bits), wordCount)
  if (initRAM == true) {
    // initialize memory to zero
    mem.initBigInt(Seq.fill(wordCount)(valueOnClear))
  }

  /* memory read latency 2 cycles */
  val d1_lookup  = RegNext(io.increment).init(False)
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
  io.counter := U(RegNext(mem.readSync(io.address, enable = True/*io.increment*/)))

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
  d2_counter := io.counter
  /* or clear counter to write back */
  when (d2_clear) {
    d2_counter := valueOnClear
  /* increment counter to write back */
  } elsewhen (d2_lookup) {
    d2_counter := io.counter + 1
  }
  val do_writeback = d3_valid// & !take_from_d3

  // @TODO why is this readWrite port?
  mem.readWriteSync(d3_address, data = d3_counter.asBits, enable = do_writeback, write = do_writeback)

  // address decoding assumes slave-local addresses
  def driveFrom(busCtrl : BusSlaveFactory) = new Area {
    assert(busCtrl.busDataWidth == 32)

    // bus write address, which addresses a memory word in the memory
    val bus_wr_addr_memory_word = (busCtrl.writeAddress >> 2/*TODO neatly calculate*/).resize(memAddressWidth)

    val writeState = RegInit(U"0")

    val memory_size = wordCount * 4;
    val size_mapping = SizeMapping(0, memory_size)

    // clear pulse only when lookup interface is idle
    val bus_slave_clear_pulse = False
    busCtrl.onWritePrimitive(address = size_mapping, haltSensitive = false, documentation = null) {
      // lookup is idle?
      when (io.lookup === False) {
        bus_slave_clear_pulse := True
      // lookup is busy, pause the write
      } otherwise {
        busCtrl.writeHalt()
      }
    }
    when(bus_slave_clear_pulse) {
      io.clear := True
      io.address := bus_wr_addr_memory_word
    }
  }
}

// companion object for case class
object LookupCounterAxi4 {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new LookupEndpointAxi4(33, 1024, Axi4Config(32, 32, 2, useQos = false, useRegion = false)/*, ClockDomain.external("portb")*/))
    val verilogReport = Config.spinal.generateVerilog(new LookupEndpointAxi4(33, 1024, Axi4Config(32, 32, 2, useQos = false, useRegion = false)/*, ClockDomain.external("portb")*/))
  }
  // https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
  def nextPowerofTwo(x: Int): Int = {
    1 << log2Up(x)
  }

  def slave_width(wordCount : Int, busCfg : Axi4Config) : Int = {
    /* calculate the bus slave address width needed to address the lookup table */
    val bytes_per_cpu_word = busCfg.dataWidth / 8
    val memory_space = wordCount * bytes_per_cpu_word
    val memory_space_address_bits = log2Up(memory_space)

    // the driving bus must have all address bits
    require(busCfg.addressWidth >= memory_space_address_bits)

    memory_space_address_bits
  }
}

case class LookupCounterAxi4(memDataWidth : Int,
                         wordCount : Int,
                         valueOnClear : BigInt = 0,
                         initRAM : Boolean = false,
                         busCfg : Axi4Config
                       /*,lookupCD: ClockDomain*/) extends Component {
  val memAddressWidth = log2Up(wordCount)

  val memory_space_address_bits = LookupCounterAxi4.slave_width(wordCount, busCfg);
  printf("LookupCounterAxi4() requires %d address bits.\n", memory_space_address_bits)
  printf("LookupCounterAxi4() bus configuration has %d address bits.\n", busCfg.addressWidth)

  // the driving bus must have all address bits
  require(busCfg.addressWidth >= memory_space_address_bits)

  // copy AXI4 properties from bus, but override address width for slave
  val slaveCfg = busCfg.copy(addressWidth = memory_space_address_bits)

  val io = new Bundle {
    val ctrlbus = slave(Axi4(slaveCfg))

    // lookup interface
    val lookup = in Bool()
    val increment = in Bool()
    val clear = in Bool()
    val address = in UInt(memAddressWidth bits)
    val counter = out UInt(memDataWidth bits)
  }
  val luc = LookupCounter(memDataWidth, wordCount, valueOnClear, initRAM)
  val ctrl = new Axi4SlaveFactory(io.ctrlbus)

  luc.io.lookup := io.lookup
  luc.io.increment := io.increment
  luc.io.clear := io.clear
  luc.io.address := io.address
  io.counter := luc.io.counter

  // drive low priority clear address bus slave, must come after other io.* := assignments!
  val bridge = luc.driveFrom(ctrl)
}

import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object LookupCounterSim {
  def main(args: Array[String]) : Unit = {
    val memDataWidth = 64
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
      val dut = new LookupCounter(memDataWidth, wordCount,
        valueOnClear = startValue, initRAM = initRAM)
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
        var clear = (Random.nextInt(iterations) >= (iterations - 1))
        // never clear on last quarter of counters
        clear = clear & (address > ((wordCount * 4) / 3))
        dut.io.address #= address
        dut.io.increment #= increment
        dut.io.clear #= clear
        dut.io.lookup #= increment | clear
        if (increment) {
            model(address) += 1
            model(address) %= (BigInt(1) << memDataWidth)
            //printf("value = %d\n", model(address))
        }
        if (clear) {
            model(address) = startValue
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
    val memDataWidth = 64
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
      val dut = new LookupCounterAxi4(memDataWidth, wordCount,
        valueOnClear = startValue, initRAM = initRAM, Axi4Config(32, 32, 2, useQos = false, useRegion = false))
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
              address = Random.nextInt(wordCount)
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
        val clear = (Random.nextInt(iterations) >= (iterations - 1))
        dut.io.address #= address
        dut.io.increment #= increment
        dut.io.clear #= clear
        dut.io.lookup #= increment | clear
        if (increment) {
            model(address) += 1
            model(address) %= (BigInt(1) << memDataWidth)
            //printf("value = %d\n", model(address))
        }
        if (clear) {
            model(address) = startValue
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