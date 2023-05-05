package corundum

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import spinal.core._
import spinal.lib._

import scala.math.pow

// ((1 << 64) - 1) = 18446744073709551615

// companion object for case class
object LookupCounter {
  // generate VHDL and Verilog
  def main(args: Array[String]) : Unit = {
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new LookupCounter(1024, startValue = 1, endValue = (BigInt(1) << 64) - 1, restart = false, initRAM = true)
      // return this
      toplevel
    })
    val verilogReport = Config.spinal.generateVerilog(new LookupCounter(1024, startValue = 1, endValue = (BigInt(1) << 64) - 1, restart = false, initRAM = true))
  }

  def word_width(endValue : BigInt) : Int = {
    val counterWidth = if (endValue==1) 1 else log2Up(endValue)
    counterWidth
  }
}

// A lookup table with counters.
// A lookup will return the current counter value.
//
// - By setting 'increment' a counter will increment after lookup, except
//   when the counter value was endValue. Then either it will reset to startValue
//   if 'restart' is true, or it will remain at endValue otherwise.
//
// - By setting 'clear' a counter is reset to startValue, which will
// be returned on the next lookup.
// - If both 'increment' and 'clear' are set, the current lookup results in
// the current counter value, the next lookup will result in startValue.
//
// This data structure is similar to a histogram.

// Took 4 hours to design implementation and simulation, re-using
// LookupTable() as a sub component. ~1 LOC/min, Verilog ~equals LOC.

// 85c0b78507526d4106ea1620664b48af3cfaf3f5 stable
//

// The cycles in LookupCounter are as follows:
// 
//     register all inputs, which includes: register the input address for the memory, (d1)
//     register the memory data output, register the registered inputs (d2)
//     drive the output, calculate new state based on memory data output and registered inputs (d2)
//     register the state (d3)
//     register data address for writing (d4)
//     remember written data (d5)
// 
// Because writing the data also takes some time, steps 1,2 can potentially "race"
// against 3,4,5, but only in the case the same address (WireGuard session index) is used.
// This is called a "pipeline data hazard".To fix the hazard (without losing too much performance)
// is to check if the address is identical to the address that is being written, and if so, take
// the "old state" data not from memory but from the pipeline.

case class LookupCounter(wordCount : Int,
                         startValue : BigInt = 0,
                         endValue : BigInt = (BigInt(1) << 64) - 1,
                         restart : Boolean = false,
                         initRAM : Boolean = false
                       /*,lookupCD: ClockDomain*/) extends Component {
  val memAddressWidth = log2Up(wordCount)
  val counterWidth = LookupCounter.word_width(endValue)
  printf("LookupCounter() startValue = %d\n", startValue)
  printf("LookupCounter() endValue = %s\n", endValue.toString)
  printf("counterWidth() = %d\n", counterWidth)
  require(startValue < endValue)
  val io = new Bundle {
    // must be set together on high priority (non-bus) lookups, with or without increment and clear
    val lookup = in Bool()
    val increment = in Bool()
    val clear = in Bool()
    val address = in UInt(memAddressWidth bits)
    val counter = out UInt(counterWidth bits)
  }

  val mem = Mem(Bits(counterWidth bits), wordCount)
  if (initRAM == true) {
    // initialize memory to zero
    mem.initBigInt(Seq.fill(wordCount)(startValue))
  }

  /* memory read latency 2 cycles */
  val d1_lookup  = RegNext(io.lookup).init(False)
  val d1_incr    = RegNext(io.increment).init(False)
  val d1_clear   = RegNext(io.clear).init(False)
  val d1_address = RegNext(io.address)

  /* lookup result is available from memory for d2_address */
  val d2_clear   = RegNext(d1_clear)
  val d2_lookup  = RegNext(d1_lookup)
  val d2_incr    = RegNext(d1_incr)
  val d2_valid   = d2_lookup | d2_clear
  val d2_address = RegNext(d1_address)
  /* d2 is taken from either memory or the to-memory-pipeline */
  val d2_counter = UInt(counterWidth bits)

  /* the to-memory pipeline, which remembers what was being written to memory */
  val d3_valid  =  RegNext(d2_valid)
  val d3_address = RegNext(d2_address)
  val d3_counter = RegNext(d2_counter)

  val d4_valid  =  RegNext(d3_valid)
  val d4_address = RegNext(d3_address)
  val d4_counter = RegNext(d3_counter)

  val d5_valid  =  RegNext(d4_valid)
  val d5_address = RegNext(d4_address)
  val d5_counter = RegNext(d4_counter)

  // by default, take counter (for output) from memory
  val take_from_memory = True
  io.counter := U(RegNext(mem.readSync(io.address, enable = True/*io.increment*/)))

  // now resolve for hazards due to memory read and write latency
  val take_from_d3 = False
  val take_from_d4 = False
  val take_from_d5 = False
  // we wrote the data to the d2_address 1 cycle earlier?
  when (d3_valid && (d3_address === d2_address)) {
    // fetch state from d3 instead of from memory, because the memory is not yet updated
    io.counter := d3_counter
    take_from_memory := False
    take_from_d3 := d2_valid
  // we wrote the data to the d2_address 2 cycles earlier?
  } elsewhen (d4_valid && (d4_address === d2_address)) {
    // fetch state from d4 instead of from memory, because the memory is not yet updated
    io.counter := d4_counter
    take_from_memory := False
    take_from_d4 := d2_valid
  // we wrote the data to the d2_address 3 cycles earlier?
  } elsewhen (d5_valid && (d5_address === d2_address)) {
    // fetch state from d5 instead of from memory, because the memory is not yet updated
    io.counter := d5_counter
    take_from_memory := False
    take_from_d5 := d2_valid
  }
  // the same counter we output, must now optionally be updated (cleared, incremented, restarted)
  d2_counter := io.counter

  val atEndValue = io.counter === U(endValue).resize(counterWidth)

  /* clear counter to write back? */
  when (d2_clear) {
    d2_counter := startValue
  /* increment counter to write back? */
  } elsewhen (d2_incr) {
      when (!atEndValue) {
        d2_counter := io.counter + 1
      // either stop or restart counter
      } otherwise {
        if (restart) {
          d2_counter := startValue
        }
      }
  }
  val do_writeback = d3_valid// & !take_from_d3

  // @TODO why is this readWrite port?
  mem.readWriteSync(d3_address, data = d3_counter.asBits, enable = do_writeback, write = do_writeback)

  def nextPowerofTwo(x: Int): Int = {
    1 << log2Up(x)
  }

  // address decoding assumes slave-local addresses
  def driveFrom(busCtrl : BusSlaveFactory) = new Area {
    assert(busCtrl.busDataWidth == 32)
    val bytes_per_cpu_word = busCtrl.busDataWidth / 8

    // for one memory word, calculate how many CPU words must be written
    val bus_words_per_memory_word = (counterWidth + busCtrl.busDataWidth - 1) / busCtrl.busDataWidth
    printf("bus_words_per_memory_word    = %d (CPU writes needed to write one word into lookup table)\n", bus_words_per_memory_word)
    // for one memory word, calculate number of CPU words in the address space 
    // it is rounded up to the next power of two, so it will be 1, 2, 4, 8, 16 etc.
    val cpu_words_per_memory_word = nextPowerofTwo(bus_words_per_memory_word)
    val bytes_per_memory_word = cpu_words_per_memory_word * bytes_per_cpu_word
    val bytes_to_cpu_word_shift = log2Up(bytes_per_cpu_word)
    val bytes_to_memory_word_shift = log2Up(bytes_per_memory_word)
    val cpu_word_to_memory_word_shift = bytes_to_memory_word_shift - bytes_to_cpu_word_shift
    printf("cpu_words_per_memory_word    = %d (CPU words reserved per lookup table word)\n", cpu_words_per_memory_word)
    printf("bytes_to_cpu_word_shift      = %d (bits to strip off)\n", bytes_to_cpu_word_shift)
    printf("bytes_to_memory_word_shift   = %d (bits to strip off)\n", bytes_to_memory_word_shift)
    printf("cpu_word_to_memory_word_shift= %d (bits to strip off)\n", cpu_word_to_memory_word_shift)

    printf("memory_words                 = %d\n", wordCount)

    // this is the address space exposed on the control bus
    val memory_size = wordCount * cpu_words_per_memory_word * bytes_per_cpu_word
    printf("memory space size = %d (0x%x) bytes, addr = %d bits\n", memory_size, memory_size, log2Up(memory_size))

    // this is the address space exposed on the control bus
    val bus_wr_addr_memory_word = (busCtrl.writeAddress >> bytes_to_memory_word_shift).resize(memAddressWidth)
    printf("memory space size = %d (0x%x) bytes, addr = %d bits\n", memory_size, memory_size, log2Up(memory_size))

    val writeState = RegInit(U"0")

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
    when (bus_slave_clear_pulse) {
      io.clear := True
      io.address := bus_wr_addr_memory_word
    }
  }
}

// companion object for case class
object LookupCounterAxi4 {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl(new LookupCounterAxi4(1024, 1, (BigInt(1) << 64) - 1, false, true, Axi4Config(32, 32, 2, useQos = false, useRegion = false)/*, ClockDomain.external("portb")*/))
    val verilogReport = Config.spinal.generateVerilog(new LookupCounterAxi4(1024, 1, (BigInt(1) << 64) - 1, false, true, Axi4Config(32, 32, 2, useQos = false, useRegion = false)/*, ClockDomain.external("portb")*/))
  }
  // https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
  def nextPowerofTwo(x: Int): Int = {
    1 << log2Up(x)
  }

  def slave_width(wordCount : Int, endValue : BigInt, busCfg : Axi4Config) : Int = {
    val counterWidth = LookupCounter.word_width(endValue)
    /* calculate the bus slave address width needed to address the lookup table */
    val bytes_per_cpu_word = busCfg.dataWidth / 8
    val bus_words_per_memory_word = (counterWidth + busCfg.dataWidth - 1) / busCfg.dataWidth
    val cpu_words_per_memory_word = nextPowerofTwo(bus_words_per_memory_word)
    val bytes_per_memory_word = cpu_words_per_memory_word * bytes_per_cpu_word
    val memory_space = wordCount * bytes_per_memory_word
    val memory_space_address_bits = log2Up(memory_space)

    // the driving bus must have all address bits
    require(busCfg.addressWidth >= memory_space_address_bits)

    memory_space_address_bits
  }
}

case class LookupCounterAxi4(
                         wordCount : Int,
                         startValue : BigInt = 0,
                         endValue : BigInt = (BigInt(1) << 64) - 1,
                         initRAM : Boolean = false,
                         restart : Boolean = false,
                         busCfg : Axi4Config
                       /*,lookupCD: ClockDomain*/) extends Component {
  val memAddressWidth = log2Up(wordCount)
  val counterWidth = LookupCounter.word_width(endValue)

  val memory_space_address_bits = LookupCounterAxi4.slave_width(wordCount, endValue, busCfg);
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
    val counter = out UInt(counterWidth bits)
  }
  val luc = LookupCounter(wordCount, startValue, endValue, restart, initRAM)
  val ctrl = new Axi4SlaveFactory(io.ctrlbus)

  luc.io.lookup := io.lookup
  luc.io.increment := io.increment
  luc.io.clear := io.clear
  luc.io.address := io.address
  io.counter := luc.io.counter

  // drive low priority clear address bus slave, must come after other io.* := assignments!
  val bridge = luc.driveFrom(ctrl)
}
