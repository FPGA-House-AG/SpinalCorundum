package corundum

import spinal.core._
import spinal.lib._

object StreamFifoWithPipeline {
  def apply[T <: Data](dataType: T, depth: Int) = new StreamFifoWithPipeline(dataType, depth)

  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new StreamFifoWithPipeline(Bits(512 bits), 32)
      toplevel
    })
    val verilogReport = Config.spinal.generateVerilog(new StreamFifoWithPipeline(Bits(512 bits), 32))
  }

  def IsPowerOfTwo(x: BigInt): Boolean = {
    return (x != 0) && ((x & (x - 1)) == 0);
  }
}

// StreamFifo() where the output has .s2mPipe() applied; attempt to use the output register in BRAMs
// and increase timing in general.
class StreamFifoWithPipeline[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  // Maybe require depth to be a power of two, plus 1, so that the FIFO RAM size is a power of two?
  // This is the optimal size for fast FIFO pointer logic (and maybe the only allowed for Gray counters
  // when using StreamFifoCC for clock-domain crossing).
  require(depth >= 3)
  require(StreamFifoWithPipeline.IsPowerOfTwo(depth - 1))
  val io = new Bundle {
    val push = slave Stream (dataType())
    val pop = master Stream (dataType())
    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }
  val fifo_ram = StreamFifo(dataType(), depth)
  fifo_ram.io.push << io.push
  fifo_ram.io.flush := io.flush

  // this adds a register stage
  io.pop <-< fifo_ram.io.pop

  val this_availability = fifo_ram.io.availability + U(!io.pop.valid)
  val this_occupancy = fifo_ram.io.occupancy + U(io.pop.valid)

  io.occupancy := RegNext(this_occupancy).init(0)
  io.availability := RegNext(this_availability).init(depth + 1)

  // Rename SpinalHDL library defaults to AXI naming convention
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

import scala.math.pow

import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object StreamFifoWithPipelineSim {
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
    
    .compile {
      val dut = new StreamFifoWithPipeline(Bits(8 bits), 32 + 1)
      dut
    }
    .doSim { dut =>

      dut.io.flush #= false
      dut.io.push.valid #= false
      dut.io.pop.ready #= false

      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling()

      var remaining = 255
      var valid = false
      var ready = true

      while (remaining >= 0) {
        if (Random.nextInt(256) >= remaining) {
          valid = !valid
        }
        if (Random.nextInt(256) <= remaining) {
            ready = !ready
        }

        dut.io.push.valid #= valid
        dut.io.push.payload #= BigInt(remaining)
        if (valid) {
          remaining -= 1
        }
        dut.io.pop.ready #= ready
        dut.clockDomain.waitRisingEdge()
      }
      dut.io.flush #= false
      dut.io.push.valid #= false
      dut.io.pop.ready #= true
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
    }
  }
}