package corundum

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import spinal.core._
import spinal.lib._

import scala.math.pow
import java.util.Base64


// @NOTE: Seems USER_WIDTH must be > 0, even when USER_ENABLE == 0
// @NOTE: Seems DEPTH > KEEP_WIDTH, maybe even by a positive integer
class axis_async_fifo(depth: Int = 8, pushCD: ClockDomain, popCD: ClockDomain,
  DATA_WIDTH : Int = 512, USER_WIDTH : Int = 1, USER_ENABLE: Int = 1,
  KEEP_ENABLE : Int = 1, LAST_ENABLE : Int = 1) extends BlackBox {

  val KEEP_WIDTH = if (KEEP_ENABLE > 0) ((DATA_WIDTH+7)/8) else 1

  addGeneric("DATA_WIDTH", DATA_WIDTH)
  addGeneric("DEPTH", depth)
  addGeneric("KEEP_ENABLE", KEEP_ENABLE)
  addGeneric("LAST_ENABLE", LAST_ENABLE)
  addGeneric("USER_WIDTH", USER_WIDTH)
  addGeneric("USER_ENABLE", USER_ENABLE)
  val ID_WIDTH = 8
  val DEST_WIDTH = 8
  addGeneric("ID_WIDTH", ID_WIDTH)
  addGeneric("DEST_WIDTH", DEST_WIDTH)
  addGeneric("ID_ENABLE", 0)
  addGeneric("DEST_ENABLE", 0)

  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    /*
     * AXI input
     */
    val s_clk = in Bool()
    val s_rst = in Bool()
    val s_axis_tdata = in Bits(DATA_WIDTH bits)
    val s_axis_tkeep = in Bits(KEEP_WIDTH bits)
    val s_axis_tvalid = in Bool()
    val s_axis_tready = out Bool()
    val s_axis_tlast = in Bool()
    val s_axis_tid = in Bits(ID_WIDTH bits)
    val s_axis_tdest = in Bits(DEST_WIDTH bits)
    val s_axis_tuser = in Bits(USER_WIDTH bits)

    /*
     * AXI output
     */
    val m_clk = in Bool()
    val m_rst = in Bool()
    val m_axis_tdata = out Bits(DATA_WIDTH bits)
    val m_axis_tkeep = out Bits(KEEP_WIDTH bits)
    val m_axis_tvalid = out Bool()
    val m_axis_tready = in Bool()
    val m_axis_tlast = out Bool()
    val m_axis_tid = out Bits(ID_WIDTH bits)
    val m_axis_tdest = out Bits(DEST_WIDTH bits)
    val m_axis_tuser = out Bits(USER_WIDTH bits)

    /*
     * Status
     */
    val s_status_overflow = out Bool()
    val s_status_bad_frame = out Bool()
    val s_status_good_frame = out Bool()
    val m_status_overflow = out Bool()
    val m_status_bad_frame = out Bool()
    val m_status_good_frame = out Bool()
  }
  mapClockDomain(pushCD, clock = io.s_clk, reset = io.s_rst)
  mapClockDomain(popCD,  clock = io.m_clk, reset = io.m_rst)

  noIoPrefix()
}

// companion object
object CorundumFrameCC {
  def main(args: Array[String]) : Unit = {
    SpinalVerilog(new CorundumFrameCC(8, ClockDomain.current, ClockDomain.current))
    SpinalVhdl(new CorundumFrameCC(8, ClockDomain.current, ClockDomain.current))
  }
}
case class CorundumFrameCC(val depth: Int,
                           val pushClock: ClockDomain,
                           val popClock: ClockDomain,
                           val dataWidth: Int = 512
                           ) extends Component {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val push =    slave Stream Fragment(CorundumFrame(dataWidth))
    val pop = master Stream Fragment(CorundumFrame(dataWidth))
  }
  val verilog = new axis_async_fifo(depth, pushClock, popClock, dataWidth)

  verilog.io.s_axis_tdata  := io.push.fragment.tdata
  verilog.io.s_axis_tkeep  := io.push.fragment.tkeep
  verilog.io.s_axis_tuser  := io.push.fragment.tuser
  verilog.io.s_axis_tvalid := io.push.valid
  verilog.io.s_axis_tlast  := io.push.last
  verilog.io.s_axis_tid    := 0
  verilog.io.s_axis_tdest  := 0
  io.push.ready             := verilog.io.s_axis_tready

  io.pop.fragment.tdata := verilog.io.m_axis_tdata
  io.pop.fragment.tkeep := verilog.io.m_axis_tkeep
  io.pop.fragment.tuser := verilog.io.m_axis_tuser
  io.pop.valid          := verilog.io.m_axis_tvalid
  io.pop.last           := verilog.io.m_axis_tlast
  verilog.io.m_axis_tready := io.pop.ready

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

// companion object
object KeyStreamCC {
  def main(args: Array[String]) : Unit = {
    SpinalVerilog(new KeyStreamCC(8, ClockDomain.current, ClockDomain.current))
    SpinalVhdl(new KeyStreamCC(8, ClockDomain.current, ClockDomain.current))
  }
}
case class KeyStreamCC(val depth: Int = 8, val pushClock: ClockDomain,
                               val popClock: ClockDomain,
                               val dataWidth: Int = 256
                               ) extends Component {
  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val push = slave Stream Bits(dataWidth bits)
    val pop  = master Stream Bits(dataWidth bits)
  }
  val verilog = new axis_async_fifo(depth, pushClock, popClock,
    DATA_WIDTH = dataWidth, USER_WIDTH = 1, USER_ENABLE = 0, KEEP_ENABLE = 0, LAST_ENABLE = 0)

  verilog.io.s_axis_tdata  := io.push.payload
  verilog.io.s_axis_tvalid := io.push.valid
  verilog.io.s_axis_tlast  := True
  verilog.io.s_axis_tkeep  := 0
  verilog.io.s_axis_tuser  := 0
  verilog.io.s_axis_tid    := 0
  verilog.io.s_axis_tdest  := 0
  io.push.ready            := verilog.io.s_axis_tready

  io.pop.payload        := verilog.io.m_axis_tdata
  io.pop.valid          := verilog.io.m_axis_tvalid
  verilog.io.m_axis_tready := io.pop.ready

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

import spinal.sim._
import spinal.core.sim._
import scala.util.Random

object CorundumFrameCCSim {
  def main(args: Array[String]) : Unit = {

    SimConfig
    .withWave
    .addRtl(s"../corundum.rx.tx/fpga/lib/eth/lib/axis/rtl/axis_async_fifo.v")

    .compile {
      val dut = new CorundumFrameCC(128, ClockDomain.current, ClockDomain.current)
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    .doSim { dut =>

      SimTimeout(10000)

      dut.io.push.valid #= false
      dut.io.pop.ready #= false

      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge(10)
      var beat_idx = 0
      var beat_num = 30

      while (beat_idx < beat_num) {
        dut.io.push.valid #= (Random.nextInt(16) >= 8)
        dut.io.push.last #= (Random.nextInt(16) >= 8)
        dut.io.pop.ready #= (Random.nextInt(16) >= 8)
        dut.io.push.fragment.tdata #= BigInt(beat_idx)
        if (dut.io.push.valid.toBoolean & dut.io.push.ready.toBoolean) {
          beat_idx += 1
        }
        dut.clockDomain.waitRisingEdge()
      }


      dut.clockDomain.waitRisingEdge(10)


      dut.clockDomain.waitRisingEdge(500)

      simSuccess()
    }
  }
}

object KeyStreamCCSim {
  def main(args: Array[String]) : Unit = {

    SimConfig
    .withWave
    .addRtl(s"../corundum.rx.tx/fpga/lib/eth/lib/axis/rtl/axis_async_fifo.v")

    .compile {
      val dut = new KeyStreamCC(128, ClockDomain.current, ClockDomain.current)
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    .doSim { dut =>

      SimTimeout(10000)

      dut.io.push.valid #= false
      dut.io.pop.ready #= false

      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge(10)
      var beat_idx = 0
      var beat_num = 30

      while (beat_idx < beat_num) {
        dut.io.push.valid #= (Random.nextInt(16) >= 8)
        dut.io.pop.ready #= (Random.nextInt(16) >= 8)
        dut.io.push.payload #= BigInt(beat_idx)
        if (dut.io.push.valid.toBoolean & dut.io.push.ready.toBoolean) {
          beat_idx += 1
        }
        dut.clockDomain.waitRisingEdge()
      }


      dut.clockDomain.waitRisingEdge(10)


      dut.clockDomain.waitRisingEdge(500)

      simSuccess()
    }
  }
}
