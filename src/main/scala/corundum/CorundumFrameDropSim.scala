package corundum


import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._
import scala.util.Random

class CorundumFrameDropThenStash(dataWidth : Int) extends Component {
  val io = new Bundle {
    val sink = slave Stream Fragment(CorundumFrame(dataWidth))
    val source = master Stream Fragment(CorundumFrame(dataWidth))
    val drop = in Bool()
  }
  val drop = CorundumFrameDrop(dataWidth)
  val stash = CorundumFrameStash(dataWidth, 2)
  drop.io.sink << io.sink

  val can_store_type123 = stash.io.availability >= 2

  drop.io.drop := !can_store_type123
  stash.io.sink << drop.io.source
  io.source << stash.io.source
}

object CorundumFrameDropSim {
  def main(args: Array[String]) {
    val dataWidth = 24
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8

    printf("keepWidth=%d\n", keepWidth)

    SimConfig.withFstWave.doSim(new CorundumFrameDropThenStash(dataWidth)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      var data0 = 0

      var first0 = true
      var last0 = false
      var valid0 = false
      var tkeep0 = 0

      for (idx <- 0 to 499){

        // active beat
        if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) data0 += 1
        // active beat, or slave was not active yet?
        if ((dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) || !valid0) {
          valid0 = (Random.nextInt(8) > 6) | (idx > 300)
          last0 = (Random.nextInt(8) >= 4) & valid0 
        }
        tkeep0 = 0
        if (valid0) {
          var tkeep_len = if (!last0) keepWidth else 1 + Random.nextInt(keepWidth-1);
          for (i <- 0 until tkeep_len) {
            tkeep0 = (tkeep0 << 1) | 1
          }
        }
        if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean & dut.io.sink.last.toBoolean) {
          data0 = Random.nextInt(maxDataValue)
        }
        data0 &= scala.math.pow(2, dataWidth).intValue - 1

        dut.io.sink.valid #= valid0
        dut.io.sink.payload.tdata #= data0
        dut.io.sink.last #= last0
        dut.io.sink.payload.tkeep #= tkeep0

        dut.io.drop #= Random.nextInt(8) >= 4

        dut.io.source.ready #= (Random.nextInt(8) > 1)

        //Wait a rising edge on the clock
        dut.clockDomain.waitRisingEdge()

        //Check that the dut values match with the reference model ones
        //val modelFlag = modelState == 0 || dut.io.cond1.toBoolean
        //assert(dut.io.state.toInt == modelState)
        //assert(dut.io.flag.toBoolean == modelFlag)

        //Update the reference model value
        //if(dut.io.cond0.toBoolean) {
        //  modelState = (modelState + 1) & 0xFF
        //}
      }
    }
  }
}

object CorundumFrameDropFormal extends App {
  import spinal.core.formal._

  FormalConfig.withCover(15).withBMC(15).withProve(15).doVerify(new Component {
    val dut = FormalDut(CorundumFrameDrop(8))
    assumeInitial(ClockDomain.current.isResetActive)

    // randomize DUT inputs
    anyseq(dut.io.source.ready)

    anyseq(dut.io.drop)
    anyseq(dut.io.sink.valid)
    anyseq(dut.io.sink.last)
    anyseq(dut.io.sink.payload.tkeep)
    anyseq(dut.io.sink.payload.tdata)
    anyseq(dut.io.sink.payload.tuser)

    // Assume AXI data remains stable when the stream is stalled
    // (VALID & !READY) -> STABLE(DATA)
    // (VALID & !READY) -> STABLE(VALID)
    // (.isStall)


    // A -> B, or A "implies" B, or if (A) then (B)
    // if "A" then assert("B") or assert(!A or B)

    assume(!dut.io.sink.isStall | stable(dut.io.sink.valid))
    assume(!dut.io.sink.isStall | stable(dut.io.sink.last))
    assume(!dut.io.sink.isStall | stable(dut.io.sink.payload.tkeep))
    assume(!dut.io.sink.isStall | stable(dut.io.sink.payload.tdata))
    assume(!dut.io.sink.isStall | stable(dut.io.sink.payload.tuser))



    // during a packet tail, drop must remain stable
    //assert((!dut.y.tail) || stable(dut.y_drop_packet))

    when (dut.y.tail) { assert(stable(dut.y_drop_packet)) }
  })
}
