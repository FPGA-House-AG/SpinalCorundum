package corundum


import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object CorundumFrameInsertHeaderSim {
  def main(args: Array[String]) {
    val dataWidth = 128
    val keepWidth = dataWidth/8
    val headerWidthBytes = 14
    val headerWidth = headerWidthBytes * 8
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1

    SimConfig.withFstWave.doSim(new CorundumFrameInsertHeader(dataWidth, headerWidthBytes)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      var data0 = BigInt(0)

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
          // fill data with bytes
          // high nibble is packet index
          // low nibble is byte index (within packet)

          data0 = 0
          for (i <- 0 until tkeep_len) {
            data0 = (data0 << 8) | ((idx % 16) * 0x10) | ((tkeep_len - i) % 16)
          }
        }

        dut.io.sink.valid #= valid0
        dut.io.sink.payload.tdata #= data0
        dut.io.sink.last #= last0
        dut.io.sink.payload.tkeep #= tkeep0

        dut.io.header #= 0

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

object CorundumFrameInsertHeaderFormal extends App {
  import spinal.core.formal._

  FormalConfig.withCover(15).withBMC(15).withProve(15).doVerify(new Component {
    val dut = FormalDut(CorundumFrameInsertHeader(24, 1))
    assumeInitial(ClockDomain.current.isResetActive)

    // randomize DUT inputs
    anyseq(dut.io.source.ready)

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

  })
}
