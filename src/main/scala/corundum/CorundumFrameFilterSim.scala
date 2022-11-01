package corundum


import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._
import scala.util.Random

object CorundumFrameFilterSim {
  def main(args: Array[String]) {
    val dataWidth = 24
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8

    printf("keepWidth=%d\n", keepWidth)

    SimConfig.withFstWave.doSim(new CorundumFrameFilter(dataWidth)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var modelState = 0

      //StreamFragmentGenerator(event: x, packetData: y, dataType: CorundumFrame)

      var data0 = 0

      var first0 = true
      var last0 = false
      var valid0 = false
      var tkeep0 = 0

      dut.io.keepMask #= 0x000000
      dut.io.keepFilter #= 0x800000

      dut.io.dropMask #= 0x000000
      dut.io.dropFilter #= 0x000100


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


object CorundumFrameFilterAxi4Sim {

  def main(args: Array[String]) {
    val dataWidth = 64
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth / 8

    printf("keepWidth=%d\n", keepWidth)

    var compiled = SimConfig
      .withFstWave
      .compile(new CorundumFrameFilterAxi4(dataWidth, Axi4Config(32, 32, 2, useQos = false, useRegion = false)))

    compiled.doSim { dut =>

      // 4 bits per printf hex nibble
      val dw = 32 / 4
      // one keep bit per byte, 4 bits per printf hex nibble
      val kw = 32 / 8 / 4

      // all our writes are single-beats
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

      dut.io.ctrlbus.w.payload.strb.assignBigInt(0xF) // 4 bytes active per beat

      dut.io.ctrlbus.ar.payload.id.assignBigInt(0)
      dut.io.ctrlbus.ar.payload.lock.assignBigInt(0) // normal
      dut.io.ctrlbus.ar.payload.prot.assignBigInt(2) // normal non-secure data access
      dut.io.ctrlbus.ar.payload.burst.assignBigInt(1) // fixed address burst
      dut.io.ctrlbus.ar.payload.len.assignBigInt(0) // 1 beat per burst
      dut.io.ctrlbus.ar.payload.size.assignBigInt(2) // 4 bytes per beat

      dut.io.sink.valid #= false
      dut.io.source.ready #= false


      //Create a new thread
      val myNewThread = fork {
        val dw = dataWidth / 4
        var in_packet_continuation = false
        var first_beat = false

        

        while (true) {
          dut.clockDomain.waitSamplingWhere(dut.io.source.valid.toBoolean & dut.io.source.ready.toBoolean)
          first_beat = !in_packet_continuation
          printf(s"DATA == 0x%0${dw}X %s%s\n", dut.io.source.payload.fragment.tdata.toBigInt,
            if (first_beat) "*" else s" ",
            if (dut.io.source.payload.last.toBoolean) "L" else s" ")
          in_packet_continuation = !dut.io.source.payload.last.toBoolean
        }
      }


      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge()
      dut.io.source.ready #= true

      // keep match
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x40) // driveFrom() stream 
      dut.io.ctrlbus.w.valid #= true
      // If it is bigger than a long, you can use "FFFFFF33".asHex
      dut.io.ctrlbus.w.payload.data.assignBigInt(0x00112277L)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      // keep match
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x44) // driveFrom() stream 
      dut.io.ctrlbus.w.valid #= true
      // If it is bigger than a long, you can use "FFFFFF33".asHex
      dut.io.ctrlbus.w.payload.data.assignBigInt(0x77665544L)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      // keep mask
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x80) // driveFrom() stream 
      dut.io.ctrlbus.w.valid #= true
      dut.io.ctrlbus.w.payload.data.assignBigInt(0x000000FFL)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      // drop mask
      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x100)
      dut.io.ctrlbus.w.valid #= true
      dut.io.ctrlbus.w.payload.data.assignBigInt(0xFFFFFFFFL)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false

      dut.io.ctrlbus.aw.valid #= true
      dut.io.ctrlbus.aw.payload.addr.assignBigInt(0x104)
      dut.io.ctrlbus.w.valid #= true
      dut.io.ctrlbus.w.payload.data.assignBigInt(0xFFFFFFFFL)
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.aw.ready.toBoolean && dut.io.ctrlbus.w.ready.toBoolean)
      dut.io.ctrlbus.aw.valid #= false
      dut.io.ctrlbus.w.valid #= false


      // push one word into FIFO
      dut.clockDomain.waitSamplingWhere(dut.io.sink.ready.toBoolean)
      dut.io.sink.payload.tdata.assignBigInt(0x0011223344556677L)
      dut.io.sink.payload.tkeep.assignBigInt((1 << keepWidth) - 1)
      dut.io.sink.payload.tuser.assignBigInt(0)
      dut.io.sink.valid #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.sink.valid #= false

      dut.io.ctrlbus.r.ready #= false
      // assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= true
      dut.io.ctrlbus.ar.payload.addr.assignBigInt(0x08) // read GIT build number 
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.ar.ready.toBoolean)
      // de-assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= false
      // accept read data on R channel
      dut.io.ctrlbus.r.ready #= true
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.r.valid.toBoolean)
      // no longer accept read data on R channel
      dut.io.ctrlbus.r.ready #= false
      printf(s"*TDATA == 0x%0${dw}X\n", dut.io.ctrlbus.r.payload.data.toBigInt)

      //Wait for a rising edge on the clock
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      
      dut.io.ctrlbus.r.ready #= false
      // assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= true
      dut.io.ctrlbus.ar.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      // wait for active beat on AR channel to take our read command
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.ar.ready.toBoolean)
      // de-assert read command on AR channel
      dut.io.ctrlbus.ar.valid #= false
      // accept read data on R channel
      dut.io.ctrlbus.r.ready #= true
      dut.clockDomain.waitSamplingWhere(dut.io.ctrlbus.r.valid.toBoolean)
      // no longer accept read data on R channel
      dut.io.ctrlbus.r.ready #= false

      printf(s"*TDATA == 0x%0${dw}X\n", dut.io.ctrlbus.r.payload.data.toBigInt)
      //printf(s"*TKEEP == 0x%0${kw}X\n", dut.io.master0.payload.tkeep.toBigInt)

      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
    }
  }
}
