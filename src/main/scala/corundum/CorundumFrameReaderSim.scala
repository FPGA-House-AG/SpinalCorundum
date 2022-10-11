package corundum

//import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._

object CorundumFrameReaderSim {

  def main(args: Array[String]) {
    val dataWidth = 64
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth / 8

    printf("keepWidth=%d\n", keepWidth)

    var compiled = SimConfig
      .withFstWave
      .compile(new CorundumFrameReaderAxi4(dataWidth, Axi4Config(32, 32, 2, useQos = false, useRegion = false)))

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

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitRisingEdge()

      // push one word into FIFO
      dut.clockDomain.waitSamplingWhere(dut.io.input.ready.toBoolean)
      dut.io.input.payload.tdata.assignBigInt(0x0011223344556677L)
      dut.io.input.payload.tkeep.assignBigInt(0xFF)
      dut.io.input.payload.tuser.assignBigInt(0)
      dut.io.input.valid #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.input.valid #= false

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
