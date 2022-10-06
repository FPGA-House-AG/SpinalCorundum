package corundum

//import scala.util.Random

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import spinal.lib.bus.amba4.axi._

// companion object
object CorundumFrameWriterDut {
}

case class CorundumFrameWriterDut(dataWidth : Int) extends Component {
  val io = new Bundle {
    val output = master(Stream(Fragment(CorundumFrame(dataWidth))))
    val slave0 = slave(Axi4(Axi4Config(32, 32, 2, useQos = false, useRegion = false)))
  }

  val ctrl = new Axi4SlaveFactory(io.slave0)
  val writer = CorundumFrameWriter(dataWidth)
  val bridge = writer.driveFrom(ctrl, 0)
  io.output << writer.io.output
}

//Generate the CorundumFrameWriter's Verilog
object CorundumFrameWriterDutVerilog {
  def main(args: Array[String]) {
   val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new CorundumFrameWriterDut(512)
      XilinxPatch(toplevel)
    })
  }
}

object CorundumFrameWriterSim {

  def main(args: Array[String]) {
    val dataWidth = 64
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth / 8

    printf("keepWidth=%d\n", keepWidth)

    var compiled = SimConfig
      .withFstWave
      .compile(new CorundumFrameWriterDut(dataWidth))

    compiled.doSim { dut =>

      dut.io.slave0.w.last #= true
      dut.io.slave0.r.ready #= true
      dut.io.slave0.b.ready #= true
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      dut.io.output.ready #= true

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.waitSampling()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()

      dut.io.slave0.aw.payload.id.assignBigInt(0)
      dut.io.slave0.aw.payload.lock.assignBigInt(0) // normal
      dut.io.slave0.aw.payload.prot.assignBigInt(2) // normal non-secure data access
      dut.io.slave0.aw.payload.burst.assignBigInt(1) // fixed address burst
      dut.io.slave0.aw.payload.len.assignBigInt(0) // 1 beat per burst
      dut.io.slave0.aw.payload.size.assignBigInt(2) // 4 bytes per beat

      dut.io.slave0.ar.payload.id.assignBigInt(0)
      dut.io.slave0.ar.payload.lock.assignBigInt(0) // normal
      dut.io.slave0.ar.payload.prot.assignBigInt(2) // normal non-secure data access
      dut.io.slave0.ar.payload.burst.assignBigInt(1) // fixed address burst
      dut.io.slave0.ar.payload.len.assignBigInt(0) // 1 beat per burst
      dut.io.slave0.ar.payload.size.assignBigInt(2) // 4 bytes per beat

      dut.io.slave0.w.payload.strb.assignBigInt(0xF) // 4 bytes active per beat

      dut.io.slave0.ar.valid #= true
      dut.io.slave0.ar.payload.addr.assignBigInt(0x08) // read GIT build number 
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.ar.ready.toBoolean)
      dut.io.slave0.ar.valid #= false
  
      //Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      
      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x00112233)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false
      //dut.clockDomain.waitRisingEdge()


      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x104) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x44556677)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false
      //dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()



      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x0899AABB)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      //dut.clockDomain.waitRisingEdge()


      dut.io.slave0.aw.valid #= true
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(3)
      dut.io.slave0.aw.payload.addr.assignBigInt(0x080) // assert TLAST
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false
      //dut.clockDomain.waitRisingEdge()


      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x104) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x0CDDEEFF)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false


      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x11111111)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x104) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x22222222)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x33333333)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x104) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x44444444)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x0899AABB)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      // dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()
      //dut.clockDomain.waitRisingEdge()

      dut.io.slave0.aw.valid #= true
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0)
      dut.io.slave0.aw.payload.addr.assignBigInt(0x080) // assert TLAST
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      dut.io.slave0.aw.valid #= true
      dut.io.slave0.aw.payload.addr.assignBigInt(0x100) // driveFrom() stream 
      dut.io.slave0.w.valid #= true
      dut.io.slave0.w.payload.data.assignBigInt(0x11112222)
      dut.clockDomain.waitSamplingWhere(dut.io.slave0.aw.ready.toBoolean && dut.io.slave0.w.ready.toBoolean)
      dut.io.slave0.aw.valid #= false
      dut.io.slave0.w.valid #= false

      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
      dut.clockDomain.waitRisingEdge()
    }
  }
}
