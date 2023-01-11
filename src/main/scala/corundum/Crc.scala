package corundum

import spinal.core._
import spinal.lib._

import scala.math.pow

import java.util.Base64

// from tester/src/main/scala/spinal/tester/code/Play2.scala

// companion object for case class
object Crc {
  // generate VHDL and Verilog
  def main(args: Array[String]) {
    val verilogReport = Config.spinal.generateVerilog({
      val toplevel = new Crc(256)
      toplevel
    })
    val vhdlReport = Config.spinal.generateVhdl({
      val toplevel = new Crc(256)
      toplevel
    })
  }
}

// true dual port ram with independent clocks, symmetric data widths
case class Crc(dataWidth: Int) extends Component {

  // i is the number of zero bytes after byte value j
  def partial_crc(i: Int, j: Int) : BigInt = {
    require (j >= 0 && j <= 255);
    val b = j;
    var crc : Int = 0;
    // move byte into most significant byte of 32-bit
    crc ^= b << 24; 
    for (k <- 0 until 8) {
      // CRC most significant bit == 1?
      if ((crc & 0x80000000) != 0) {
        crc = (crc << 1) ^ polynomial;
      } else {
        crc = (crc << 1)
      }
    }
    // evolve it now, iterate over trailing zero bytes
    for (k <- 0 until i) {
      val b = 0;
      // move byte into most significant byte of 32-bit
      crc ^= b << 24;
      // iterate over all bits in byte, starting with msb
      for (k <- 0 until 8) {
        if ((crc & 0x80000000) != 0) {
          crc = (crc << 1) ^ polynomial;
        } else {
          crc = (crc << 1)
        }
      }
    }
    // interpret the 32-bit signed Int as an 32-bit unsigned BigInt value
    // logically shift in a most significant zero bit, shift back up, add lsb back
    // (https://stackoverflow.com/questions/21212993/unsigned-variables-in-scala)
    var result_crc = (BigInt(crc >>> 1) << 1) + (crc & 1)
    //printf("%d, %d = 0x%08x (%s BigInt)\n", i, j, result_crc.toLong, result_crc.toString(16))
    result_crc
  }

  val io = new Bundle {
    val i = in UInt(8 bits)
    val j = in UInt(8 bits)
    val o = out UInt(32 bits)
  }

  final val polynomial = 0x04C11DB7;
  //var luts = List.fill(dataWidth / 8)(List.fill(256)(Reg(UInt(32 bits))))
  //var luts = Vec.fill(dataWidth / 8)(Vec.fill(256)(Reg(UInt(32 bit))))
  //var luts = Array.ofDim[UInt(32 bits)](dataWidth / 8, 256)
  //val x = Array.tabulate(dataWidth / 8, 256)(i, j) => {

  //val luts = Array.fromFunction(partial_crc(_,_))(dataWidth / 8, 256)

  //var myMatrix = Array.ofDim[UInt(32 bits)](3,3)

  //var luts = List.tabulate(3, x => List.tabulate(3, y => (x, y)).toArray).toArray

  var luts = Array.range(0, dataWidth / 8).map(i => Array.range(0, 256).map(j => partial_crc(i, j)))
  // luts.foreach { row => row foreach print; println }
  for(i <- 0 until dataWidth / 8; j <- 0 until 256)
  {
    if (((i + j) % 13 == 0))
    printf("@(i, j) = @(%2d, %3d) = 0x8%s\n", i, j, luts(i)(j).toString(16))
  }

  io.o := 0 //io.i + io.j

  var memluts = List.fill(dataWidth / 8)(Mem(UInt(32 bits), 256))
  for (i <- 0 until dataWidth / 8) {
    memluts(i).initBigInt(luts(i))
  }

  // the following code does nothing, it is now in partial_crc()
  // @TODO to be removed later
  for (i <- 0 until dataWidth / 8; j <- 0 until 256) {
    val b = j;
    var crc = 0;
    // move byte into most significant byte of 32-bit
    crc ^= b << 24; 
    for (k <- 0 until 8) {
      // CRC most significant bit == 1?
      if ((crc & 0x80000000) != 0) {
        crc = (crc << 1) ^ polynomial;
      } else {
        crc <<= 1;
      }
    }
    // evolve it now, iterate over trailing zero bytes
    for (k <- 0 until i) {
      val b = 0;
      // move byte into most significant byte of 32-bit
      crc ^= b << 24;
      // iterate over all bits in byte, starting with msb
      for (k <- 0 until 8) {
        if ((crc & 0x80000000) != 0) {
            crc = (crc << 1) ^ polynomial;
        } else {
            crc <<= 1;
        }
      }
    }
  }
}

import spinal.sim._
import spinal.core.sim._
import spinal.core.sim.{SimPublic, TracingOff}
import scala.util.Random

object CrcSim {
  def main(args: Array[String]) {
    SimConfig.withFstWave.doSim(new Crc(256)){dut =>
      // Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)
    }
  }
}

object CrcFormal extends App {
  import spinal.core.formal._

  FormalConfig.withCover(15).withBMC(15).withProve(15).doVerify(new Component {
    val dut = FormalDut(Crc(32))
    assumeInitial(ClockDomain.current.isResetActive)
  })
}

object QuickyList {
  def main(args: Array[String]) {
    val x = Array.tabulate(3, 5)((x, y) => {
      (x + y)
    })
    x.foreach { row => row foreach print; println }
  }
}