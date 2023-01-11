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

// parallel CRC implementation, LUT based
case class Crc(dataWidth: Int) extends Component {

  // Calculation of the CRC over a multiple bytes can be split
  // CRC(X + Y) = CRC(X) + CRC(Y)

  // copied from SpinalSimMacTester
  // convert hexadecimal string to a sequence of Int with values in range [0-255] (bytes)
  def hexStringToFrame(str : String) = {
    val spaceLess = str.replace(" ","")
    Seq.tabulate[Int](spaceLess.size/2)(i => Integer.parseInt(spaceLess.substring(i*2, i*2+2), 16))
  }

  // copied from SpinalSimMacTester
  // pretty reference bit-by-bit CRC32 calculator
  // input 'that' is a sequence of Int with values in range [0-255] (bytes)
  def calcCrc32(that : Seq[Int]): Int ={
    def getBit(id : Int) = (that(id/8) >> ((id % 8))) & 1
    var crc = -1
    for(bitId <- 0 until that.size*8){
      val bit = getBit(bitId) ^ ((crc >> 31) & 1)
      crc = (crc << 1) ^ ((if(bit == 1) 0x04C11DB7 else 0))
    }
    val crcReversed = (0 until 32).map(i => ((crc >> i) & 1) << (31-i)).reduce(_ | _)
    ~crcReversed
  }

    def calcCrc32_using_lut(that : Seq[Int]): Int ={
    def getBit(id : Int) = (that(id/8) >> ((id % 8))) & 1
    var crc = -1
    for (byteId <- 0 until that.size){
      val byte_value = that(byteId)
      crc = crc ^ luts(byteId)(byte_value).toInt
    }
    val crcReversed = (0 until 32).map(i => ((crc >> i) & 1) << (31-i)).reduce(_ | _)
    ~crcReversed
  }

  def testit() = {
    val frameCorrectA = hexStringToFrame("33330000 0002000A CD2C1594 86DD600B DD410008 3AFFFE80 00000000 0000FC3B 9A3CE0E2 3955FF02 00000000 00000000 00000000 00028500 CC860000 00005901 A328")
    val frameCorrectB = hexStringToFrame("33330000 00FB000A CD2C1594 86DD600C 36DF0091 11FFFE80 00000000 0000FC3B 9A3CE0E2 3955FF02 00000000 00000000 00000000 00FB14E9 14E90091 C6390000 84000000 00020000 00000135 01350139 01330132 01650130 01650163 01330161 01390162 01330163 01660130 01300130 01300130 01300130 01300130 01300130 01300130 01380165 01660369 70360461 72706100 000C8001 00000078 000D0572 61777272 056C6F63 616C00C0 60001C80 01000000 780010FE 80000000 000000FC 3B9A3CE0 E239550D 5BA667")
    val crc = calcCrc32(frameCorrectA)
    if (crc != 0x2144DF1C) {
      printf("CRC mismatch\n")
    }
    // reference crc
    val frameSmall = hexStringToFrame("33330000")
    val crc_good = calcCrc32(frameSmall)

    val crc_lut = calcCrc32_using_lut(frameSmall)
    if (crc_lut != crc_good) {
      printf("CRC mismatch\n")
    }
    //printf("calculated CRC = 0x%s, expected = 0x2144DF1C\n", crc.toString(16))
  }

  // i is the number of zero bytes after byte value j
  // result is 32-bit CRC
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
    // evolve it now, iterate over i trailing zero bytes
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

  // The datawidth is split in smaller sized vectors (currently hardcoded a byte)
  // Then for each byte in each location (i), a LUT is generated. Each LUT entry contains
  // the partial CRC contribution for each possible byte value (0<=j<256) at byte position (i).

  // populate the LUTs with precalculated partial CRC contribution
  var luts = Array.range(0, dataWidth / 8).map(i => Array.range(0, 256).map(j => partial_crc(i, j)))

  var memluts = List.fill(dataWidth / 8)(Mem(UInt(32 bits), 256))
  for (i <- 0 until dataWidth / 8) {
    memluts(i).initBigInt(luts(i))
  }

  // luts.foreach { row => row foreach print; println }
  for(i <- 0 until dataWidth / 8; j <- 0 until 256)
  {
    if (((i + j) % 13 == 0))
    printf("@(i, j) = @(%2d, %3d) = 0x8%s\n", i, j, luts(i)(j).toString(16))
  }


  testit()

  io.o := 0 //io.i + io.j
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