package corundum

import spinal.core._
import spinal.lib.eda.bench.{Rtl, Report, Target, Bench}
import spinal.lib.eda.xilinx.VivadoFlow

import java.util.concurrent.ForkJoinPool
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

// inspired by SpinalHDL/**/Bench.scala (Demo) main
object CorundumFrameStashBench {
  def main(args: Array[String]) {
    val dataWidth = 128
    //val depth = 8

    import spinal.lib._
    val rtls = List(128/*, 1024*/).map(depth => Rtl(SpinalVerilog(new Component {
      setDefinitionName(s"CorundumFrameStash${depth}x${dataWidth}")
      val push = slave Stream(Fragment(CorundumFrame(dataWidth, 1)))
      val pop = master Stream(Fragment(CorundumFrame(dataWidth, 1)))

      val f = new CorundumFrameStash(dataWidth, fifoSize = depth, userWidth = 1)
      //f.logic.ram.addAttribute("ram_style", "block")
      f.io.sink << push.halfPipe()
      f.io.source.halfPipe() >> pop

      //xorOutputs(f)
    })))

    val targets = ArrayBuffer[Target]()
    targets += new Target {
      override def getFamilyName(): String = "Virtex UltraScale+"
      override def synthesise(rtl: Rtl, workspace: String): Report = {
        VivadoFlow(
          frequencyTarget = 825 MHz,
          vivadoPath="/opt/Xilinx/Vivado/2021.2/bin",
          workspacePath=workspace + "_fmax",
          rtl=rtl,
          family=getFamilyName(),
          device="xcu50-fsvh2104-2-e" // Alveo U50 FPGA
        )
      }
    }
    Bench(rtls, targets, workspacesRoot = "bench")
  }
}