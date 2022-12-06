//package corundum

import spinal.core._
import spinal.lib._

//object CorundumAxi4SpecRenamer{
//  def apply[T <: Data](that : Fragment(Stream[T])): Fragment(Stream[T]) = {
//    that.payload match {
//      case axis: CorundumFrame => {
//        def doIt = {
//          axis.tdata.overrideLocalName("tdata")
//          (axis.tkeep != null)  generate axis.tkeep.overrideLocalName("tkeep")
//          (axis.tlast != null)  generate axis.tlast.overrideLocalName("tlast")
//          (axis.tuser != null)  generate axis.tuser.overrideLocalName("tuser")
//          that.flatten.foreach((bt) => {
//            bt.setName(bt.getName().replace("payload_", ""))
//            bt.setName(bt.getName().replace("valid", "tvalid"))
//            bt.setName(bt.getName().replace("ready", "tready"))
//            if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
//          })
//        }
//        if(Component.current == that.component)
//          that.component.addPrePopTask(() => {doIt})
//        else
//          doIt
//      }
//    }
//    that
//  }
//}

//object FrameSpecRenamer{
//  def apply[T <: Bundle with CorundumFrame](that : T): T ={
//    def doIt = {
//      that.flatten.foreach((bt) => {
//        println(bt.getName())
//        bt.setName(bt.getName().replace("_payload_",""))
//        bt.setName(bt.getName().replace("_valid","valid"))
//        bt.setName(bt.getName().replace("_ready","ready"))
//        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
//      })
//    }
//    if(Component.current == that.component)
//      that.component.addPrePopTask(() => {doIt})
//    else
//      doIt
//
//    that
//  }
//}
//
//// https://gitter.im/SpinalHDL/SpinalHDL?at=5c2297c28d31aa78b1f8c969
//object XilinxPatch {
//  def apply[T <: Component](c : T) : T = {
//    //Get the io bundle via java reflection
//    val m = c.getClass.getMethod("io")
//    val io = m.invoke(c).asInstanceOf[Bundle]
//    println("getClass %s", m);
//
//    //Patch things
//    io.elements.map(_._2).foreach {
//      //case axi : AxiLite4 => AxiLite4SpecRenamer(axi)
//      //case axi : Axi4 => Axi4SpecRenamer(axi)
//      case axi : CorundumFrame => FrameSpecRenamer(axi)
//      case _ => println("unknown")
//    }
//
//    //Builder pattern return the input argument
//    c 
//  }
//}
