package corundum

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
