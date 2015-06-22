package scala.lms
package targets
package scalalike

import ops.IfThenElseExp
import scala.lms.internal.InternalFunctionsExp

trait ScalaGenIfThenElse extends ScalaCodegen{
  val IR: IfThenElseExp with InternalFunctionsExp
  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block,String) => String): String = tp.rhs match {
    case IfThenElse(c,a,b) =>
      val l1 = "val " + quote(tp) + " = if (" + quote(c) + ") {"
      val l2 = block_callback(a,l1)
      val l3: String = l2 + quote(getBlockResults(a).head) //RF - fix for multi result
      val l4 = l3 + "} else {"
      val l5 = block_callback(b,l4)
      val l6: String = l5 + quote(getBlockResults(b).head) //RF - fix for multi result
      l6 + "}"
    case _ => super.emitNode(tp,acc,block_callback)
  }
}
