package scala.lms
package targets
package scalalike

import ops.IfThenElseExp
import scala.lms.internal.FunctionsExp

trait ScalaGenFunctions extends ScalaCodegen{
 val IR: IfThenElseExp with FunctionsExp
 import IR._

 override def emitNode(tp: TP[_], acc: String,
                       block_callback: (Block,String) => String): String = tp.rhs match {

  case _ => super.emitNode(tp,acc,block_callback)
 }
}
