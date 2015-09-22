package scala.lms
package targets
package scalalike

import ops.PurePrimitiveOpsExp

trait ScalaGenPrimitivOps extends ScalaCodegen{
 val IR: PurePrimitiveOpsExp with internal.FunctionsExp
 import IR._

 override def emitNode(tp: TP[_], acc: String,
                       block_callback: (Block,String) => String): String = {
  val ma = tp.rhs match {
   case IntPlus(lhs,rhs) => emitValDef(tp, src"$lhs + $rhs")
   case _ => super.emitNode(tp,acc,block_callback)
  }
  ma
 }
}
