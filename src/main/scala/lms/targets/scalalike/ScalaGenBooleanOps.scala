package scala.lms
package targets
package scalalike


import ops.BooleanOpsExp

trait ScalaGenBooleanOps extends ScalaCodegen{
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block,String) => String): String = {
    val ma = tp.rhs match {
      case BooleanNegate(b) => emitValDef(tp, src"!$b")
      case BooleanAnd(lhs,rhs) => emitValDef(tp, src"$lhs && $rhs")
      case BooleanOr(lhs,rhs) => emitValDef(tp, src"$lhs || $rhs")
      case _ => super.emitNode(tp,acc,block_callback)
    }
    ma
  }
}
