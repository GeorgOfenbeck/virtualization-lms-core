package scala.lms
package targets
package scalalike

import ops._

trait ScalaGenOrderingOps extends ScalaCodegen {
  val IR: OrderingOpsExp with internal.FunctionsExp

  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block, String) => String): String = {
    val ma = tp.rhs match {
      case OrderingLT(a, b) => emitValDef(tp, src"$a < $b")
      case OrderingLTEQ(a, b) => emitValDef(tp, src"$a <= $b")
      case OrderingGT(a, b) => emitValDef(tp, src"$a > $b")
      case OrderingGTEQ(a, b) => emitValDef(tp, src"$a >= $b")
      case OrderingEquiv(a, b) => emitValDef(tp, src"$a equiv $b")
      case OrderingMax(a, b) => emitValDef(tp, src"$a max $b")
      case OrderingMin(a, b) => emitValDef(tp, src"$a min $b")
      case c@OrderingCompare(a, b) => c.mev match {
        case m if m == Manifest.Int => emitValDef(tp, "java.lang.Integer.compare(" + quote(a) + "," + quote(b) + ")")
        case m if m == Manifest.Long => emitValDef(tp, "java.lang.Long.compare(" + quote(a) + "," + quote(b) + ")")
        case m if m == Manifest.Double => emitValDef(tp, "java.lang.Double.compare(" + quote(a) + "," + quote(b) + ")")
        case m if m == Manifest.Float => emitValDef(tp, "java.lang.Float.compare(" + quote(a) + "," + quote(b) + ")")
        case m if m == Manifest.Boolean => emitValDef(tp, "java.lang.Boolean.compare(" + quote(a) + "," + quote(b) + ")")
        case m if m == Manifest.Byte => emitValDef(tp, "java.lang.Byte.compare(" + quote(a) + "," + quote(b) + ")")
        case m if m == Manifest.Char => emitValDef(tp, "java.lang.Character.compare(" + quote(a) + "," + quote(b) + ")")
        case m if m == Manifest.Short => emitValDef(tp, "java.lang.Short.compare(" + quote(a) + "," + quote(b) + ")")
        case _ => emitValDef(tp, quote(a) + " compare " + quote(b))
      }
      case _ => super.emitNode(tp, acc, block_callback)
    }
    ma
  }
}