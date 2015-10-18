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
   //case ObjDoubleParseDouble(s) => emitValDef(tp, src"java.lang.Double.parseDouble($s)")
   case ObjDoublePositiveInfinity() => emitValDef(tp, "scala.Double.PositiveInfinity")
   case ObjDoubleNegativeInfinity() => emitValDef(tp, "scala.Double.NegativeInfinity")
   case ObjDoubleMinValue() => emitValDef(tp, "scala.Double.MinValue")
   case ObjDoubleMaxValue() => emitValDef(tp, "scala.Double.MaxValue")
   //case DoubleFloatValue(lhs) => emitValDef(tp, quote(lhs) + ".floatValue()")
   case DoublePlus(lhs,rhs) => emitValDef(tp, quote(lhs) + " + " + quote(rhs))
   case DoubleMinus(lhs,rhs) => emitValDef(tp, quote(lhs) + " - " + quote(rhs))
   case DoubleTimes(lhs,rhs) => emitValDef(tp, quote(lhs) + " * " + quote(rhs))
   case DoubleDivide(lhs,rhs) => emitValDef(tp, quote(lhs) + " / " + quote(rhs))
   case DoubleToInt(lhs) => emitValDef(tp, quote(lhs) + ".toInt")
   case DoubleToFloat(lhs) => emitValDef(tp, quote(lhs) + ".toFloat")
   //case ObjFloatParseFloat(s) => emitValDef(tp, "java.lang.Float.parseFloat(" + quote(s) + ")")
   case FloatToInt(lhs) => emitValDef(tp, quote(lhs) + ".toInt")
   case FloatToDouble(lhs) => emitValDef(tp, quote(lhs) + ".toDouble")
   case FloatPlus(lhs,rhs) => emitValDef(tp, quote(lhs) + " + " + quote(rhs))
   case FloatMinus(lhs,rhs) => emitValDef(tp, quote(lhs) + " - " + quote(rhs))
   case FloatTimes(lhs,rhs) => emitValDef(tp, quote(lhs) + " * " + quote(rhs))
   case FloatDivide(lhs,rhs) => emitValDef(tp, quote(lhs) + " / " + quote(rhs))
   //case ObjIntegerParseInt(s) => emitValDef(tp, "java.lang.Integer.parseInt(" + quote(s) + ")")
   case ObjIntMaxValue() => emitValDef(tp, "scala.Int.MaxValue")
   case ObjIntMinValue() => emitValDef(tp, "scala.Int.MinValue")
   case IntPlus(lhs,rhs) => emitValDef(tp, quote(lhs) + " + " + quote(rhs))
   case IntMinus(lhs,rhs) => emitValDef(tp, quote(lhs) + " - " + quote(rhs))
   case IntTimes(lhs,rhs) => emitValDef(tp, quote(lhs) + " * " + quote(rhs))
   // case IntDivideFrac(lhs,rhs) => emitValDef(tp, quote(lhs) + " / " + quote(rhs))
   case IntDivide(lhs,rhs) => emitValDef(tp, quote(lhs) + " / " + quote(rhs))
   case IntMod(lhs,rhs) => emitValDef(tp, quote(lhs) + " % " + quote(rhs))
   case IntBinaryOr(lhs,rhs) => emitValDef(tp, quote(lhs) + " | " + quote(rhs))
   case IntBinaryAnd(lhs,rhs) => emitValDef(tp, quote(lhs) + " & " + quote(rhs))
   case IntBinaryXor(lhs,rhs) => emitValDef(tp, quote(lhs) + " ^ " + quote(rhs))
   case IntShiftLeft(lhs,rhs) => emitValDef(tp, quote(lhs) + " << " + quote(rhs))
   case IntShiftRightArith(lhs, rhs) => emitValDef(tp, quote(lhs) + " >> " + quote(rhs))
   case IntShiftRightLogical(lhs, rhs) => emitValDef(tp, quote(lhs) + " >>> " + quote(rhs))
   //case IntDoubleValue(lhs) => emitValDef(tp, quote(lhs) + ".doubleValue()")
   //case IntFloatValue(lhs) => emitValDef(tp, quote(lhs) + ".floatValue()")
   case IntBitwiseNot(lhs) => emitValDef(tp, "~" + quote(lhs))
   case IntToLong(lhs) => emitValDef(tp, quote(lhs) + ".toLong")
   case IntToFloat(lhs) => emitValDef(tp, quote(lhs) + ".toFloat")
   case IntToDouble(lhs) => emitValDef(tp, quote(lhs) + ".toDouble")
   //case ObjLongParseLong(s) => emitValDef(tp, "java.lang.Long.parseLong(" + quote(s) + ")")
   case LongBinaryOr(lhs,rhs) => emitValDef(tp, quote(lhs) + " | " + quote(rhs))
   case LongBinaryAnd(lhs,rhs) => emitValDef(tp, quote(lhs) + " & " + quote(rhs))
   case LongShiftLeft(lhs,rhs) => emitValDef(tp, quote(lhs) + " << " + quote(rhs))
   case LongShiftRightUnsigned(lhs,rhs) => emitValDef(tp, quote(lhs) + " >>> " + quote(rhs))
   case LongToInt(lhs) => emitValDef(tp, quote(lhs) + ".toInt")
   case _ => super.emitNode(tp,acc,block_callback)
  }
  ma
 }
}
