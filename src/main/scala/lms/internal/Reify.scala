package scala.lms
package internal

import scala.lms.BaseExp



trait ReificationPure{
 val IR: BaseExp with InternalFunctionsExp
 val sym2tp: Map[IR.Exp[_], IR.TP[_]]
 val def2tp: Map[IR.Def[_], IR.TP[_]]
 val id2tp: Map[Int, IR.TP[_]]
 val rootlambda: IR.InternalLambda[_,_]

 object Const {
  def unapply[T](e: IR.Exp[T]): Option[IR.ConstDef[T]] = {
   val tp = sym2tp(e)
   tp.rhs match {
    case d@IR.ConstDef(x) => Some(d.asInstanceOf[IR.ConstDef[T]]) //TODO - get rid of type cast
    case _ => None
   }
  }
 }

 object Arg {
  def unapply[T](e: IR.Exp[T]): Option[IR.ArgDef[T]] = {
   val tp = sym2tp(e)
   tp.rhs match {
    case d@IR.ArgDef(id) => Some(d.asInstanceOf[IR.ArgDef[T]]) //TODO - get rid of type cast
    case _ => None
   }
  }
 }
}