package scala.virtualization.lms
package common


import shapeless.HList

import scala.virtualization.lms.internal._

trait ReificationPure{
  val IR: BaseExp with PureFunctionsExp
  val sym2tp: Map[IR.Sym[_], IR.TP[_]]
  val def2tp: Map[IR.Def[_], IR.TP[_]]
  val rootfunction: IR.Exp[_ => _]
}


trait ReifyPure{
  self =>
  val IR: BaseExp with PureFunctionsExp
  import IR._

  def reifyProgram[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): ReificationPure = {
    IR.reset()
    val lambda = fun(f)
    reifyProgram(lambda)
  }

  def reifyProgram(lambda: Exp[_ => _]): ReificationPure = {
    val immutable_out = new ReificationPure {
      val IR: self.IR.type = self.IR
      val sym2tp: Map[IR.Sym[_], IR.TP[_]] = self.IR.sym2tp
      val def2tp: Map[IR.Def[_], IR.TP[_]] = self.IR.def2tp
      val rootfunction = lambda
    }
    immutable_out
  }
}

