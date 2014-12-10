package scala.virtualization.lms
package common


import shapeless.HList

import scala.virtualization.lms.internal._

trait ReificationPure{
  val IR: BaseExp with PureFunctionsExp
  val sym2tp: Map[IR.Exp[_], IR.TP[_]]
  val def2tp: Map[IR.Def[_], IR.TP[_]]
  val id2tp: Map[Int, IR.TP[_]]
  //val rootblock: IR.Block
  val rootlambda: IR.Lambda[_,_]
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
    val tp = exp2tp(lambda)
    val lam: Lambda[_,_] = tp.rhs match{
      case x@Lambda(_,_,block,_,_) => x
      case _ => {
        assert(false, "This should not be possible")
        ???
      }
    }

    val immutable_out = new ReificationPure {
      val IR: self.IR.type = self.IR
      val sym2tp: Map[IR.Exp[_], IR.TP[_]] = self.IR.exp2tp
      val def2tp: Map[IR.Def[_], IR.TP[_]] = self.IR.def2tp
      val id2tp: Map[Int,IR.TP[_]] = self.IR.id2tp

      //val rootblock = lam.y
      val rootlambda = lam
    }
    immutable_out
  }
}

