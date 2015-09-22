package scala.lms
package targets
package scalalike

import ops._
import scala.lms.internal.FunctionsExp

trait ScalaGenIfThenElse extends ScalaCodegen with EmitHeadInternalFunctionAsClass{
  val IR: IfThenElsePureExp with FunctionsExp
  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block,String) => String): String = tp.rhs match {
    case myIfThenElse(cond,thenp,elsep,resexpose) => {
      val thenlambda = exp2tp(thenp)
      val elselambda = exp2tp(elsep)

      val rets: String = (thenlambda.rhs,elselambda.rhs) match {
        case (InternalLambda(tf,tx,ty,targs,treturns),InternalLambda(ef,ex,ey,eargs,ereturns)) => {
          val l1 = "val " + quote(tp) + " = if (" + quote(cond) + ") {\n"
          val l2 = block_callback(ty,l1)
          val trestuple: Vector[String] = ty.res.map(r => quote(r))
          val l3: String = l2 + tupledeclarehelper(trestuple,"")
          val l4 = l3 + "\n} else {\n"
          val l5 = block_callback(ey,l4)
          val erestuple: Vector[String] = ey.res.map(r => quote(r))
          val l6: String = l5 + tupledeclarehelper(erestuple,"")
          l6 + "\n}\n"
        }
        case _ => {
          assert(false, "got an if statment which does not contain lambdas for its branches")
          ""
        }
      }
      rets

    }
    /*case IfThenElse(c,a,b) =>
      val l1 = "val " + quote(tp) + " = if (" + quote(c) + ") {"
      val l2 = block_callback(a,l1)
      val l3: String = l2 + quote(getBlockResults(a).head) //RF - fix for multi result
      val l4 = l3 + "} else {"
      val l5 = block_callback(b,l4)
      val l6: String = l5 + quote(getBlockResults(b).head) //RF - fix for multi result
      l6 + "}"*/
    case _ => super.emitNode(tp,acc,block_callback)
  }
}
