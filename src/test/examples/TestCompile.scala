package examples


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._



import org.scalatest._

import scala.lms.targets.scalalike.{EmitHeadInternalFunctionasClass, ScalaCodegen}


class TestCompile extends Suite {
  def testdsl(): Unit =  {

    class DSL extends BooleanOpsExp with InternalFunctionsExp{
      self =>
      val emitString = new ScalaCodegen with EmitHeadInternalFunctionasClass {
        val IR: self.type = self
        val className = "testClass"
      }

      def mystagedf(x: Rep[Boolean]): Rep[Boolean] = x && x

      val iarg = exposeRepFromRep[Boolean]
      val iret = exposeRepFromRep[Boolean]
    }
    val dsl = new DSL
    val (code, esc) = dsl.emitString.emit("",dsl.mystagedf)(dsl.iarg,dsl.iret)
    println(code)
  }
}