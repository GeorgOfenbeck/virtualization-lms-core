package examples


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._



import org.scalatest._


class TestCompile extends Suite {
  def testdsl(): Unit =  {

    class DSL extends BooleanOpsExp with InternalFunctionsExp{
      self =>
      val emitString = new Emit[Vector[String]] {
        val IR: self.type = self
      }
      def mystagedf(x: Rep[Boolean]): Rep[Boolean] ={
        x && x
      }
      val iarg = exposeRepFromRep[Boolean]
      val iret = exposeRepFromRep[Boolean]
    }
    val dsl = new DSL
    dsl.emitString.emit(Vector.empty[String],dsl.mystagedf)(dsl.iarg,dsl.iret)



  }
}