import java.io.PrintWriter

import org.scalacheck._
import scala.virtualization.lms
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._


class DSL extends NumericOpsExp with LiftAll with PureFunctionsExp with Reify { //with ReifyProgram {
  self =>
    val IR: self.type = self
  def power(b: Rep[Double], x: Int): Rep[Double] =
    if (x == 0) 1.0 else b * power(b, x - 1)
  val f: (Rep[Double] => Rep[Double]) = (in: Rep[Double]) => power(in,2)
}

class TestReifyProgram extends Properties("Program Reification Tests") {
}


import org.scalatest.FunSpec

class TestReify extends FunSpec{
  describe("check reify") {
    val dsl = new DSL
    val reified = dsl.reifyProgram(dsl.f)
    val cm = CodeMotion(reified)
    /*val codegen = new ScalaCodegen {
      override val cminfo: CodeMotion = cm
    }*/
    //codegen.emitSource("FFT4", new PrintWriter(System.out))

    println("bla")
  }
}
