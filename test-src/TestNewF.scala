import org.scalatest.FunSpec
import scala.virtualization.lms._
import common._

class TestNewF extends FunSpec{


  describe("trying the new style functions") {


    class DSL extends NumericOpsExp with PureFunctionsExp with ReifyPure{
      self =>
      val IR: self.type = self
      def myfunction(in: Rep[Double]): Rep[Double] = {
        in + in
      }
    }

    val dsl = new DSL
    import dsl._


    val myf: Rep[Double] => Rep[Double] = dsl.myfunction
    val reified = dsl.reifyProgram(myf)

    //dsl.doLambda(dsl.myfunction)// (exposeRepFromRep(),exposeRepFromRep())

    //val reified = dsl.rei

    println("aha - no idea why this works")




  }

}
