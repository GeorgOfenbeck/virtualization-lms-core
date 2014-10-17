/**
 * Georg Ofenbeck
 First created:
 * Date: 03/09/2014
 * Time: 16:47 
 */


import org.scalatest.FunSpec

import scala.virtualization.lms.common._

class ToyLMS extends FunSpec {


  describe("checking it out") {


    trait DSL extends PrimitiveOpsExp {


      def simpleplus(a: Rep[Double], b: Rep[Double]): Rep[Double] = {

        infix_+(a, b)
      }

    }

    class mydsl extends DSL
    val foo = new mydsl


    val a = foo.Const(10.0)
    val b = foo.Const(10.0)

    foo.simpleplus(a, b) //stage here


  }
}
