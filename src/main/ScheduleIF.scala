




import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._


import scala.lms.targets.scalalike._

import scala.lms.targets.graphviz.{MyRange, ScalaGenMyRange, GraphVizExport}


object ScheduleIF extends App {


  class DSL(psize: Int) extends BooleanOpsExp with PurePrimitiveOpsExp with FunctionsExp with IfThenElsePureExp with ScalaCompile with ImplicitOpsExp with MyRange with OrderingOpsExp{
    self =>
    override val codegen = new ScalaCodegen
      with EmitHeadInternalFunctionAsClass
      with ScalaGenBooleanOps
      with ScalaGenPrimitivOps
      with ScalaGenIfThenElse
      with ScalaGenMyRange
      with ScalaGenOrderingOps
    {
      val IR: self.type = self
    }
    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }

    def createif(deepth: Int,num: Int): Rep[Int] => Rep[Int] = {

      if (deepth == 0) {
        val f: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {
          i + unit(num)
        }
        f
      } else {
        val f = createif(deepth - 1, num+1)
        val g = createif(deepth - 1, num+2)
        val h: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {
          val rbool = ordering_lteq(i, unit(0))
          val r = myifThenElse(rbool, {
            f(i)
          }, {
            g(i)
          })
          r
        }
        h
      }
    }



    def testif(i: Rep[Int]): Rep[Int]  = {
      val f = createif(psize, 0)
      f(i)
    }

    //val mystagedf: Rep[Int] => Rep[Int] = (i: Rep[Int]) => createsf(9).apply(i)
    val iarg = exposeRepFromRep[Int]
    val iret = exposeRepFromRep[Int]

  }


  var Timings: Vector[(Int, Double)] = Vector.empty
  var time: Double = 0
  var size = 2
  do {
    val start = System.nanoTime()
    val dsl = new DSL(size)


    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\IF" + size + "_improved.scala"))
    val esc = dsl.codegen.emitSource(dsl.testif, "testClass", stream2)(dsl.iarg, dsl.iret)
    stream2.flush()
    stream2.close()

    val stop = System.nanoTime()
    time = (stop - start) / 1000000000.0;
    Timings = Timings :+(size, time)
    println("Result: " +(size, time))
    size = size +1
  }

  while (time < 60)

  Timings.map(t => println(t._1 + "  " + t._2))
}





