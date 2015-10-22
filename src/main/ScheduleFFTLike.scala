

package examples


import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._


import scala.lms.targets.scalalike._

import scala.lms.targets.graphviz.GraphVizExport


object ScheduleFFTLike extends App {


  class DSL(psize: Int) extends BooleanOpsExp with PurePrimitiveOpsExp with FunctionsExp with IfThenElsePureExp with ScalaCompile with ImplicitOpsExp {
    self =>
    override val codegen = new ScalaCodegen
      with EmitHeadInternalFunctionAsClass
      with ScalaGenBooleanOps
      with ScalaGenPrimitivOps
      with ScalaGenIfThenElse {
      val IR: self.type = self
    }
    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }

    def createsf(deepth: Int): StagedFunction[Rep[Int], Rep[Int]] = {

      if (deepth == 0) {
        val f: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {
          val t = unit(1)
          i + t

        }
        val sf = doLambda(f, false)
        sf
      } else {
        val f = createsf(deepth - 1)
        val g = createsf(deepth - 1)
        val h: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {

          val nums = for (j <- 0 until 100)
            yield (i + unit(j))

          val t0 = nums.reduce((a, b) => {
            val t = a + b
            t
          })

          val t1 = t0
          val t2 = t0 * unit(-1)

          val t3 = t1 + t2
          val c = i - t3
          f.apply(c) + g.apply(c)
        }
        val sf = doLambda(h, false)
        sf
      }
    }

    def createlin(deepth: Int): StagedFunction[Rep[Int], Rep[Int]] = {
      val f: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {
        val res = (0 until deepth).foldLeft(i)({
          (acc, ele) => {
            //infix_+(acc,ele)
            acc + ele
          }
        })
        res
      }
      val sf = doLambda(f, false)
      sf
    }

    def testlin(i: Rep[Int]): Rep[Int] = {
      val f = createlin(psize)
      f(i)
    }

    val mystagedfft: Rep[Int] => Rep[Int] = (i: Rep[Int]) => createsf(psize).apply(i)
    val iarg = exposeRepFromRep[Int]
    val iret = exposeRepFromRep[Int]

  }


  var Timings: Vector[(Int, Double)] = Vector.empty
  var time: Double = 0
  var size = 2
  do {
    val start = System.nanoTime()
    val dsl = new DSL(size)


    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\FFT" + size + "_improved.scala"))
    val esc = dsl.codegen.emitSource(dsl.mystagedfft, "testClass", stream2)(dsl.iarg, dsl.iret)
    stream2.flush()
    stream2.close()

    val stop = System.nanoTime()
    time = (stop - start) / 1000000000.0;
    Timings = Timings :+(size, time)
    println("Result: " +(size, time))
    size = size +1
  }

  while (time < 600)

  Timings.map(t => println(t._1 + "  " + t._2))
}





