


import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._


import scala.lms.targets.scalalike._

import scala.lms.targets.graphviz.GraphVizExport


object EffectTest extends App {


  class DSL(psize: Int) extends BooleanOpsExp with PurePrimitiveOpsExp with FunctionsExp with IfThenElsePureExp with ScalaCompile with ImplicitOpsExp with ArrayOpsExp {
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


    def testeff(i: Rep[Int]): Rep[Int] = {
      val a = NewArray[Int](unit(10))
      i
    }


    val iarg = exposeRepFromRep[Int]
    val iret = exposeRepFromRep[Int]


    def graph() = {
      val (code, cm) = emitGraph.emitDepGraphf(testeff)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check2.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

  }


  var Timings: Vector[(Int, Double)] = Vector.empty
  var time: Double = 0
  var size = 2

  val start = System.nanoTime()
  val dsl = new DSL(size)

  dsl.graph()


  val stop = System.nanoTime()
  time = (stop - start) / 1000000000.0;
  Timings = Timings :+(size, time)
  println("Result: " +(size, time))
}





