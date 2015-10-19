import org.scala_lang.virtualized.SourceContext



import scala.lms.targets.scalalike._
import scala.lms.internal.FunctionsExp
import scala.lms.ops._
import scala.lms.targets.scalalike._
import scala.lms.targets.graphviz._

/**
 * Georg Ofenbeck
 First created:
 * Date: 18/10/2015
 * Time: 11:10 
 */




object Running_Example extends App {

  class DSL extends PurePrimitiveOpsExp with MyRange with IfThenElsePureExp with BooleanOpsExp with OrderingOpsExp with ScalaCompile {
    self =>
    override val codegen = new ScalaCodegen
      with EmitHeadInternalFunctionAsClass
      with ScalaGenBooleanOps
      with ScalaGenPrimitivOps
      with ScalaGenIfThenElse
      with ScalaGenMyRange
      with ScalaGenOrderingOps {
      val IR: self.type = self
    }
    val emitGraph = new InstGraphVizExport {
      override val IR: self.type = self
    }

    def tx(x: Rep[Int]) = {
      val t = range_map(unit(0), unit(10), (i: Rep[Int]) => {
        range_map(unit(0), unit(10), (j: Rep[Int]) => {
          val mod = int_mod(i, unit(2))
          val rbool = ordering_lteq(mod, unit(0))
          val r = myifThenElse(rbool, {
            j * i * x
          }, {
            j + i + x
          })
          r
        })
      })
      t
    }

    def prog(x: Rep[Int]) = {

      /*val s = x + x
      val t = range_map(s, unit(10), (i: Rep[Int]) => {
        s
      })
      t*/

      /*val s = x + x
      val r = myifThenElse(unit(false), {
        x
      }, {
        s
      })
      r + s*/

      val t = range_map(unit(0), unit(10), (i: Rep[Int]) => {
        val t0 = expensive_pure_f(x,unit(2))
        val y = x / t0
        val s = i + y
        val mod = int_mod(s, unit(2))
        val rbool = ordering_lteq(mod, unit(0))
        val t1 = expensive_pure_f(i,unit(3))
        val r = myifThenElse(rbool, {
          t1
        }, {
          val t2 = expensive_pure_f(x,i)
          s
        })
        r
      })
      t
    }


    def graph() = {
      val (code, cm) = emitGraph.emitDepGraphf(prog)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check2.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

    def code() = {
      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
      val esc = codegen.emitSource(prog, "testClass", stream2)
      stream2.flush()
      stream2.close()
    }


  }

  val dsl = new DSL

  dsl.graph()
  dsl.code


  def expensive_pure_f(o: Int, p: Int): Int = ???


  def foo(x: Int) = {
    for (i <- 0 until 10) yield {
      val t0 = expensive_pure_f(x, 2) //move this out
      val y = x / t0 //move this out
      val s = i + y //this stays
      val t1 = expensive_pure_f(x, 3) //move in
      if (s % 2 == 0)
        t1
      else {
        val t3 = expensive_pure_f(x, i + 20) //deadcode
        s
      }
    }
  }


  def foo2(x: Int) = {
    val loopresult1 =
      for (i <- 0 until 10) yield {
        val c = expensive_pure_f(x, 2) //move outside
        val j = x * i //needs to stay inside
        val d = expensive_pure_f(2, x) //deadcode
        c * j
      }
    val loopresult2 =
      for (i <- 20 until 25) yield {
        val a = expensive_pure_f(x, 12) //move outside
        val b = expensive_pure_f(x, i) //move insde
        val c = Math.sqrt(x + i) //move inside
        val result = if (i % 2 == 0) {
            val d = Math.sqrt(x - 1) //deadcode
            a * b
          }
          else
            c
        result
      }

    (loopresult1, loopresult2)
  }


}
