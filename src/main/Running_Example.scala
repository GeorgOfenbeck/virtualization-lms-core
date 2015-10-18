import org.scala_lang.virtualized.SourceContext

import scala.lms.internal.FunctionsExp
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

/**
 * Georg Ofenbeck
 First created:
 * Date: 18/10/2015
 * Time: 11:10 
 */


trait MyRange extends PurePrimitiveOpsExp with FunctionsExp with ImplicitOpsExp {

  case class RangeMap[T: Manifest](start: Exp[Int], end: Exp[Int], body: Exp[_ => _]) extends Def[IndexedSeq[T]]

  def range_map[T](s: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[T])(implicit tr: TypeRep[T], mf: Manifest[T]): Exp[IndexedSeq[T]] = {
    val lambda = doInternalLambda(block)(exposeRepFromRep[Int],exposeRepFromRep[T])
    val cc = RangeMap[T](s, end, lambda.exp)
    toAtom(cc)
  }
}

trait ScalaGenMyRange extends ScalaCodegen with EmitHeadInternalFunctionAsClass {
  val IR: MyRange

  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block, String) => String): String = {
    tp.rhs match {
      case RangeMap(start, end, body) => {
        val thenlambda = exp2tp(body)
        val rets: String = (thenlambda.rhs) match {
          case (InternalLambda(tf, tx, ty, targs, treturns)) => {
            val l1 = "val " + quote(tp) + " = for (" + quote(tx.head) + " <- " + quote(start) + " until " + quote(end) + " ) yield {\n" //TODO: fix me!
            val l2 = block_callback(ty, l1)
            val trestuple: Vector[String] = ty.res.map(r => quote(r))
            val l3: String = l2 + tupledeclarehelper(trestuple, "")
            val l4 = l3 + "\n}"
            l4 + "\n"
          }
          case _ => {
            assert(false, "got an if statment which does not contain lambdas for its branches")
            ""
          }
        }
        rets
      }
      case _ => super.emitNode(tp, acc, block_callback)
    }
  }
}


object Running_Example extends App {

  class DSL extends PurePrimitiveOpsExp with MyRange with IfThenElsePureExp with BooleanOpsExp with OrderingOpsExp with ScalaCompile {
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

    def prog(x: Rep[Int]) = {

      //val l1 =

      //val f: Rep[Int] => Rep[Int] = (i: Rep[Int]) => i * x
      val t = range_map(unit(0),unit(10), (i: Rep[Int]) => {
        range_map(unit(0),unit(10), (j: Rep[Int]) => {
          val mod = int_mod(i,unit(2))
          val rbool =  ordering_lteq(mod,unit(0))
          val r = myifThenElse( rbool, { j * i * x }, { j + i + x})
          r
        })
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
      val esc = codegen.emitSource(prog,"testClass",stream2)
      stream2.flush()
      stream2.close()
    }


  }

  val dsl = new DSL

  dsl.graph()
  dsl.code




  def foo(x: Double) = {
    val loopresult1 =
      for (i <- 0 until 10) yield {
        val c = Math.pow(x, 2) //move outside
        val j = x * i //needs to stay inside
        val d = Math.pow(x, 13) //deadcode
        c * j
      }
    val loopresult2 =
      for (i <- 20 until 25) yield {
        val a = Math.pow(x, 12) //move outside
        val b = Math.pow(x, i) //move insde
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
