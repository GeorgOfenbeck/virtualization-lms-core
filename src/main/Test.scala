

package examples


import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._




import scala.lms.targets.scalalike._

import scala.lms.targets.graphviz.GraphVizExport


/*trait TestExp extends BooleanOpsExp{

  case class Nest(b: Block) extends Def[Boolean]
  def nest(b: Exp[Boolean]) = Nest(Block(Vector(b)))

  override def boundExps(e: Any): Vector[Exp[_]] = e match{
    case Nest(b) => b.res
    case _ => {
      super.boundExps(e)
    }
  }
}*/
/*
trait ScalaGenTest extends ScalaCodegen {
  val IR: TestExp

  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block, String) => String): String = {
    val ma = tp.rhs match {
      case Nest(b) => "<Nest" + quote(tp) + ">\n" + block_callback(b, "") + "</Nest" + quote(tp) + ">\n"
      case _ => super.emitNode(tp, acc, block_callback)
    }
    ma
  }
}*/

object TestCompile extends App {
 testdsl()
 def testdsl(): Unit =  {

  class DSL extends BooleanOpsExp with PurePrimitiveOpsExp with FunctionsExp with IfThenElsePureExp with ScalaCompile  with ImplicitOpsExp{
   self =>
   override val codegen = new ScalaCodegen
     with EmitHeadInternalFunctionAsClass
     with ScalaGenBooleanOps
     with ScalaGenPrimitivOps
     with ScalaGenIfThenElse
     //with ScalaGenTest
   {
    val IR: self.type = self
   }
   val emitGraph = new GraphVizExport {
    override val IR: self.type = self
   }

   def f (x: Int) = x
   sameFunction(f _, f _)



   case class Complex(re: Rep[Int], im: Rep[Int])

   implicit val exposeComplex = new ExposeRep[Complex](){
    val freshExps = (u: Unit) => Vector(Arg[Int],Arg[Int])
    val vec2t: Vector[Exp[_]] => Complex = (in: Vector[Exp[_]]) => Complex(in.head.asInstanceOf[Rep[Int]],in.tail.head.asInstanceOf[Rep[Int]])
    val t2vec: Complex => Vector[Exp[_]] = (in: Complex) => Vector(in.re,in.im)
   }


   val FunctionOnComplex: Complex => StagedFunction[Complex,Complex] = (in: Complex) => {
    val FunctionOnComplex1: Complex => Complex = (in1: Complex) => {
     val repfun: (Rep[Int] => Rep[Int]) = (in: Rep[Int]) => in
     val reps = doLambda(repfun, false)
     Complex(in1.im, reps(in.re))
    }
    val sf = doLambda(FunctionOnComplex1, false)
    sf
   }


   def createsf(deepth: Int): StagedFunction[Rep[Int],Rep[Int]] = {

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

      val nums = for (j <- 0 until 1)
       yield (i + unit(j))

      val t0 = nums.reduce( (a,b) => {
       val t = a + b
       t
      })

      val t1 = t0
      val t2 = t0 * unit(-1)

      val t3 = t1 + t2
      val c = i - t3
      f.apply(c) + g.apply(c)
     }
     val sf = doLambda(h,false)
     sf
    }
   }

   def createsf2(deepth: Int): StagedFunction[Rep[Int],Rep[Int]] = {
    val f: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {
     val t = unit(1)
     i + t

    }
    val sf = doLambda(f, false)
    sf
   }

   val mystagedf: Rep[Int] => Rep[Int] = (i: Rep[Int]) => createsf(8).apply(i)
   //val iarg = exposeRepFromRep[Int]
   //val inest = exposeFunction[Complex,Complex]
   //val iret = exposeFunction[Complex,Complex => Complex](exposeComplex,inest)
   //val iret = exposeFunction[Complex,Complex]
   //val iret = exposeComplex
   val iarg = exposeRepFromRep[Int]
   val iret = exposeRepFromRep[Int]
   //val iarg = exposeComplex
   //val iret = exposeComplex

   //val iarg = exposeFunction[Complex,Complex]
   //val iret = exposeFunction[Complex,Complex]
  }
  val dsl = new DSL
  //val (code, esc) = dsl.emitString.emit("",dsl.mystagedf)(dsl.iarg,dsl.iret)


  //val esc = dsl.codegen.emitSource(dsl.mystagedf,"testClass",new PrintWriter(System.out))(dsl.iarg,dsl.iret)

  val (code, cm) = dsl.emitGraph.emitDepGraphf(dsl.mystagedf)(dsl.iarg,dsl.iret)
  val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check.dot"))
  stream.println(code)
  stream.flush()
  stream.close()

  val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
  val esc = dsl.codegen.emitSource(dsl.mystagedf,"testClass",stream2)(dsl.iarg,dsl.iret)
  stream2.flush()
  stream2.close()
  //dsl.compile(dsl.mystagedf)(dsl.iarg,dsl.iret)
  //println(code)
  //println("hae?")
 }
}



