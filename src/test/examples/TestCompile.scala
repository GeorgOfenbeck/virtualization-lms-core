package examples


import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._



import org.scalatest._

import scala.lms.targets.scalalike._

import scala.lms.targets.graphviz.GraphVizExport

class TestCompile extends Suite {
  def testdsl(): Unit =  {

    class DSL extends BooleanOpsExp with InternalFunctionsExp with IfThenElseExp with ScalaCompile{
      self =>
      override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenBooleanOps with ScalaGenIfThenElse{
        val IR: self.type = self
      }
      val emitGraph = new GraphVizExport {
        override val IR: self.type = self
      }

      def f (x: Int) = x
      sameFunction(f _, f _)


      implicit val exposeComplex = new ExposeRep[Complex](){
        val freshExps = (u: Unit) => Vector(Arg[Boolean],Arg[Boolean])
        val vec2t: Vector[Exp[_]] => Complex = (in: Vector[Exp[_]]) => Complex(in.head.asInstanceOf[Rep[Boolean]],in.tail.head.asInstanceOf[Rep[Boolean]])
        val t2vec: Complex => Vector[Exp[_]] = (in: Complex) => Vector(in.re,in.im)
      }


      case class Complex(re: Rep[Boolean], im: Rep[Boolean])
      val innerf = (b: Rep[Boolean]) =>  b
      val FunctionOnComplex = (in: Complex) => Complex(in.im,in.re)
      def mystagedf(x: Rep[Boolean]): Rep[Boolean] = {
        val complex = Complex(x,x)
        val sf = doLambda(innerf)
        val stageFunctiononComplex = doLambda(FunctionOnComplex)
        val retcomplex = stageFunctiononComplex(complex)
        val ret = sf(retcomplex.im)
        ret
      }

      val iarg = exposeRepFromRep[Boolean]
      val iret = exposeRepFromRep[Boolean]
    }
    val dsl = new DSL
    //val (code, esc) = dsl.emitString.emit("",dsl.mystagedf)(dsl.iarg,dsl.iret)

    //val esc = dsl.emitString.emitSource(dsl.mystagedf,"testClass",new PrintWriter(System.out))(dsl.iarg,dsl.iret)
    //val esc = dsl.codegen.emitSource(dsl.mystagedf,"testClass",new PrintWriter(System.out))(dsl.iarg,dsl.iret)

    val (code, cm) = dsl.emitGraph.emitDepGraphf(dsl.mystagedf)(dsl.iarg,dsl.iret)
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check.dot"))
    stream.println(code)
    stream.flush()
    stream.close()

    //dsl.compile(dsl.mystagedf)(dsl.iarg,dsl.iret)
    /*val (code, esc) =
    println(code)*/
    println("hae?")
  }
}


