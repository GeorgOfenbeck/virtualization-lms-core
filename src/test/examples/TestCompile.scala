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


      def mystagedf(x: Rep[Boolean]): Rep[Boolean] = {
        def innerf(u: Rep[Unit]): Rep[Boolean] = !x
        val f = innerf _
        println("same function: " + sameFunction(f,f ))
        val bla: Rep[Boolean] = ifThenElseLambda(x,f,f)
        bla
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


