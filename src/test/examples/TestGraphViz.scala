package examples


import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._



import org.scalatest._

import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


class TestGraphViz  extends Suite {
 def testdsl(): Unit =  {

  class DSL extends BooleanOpsExp with InternalFunctionsExp{
   self =>
   val emitGraph = new GraphVizExport {
    override val IR: self.type = self
   }
   def mystagedf(x: Rep[Boolean]): Rep[Boolean] = x && x

   val iarg = exposeRepFromRep[Boolean]
   val iret = exposeRepFromRep[Boolean]
  }
  val dsl = new DSL
  //val (code, esc) = dsl.emitString.emit("",dsl.mystagedf)(dsl.iarg,dsl.iret)
  val (code, cm) = dsl.emitGraph.emitDepGraphf(dsl.mystagedf)(dsl.iarg,dsl.iret)

  val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check.dot"))

  stream.println(code)
  stream.flush()
  stream.close()
  /*val (code, esc) =
  */


  println("hae2?")
 }
}