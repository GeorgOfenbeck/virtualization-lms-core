import java.io.PrintWriter

import scala.lms.ops._
import scala.lms.internal._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike.{EmitHeadInternalFunctionAsClass, ScalaGenBooleanOps}

/**
 * Georg Ofenbeck
 First created:
 * Date: 27/08/2015
 * Time: 10:22 
 */


trait DSL extends BooleanOpsExp with FunctionsExp{

 def test(x: Rep[Boolean])= {

  //this creates a function at the meta level
  val f1: Rep[Boolean] => Rep[Boolean] = (in: Rep[Boolean]) => boolean_negate(in)

  //this takes the function from the meta level and makes it a function at the next stage
  val stagedf1 = doLambda(f1)


  val res1 = f1(x) //in the next stage only the body of the function will be visible
  val res2 = stagedf1(x) //in the next stage a explicit call to a function will be visible

  boolean_and(res1,res2)
 }
}


/*****************************************
  Emitting Generated Code
  *******************************************/
class Test extends (((Boolean))=> ((Boolean))) {
 def apply( helper: ((Boolean))): ((Boolean)) = {
  val x0 : Boolean = helper

  val x3: ((Boolean)) => ((Boolean)) = (helper: ((Boolean))) =>{
   val x1 : Boolean = helper
   val x2 = !x1
   (x2)
  }
  val x4 = !x0
  val x6 =  x3(x0)
  val x7 = x6
  val x8 = x4 && x7
  (x8)
 }}
/*****************************************
  End of Generated Code
  *******************************************/



object myApp extends App{
 val mydsl = new DSL  { self =>
  val codegen = new ScalaGenBooleanOps with EmitHeadInternalFunctionAsClass{
   val IR: self.type = self
  }
  codegen.emitSource(test,"Test",new PrintWriter(System.out))
  val emitGraph = new GraphVizExport {
   override val IR: self.type = self
  }
  val (graph, cm) =emitGraph.emitDepGraphf(test)
  val stream = new java.io.PrintWriter(new java.io.FileOutputStream("Simple Functions.dot"))
  stream.println(graph)
  stream.flush()
  stream.close()
 }
}

