/*
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



trait DSL2 extends BooleanOpsExp with FunctionsExp{

 case class BContainer(x: Rep[Boolean], y: Rep[Boolean])
 implicit val exposeBContainer = new ExposeRep[BContainer](){
  val freshExps = (u: Unit) => Vector(Arg[Boolean],Arg[Boolean])
  val vec2t: Vector[Exp[_]] => BContainer = (in: Vector[Exp[_]]) =>
   BContainer(in.head.asInstanceOf[Rep[Boolean]],in.tail.head.asInstanceOf[Rep[Boolean]])
  val t2vec: BContainer => Vector[Exp[_]] = (in: BContainer) => Vector(in.x,in.y)
 }

 def test(x: BContainer): BContainer= {

  //this creates a function at the meta level
  val f1: BContainer => BContainer = (in: BContainer) => BContainer(boolean_negate(in.x),boolean_negate(in.y))

  //this takes the function from the meta level and makes it a function at the next stage
  val stagedf1 = doLambda(f1)

  val res1 = f1(x) //in the next stage only the body of the function will be visible
  val res2 = stagedf1(x) //in the next stage a explicit call to a function will be visible

  BContainer(boolean_and(res1.x,res2.x),boolean_and(res1.y,res2.y))
 }
}

/*****************************************
  Emitting Generated Code
  *******************************************/
class Test2 extends (((Boolean,Boolean))=> ((Boolean,Boolean))) {
 def apply( helper: ((Boolean,Boolean))): ((Boolean,Boolean)) = {
  val x0 : Boolean = helper._1
  val x1 : Boolean = helper._2
  val x6: ((Boolean,Boolean)) => ((Boolean,Boolean)) = (helper: ((Boolean,Boolean))) =>{
   val x2 : Boolean = helper._1
   val x3 : Boolean = helper._2
   val x4 = !x2
   val x5 = !x3

   (x4,x5)
  }
  val x7 = !x0
  val x11 =  x6(x0, x1)

  val x8 = !x1
  val x13 = x11._2//returnarg  true
  val x12 = x11._1//returnarg  false
  val x15 = x8 && x13
  val x14 = x7 && x12

  (x14,x15)
 }}
/*****************************************
  End of Generated Code
  *******************************************/


object myApp2 extends App{
 val mydsl = new DSL2  { self =>
  val codegen = new ScalaGenBooleanOps with EmitHeadInternalFunctionAsClass{
   val IR: self.type = self
  }
  codegen.emitSource(test,"Test2",new PrintWriter(System.out))
  val emitGraph = new GraphVizExport {
   override val IR: self.type = self
  }
  val (graph, cm) =emitGraph.emitDepGraphf(test)
  val stream = new java.io.PrintWriter(new java.io.FileOutputStream("N-Ary Functions.dot"))
  stream.println(graph)
  stream.flush()
  stream.close()
 }
}

*/
