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



trait DSL4 extends BooleanOpsExp with FunctionsExp{

 case class BContainer(x: Rep[Boolean], y: Rep[Boolean])

 implicit val exposeBContainer = new ExposeRep[BContainer](){
  val freshExps = (u: Unit) => Vector(Arg[Boolean],Arg[Boolean])
  val vec2t: Vector[Exp[_]] => BContainer = (in: Vector[Exp[_]]) =>
   BContainer(in.head.asInstanceOf[Rep[Boolean]],in.tail.head.asInstanceOf[Rep[Boolean]])
  val t2vec: BContainer => Vector[Exp[_]] = (in: BContainer) => Vector(in.x,in.y)
 }


 def test(x: BContainer): StagedFunction[BContainer,BContainer] = {
  val f1: BContainer => BContainer = (in: BContainer) => BContainer(boolean_and(in.x,x.x),boolean_and(in.y,x.y))
  val stagedf1 = doLambda(f1)

  val f2: StagedFunction[BContainer,BContainer] => StagedFunction[BContainer,BContainer] = (in: StagedFunction[BContainer,BContainer]) => in
  val stagedf2 = doLambda(f2)

  stagedf2(stagedf1)
 }
}
/*****************************************
  Emitting Generated Code
  *******************************************/
class Test4 extends (((Boolean,Boolean))=> ((scala.Function1[(Boolean,Boolean),(Boolean,Boolean)]))) {
 def apply( helper: ((Boolean,Boolean))): ((scala.Function1[(Boolean,Boolean),(Boolean,Boolean)])) = {
  val x0 : Boolean = helper._1
  val x1 : Boolean = helper._2
  val x6: ((Boolean,Boolean)) => ((Boolean,Boolean)) = (helper: ((Boolean,Boolean))) =>{
   val x2 : Boolean = helper._1
   val x3 : Boolean = helper._2
   val x4 = x2 && x0
   val x5 = x3 && x1

   (x4,x5)
  }

  (x6)
 }}
/*****************************************
  End of Generated Code
  *******************************************/


object myApp4 extends App{
 val mydsl = new DSL4  { self =>
  val codegen = new ScalaGenBooleanOps with EmitHeadInternalFunctionAsClass{
   val IR: self.type = self
  }
  codegen.emitSource(test,"Test3",new PrintWriter(System.out))
  val emitGraph = new GraphVizExport {
   override val IR: self.type = self
  }
  val (graph, cm) =emitGraph.emitDepGraphf(test)
  val stream = new java.io.PrintWriter(new java.io.FileOutputStream("HigherOrder_nested.dot"))
  stream.println(graph)
  stream.flush()
  stream.close()
 }
}

