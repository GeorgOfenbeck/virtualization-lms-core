package TCTest

/**
  * Georg Ofenbeck
  * First created:
  * Date: 16/09/2016
  * Time: 12:57 
  */

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._
import scala.spores._
import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling._
import scala.pickling.json._
import SporePicklers._

class DSL2 extends BaseExp with FunctionsExp with BooleanOpsExpOpt with IfThenElsePureExp with PurePrimitiveOpsExp  with VectorOpsExp with OrderingOpsExp with RangeOpsExp with ImplicitOpsExp with ScalaCompile  {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }



  trait IRep[T[_]]

  implicit object isRep extends IRep[Rep]
  implicit object noRep extends IRep[NoRep]

  trait isStatic
  trait isDynamic
  //class Header[RA[_],RB[_],A,B](a: RA[A], b: RB[B])(implicit val eva: IRep[RA], val evb: IRep[RB])
  class Header[RA[_],RB[_]](a: RA[Int], b: RB[Int])(implicit val eva: IRep[RA], val evb: IRep[RB])
  class Mix[RA[_],RB[_]](a: RA[Int], b: RB[Int]) extends Header[RA,RB](a,b) with isStatic with isDynamic

  object Mix{
    def apply[RA[_],RB[_]](hs: Header[RA,RB] with isStatic, hd: Header[RA,RB] with isDynamic): Mix[RA,RB] = {
      ???
    }
  }


  def genf[RA[_],RB[_]](hs: Header[RA,RB] with isStatic)(implicit eva: IRep[RA], evb: IRep[RB]):
  StagedFunction[Header[RA,RB] with isDynamic, Rep[Int]]= {

    val expdyn: ExposeRep[Header[RA,RB] with isDynamic] = ???
    val exprepint = exposeRepFromRep[Int]

    def stageme(hd: Header[RA,RB] with isDynamic): Rep[Int] = {
      val mix: Mix[RA,RB] = Mix.apply[RA,RB](hs,hd)

      ???
    }

    val sf: StagedFunction[Header[RA,RB] with isDynamic, Rep[Int]] = doGlobalLambda(stageme,false)(expdyn,exprepint)
    sf


  }

  def start(i: Rep[Int]):Rep[Int] = {
    val s: Header[Rep,NoRep] with isStatic = ???
    val sf = genf(s)
    println(sf)
    i
  }


  type NoRep[T] = T



  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    val esc = codegen.emitSource(start, "testClass", stream2)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}



object PolyTest extends App {

  val dsl = new DSL2

  dsl.codeexport()


}

