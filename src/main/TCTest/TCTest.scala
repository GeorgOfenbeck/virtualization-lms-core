package TCTest

/**
  * Georg Ofenbeck
  * First created:
  * Date: 14/09/2016
  * Time: 10:56 
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

class DSL extends BaseExp with FunctionsExp with BooleanOpsExpOpt with IfThenElsePureExp with PurePrimitiveOpsExp  with VectorOpsExp with OrderingOpsExp with RangeOpsExp with ImplicitOpsExp with ScalaCompile  {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }


  val f: Rep[Int] => Rep[Int] = (in: Rep[Int]) => in



  type NoRep[T] = T


  trait IRep[T[_]] {
    def exposeDyn(x: T[_]): Vector[Exp[_]]
    def exRep[R: TypeRep](x: T[R]): Vector[ExposeRep[_]]
  }

  implicit object isRep extends IRep[Rep]{
    def exposeDyn(x: Rep[_]): Vector[Exp[_]] = Vector(x)
    def exRep[T: TypeRep](x: Rep[T]): Vector[ExposeRep[_]] = Vector(exposeRepFromRep[T])
  }

  implicit object noRep extends IRep[NoRep]{
    def exposeDyn(x: NoRep[_]): Vector[Exp[_]] = Vector.empty
    def exRep[T: TypeRep](x: NoRep[T]): Vector[ExposeRep[_]] = Vector.empty
  }

  case class Header[R[_]: IRep](dyn: Exp[Int], maybe: R[Int])
  {


  }




  class Static(val a: Option[Int], val b: Option[Int])
  class Dynamic(val a: Option[Rep[Int]], val b: Option[Rep[Int]])
  class Mix(val a: Either [Rep[Int], Int], val b: Either[Rep[Int], Int])
  {
    def getStat(): Static = new Static(a.fold(fa => None, fb => Some(fb)),b.fold(fa => None, fb => Some(fb)))


  }


  /*
  class Static(val a: NoRep[Int], val b: NoRep[Int])
  {
   Option
  }

  class Mix[A[_],B[_]](val a: A[Int], val b: B[Int])
                      (implicit val ar: IRep[A], br: IRep[B])
  {
    def getStatic() = {

    }
  }*/



  def mix(s: Static, d: Dynamic): Mix = ???

  def genf(s: Static): StagedFunction[Dynamic, Rep[Int]] = {
    val expdyn: ExposeRep[Dynamic] = ???
    val exprepint = exposeRepFromRep[Int]

    def getdyn(s: Static) = ???

    val f: (Dynamic => Rep[Int]) = (d: Dynamic) => {

      val m = mix(s,d)
      val recurse = genf(s)
      val ret = recurse(d)
      ret
    }
    val sf: StagedFunction[Dynamic, Rep[Int]] = doGlobalLambda(f,false)(expdyn,exprepint)
    sf
  }





/*  def bla[R[_]: IRep](static: Int, maybe: R[Int])(implicit irep: IRep[R]): StagedFunction[R[Int],R[Int]]  = {

    val f: DynHeader => Rep[Int] = (in: Rep[Int]) => {
      if (i > 0) {
        val inner = returnf(i-1)
        inner(in)
      }
      else in
    }
    doGlobalLambda(f,false,Some("f" + i.toString()))
  }*/

  def returnf(i: Int): StagedFunction[Rep[Int],Rep[Int]] = {
    val f: Rep[Int] => Rep[Int] = (in: Rep[Int]) => {
      if (i > 0) {
        val inner = returnf(i-1)
        inner(in)
      }
      else in
    }
    doGlobalLambda(f,false,Some("f" + i.toString()))
  }


  def start(i: Rep[Int]):Rep[Int] = {
    val f = returnf(5)
    f(i)
  }

  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    val esc = codegen.emitSource(start, "testClass", stream2)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}



object TCTest extends App {

  val dsl = new DSL

  dsl.codeexport()


}
