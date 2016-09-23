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


class DSL2 extends BaseExp with FunctionsExp with BooleanOpsExpOpt with IfThenElsePureExp with PurePrimitiveOpsExp  with VectorOpsExp with OrderingOpsExp with RangeOpsExp with ImplicitOpsExp with ScalaCompile  {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }


  abstract class NumericOps[T:Manifest] {	//inspired by Numeric
    lazy val m = implicitly[Manifest[T]]
    def plus       (x: T, y: T) : T
    def minus      (x: T, y: T) : T
    def times      (x: T, y: T) : T
    class Ops(lhs: T) {
      def +(rhs: T) = plus (lhs, rhs)
      def -(rhs: T) = minus(lhs, rhs)
      def *(rhs: T) = times(lhs, rhs)
    }
    def mkNumericOps(lhs: T): Ops = new Ops(lhs)
  }

  protected abstract class NumericRepOps[T:Manifest:Numeric] extends NumericOps[Rep[T]]  {
    def plus  (x : Rep[T], y: Rep[T]) = ??? ///numeric_plus [T](x, y)
    def minus (x : Rep[T], y: Rep[T]) = ??? //numeric_minus[T](x, y)
    def times (x : Rep[T], y: Rep[T]) = {
      lazy val m = implicitly[Manifest[T]]
      m.toString() match {
        case "Int" => {
          val r: Rep[Int] = int_times(x.asInstanceOf[Rep[Int]],y.asInstanceOf[Rep[Int]])
          r.asInstanceOf[Rep[T]]
        }
        case _ => ???
      }

    } //numeric_times[T](x, y)
  }
  implicit object IntRep extends NumericRepOps[Int]


  protected abstract class NumericNoRepOps[T:Numeric:Manifest] extends NumericOps[T]  {
    def plus  (x : T, y: T) = implicitly[Numeric[T]].plus (x, y)
    def minus (x : T, y: T) = implicitly[Numeric[T]].minus(x, y)
    def times (x : T, y: T) = implicitly[Numeric[T]].times(x, y)
  }
  implicit object IntNoRep    extends NumericNoRepOps[Int]
  implicit object LongNoRep   extends NumericNoRepOps[Float]
  implicit object FloatNoRep  extends NumericNoRepOps[Float]
  implicit object DoubleNoRep extends NumericNoRepOps[Double]

  trait IRep[T[_]] {
    def isRep(): Boolean
    def getRep[A](x: T[A]): Option[Rep[A]]
    def getNoRep[A](x: T[A]): Option[A]
    def fresh[A: TypeRep](): Vector[Rep[_]]
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Option[T[A]])
    /*
    def convert[G[_],A[_],X: TypeRep](me: T[X], that: A[X])(implicit evthat: IRep[A], evres: IRep[G]): (T[X],T[X])
    def convertc[X: TypeRep](me: T[X], that: NoRep[X]): (T[X],T[X])
    def convertc[X: TypeRep](me: T[X], that: Rep[X]): (Rep[X],Rep[X])
    */
  }

  implicit object isRep extends IRep[Rep] {
    val isRep = true
    def getRep[A](x: Rep[A]): Some[Rep[A]] = Some(x)
    def getNoRep[A](x: Rep[A]): Option[A] = None
    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector(Arg[A])
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Some[Rep[A]]) = (x.tail,Some(x.head.asInstanceOf[Rep[A]]))
    /*
    def convert[A[_], X: TypeRep](me: Rep[X], that: A[X])(implicit evthat: IRep[A]): (Rep[X],Rep[X]) = {
      val t = evthat.convertc[X](that,me)
      t
    }
    def convertc[X: TypeRep](me: Rep[X], that: NoRep[X]): (Rep[X],Rep[X]) = (me, Const(that))
    def convertc[X: TypeRep](me: Rep[X], that: Rep[X]): (Rep[X],Rep[X]) = (me, that)
    */

    //def push[A: TypeRep](x: Rep[A]): Vector[Rep[_]] = Vector(x)
  }
  implicit object noRep extends IRep[NoRep] {
    val isRep = false
    def getRep[A](x: NoRep[A]): Option[Rep[A]] = None
    def getNoRep[A](x: NoRep[A]): Some[A] = Some(x)
    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector.empty
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Option[NoRep[A]]) = (x,None)
    /*
    def convert[A[_],X: TypeRep](me: NoRep[X], that: A[X])(implicit evthat: IRep[A]): (A[X],A[X]) = evthat.convertc[X](that,me)
    def convertc[X: TypeRep](me: NoRep[X], that: NoRep[X]): (NoRep[X],NoRep[X]) = (me, that)
    def convertc[X: TypeRep](me: NoRep[X], that: Rep[X]): (Rep[X],Rep[X]) = (Const(me), that)
    */
  }



  abstract class Base[RA[_], RB[_]](a: RA[Int], b: RB[Int])
  trait isDynamic
  class Dyn[RA[_],RB[_]](a: RA[Int],b: RB[Int]) extends Base[RA,RB](a,b) with isDynamic
  trait isStatic
  class Static[RA[_],RB[_]](a: RA[Int],b: RB[Int]) extends Base[RA,RB](a,b) with isStatic
  class Full[RA[_],RB[_]](a: RA[Int],b: RB[Int]) extends Base[RA,RB](a,b) with isStatic with isDynamic



  //class Header[RA[_],RB[_],A,B](a: RA[A], b: RB[B])(implicit val eva: IRep[RA], val evb: IRep[RB])
  abstract class Header[RA[_],RB[_]](a: RA[Int], b: RB[Int])(implicit eva: IRep[RA], evb: IRep[RB])
  //access always together with static! need the - only needs to set
  class DynHeader[RA[_],RB[_]]( a: RA[Int], b: RB[Int], val eva: IRep[RA], val evb: IRep[RB]) extends Header(a,b)(eva,evb){
    def a(): Option[RA[Int]] = if(eva.isRep()) Some(a) else None
    def b(): Option[RB[Int]] = if(evb.isRep()) Some(b) else None
  }
  //access also alone
  class StatHeader[RA[_],RB[_]]( a: RA[Int], b: RB[Int], val eva: IRep[RA], val evb: IRep[RB]) extends Header(a,b)(eva,evb) {
    def a(): Option[RA[Int]] = if(!eva.isRep()) Some(a) else None
    def b(): Option[RB[Int]] = if(!evb.isRep()) Some(b) else None
  }
  class Mix[RA[_],RB[_]](val a: RA[Int], val b: RB[Int], val eva: IRep[RA], val evb: IRep[RB]) extends Header[RA,RB](a,b)(eva, evb)
  {
    def getStat(): StatHeader[RA,RB] = new StatHeader[RA,RB](a,b,eva,evb)
    def getDyn(): DynHeader[RA,RB] = new DynHeader[RA,RB](a,b,eva,evb)
  }

  object Mix{
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = {
      if (ev.isRep()) b.get else a.get
    }

    def apply[RA[_],RB[_]](hs: StatHeader[RA,RB], hd: DynHeader[RA,RB]): Mix[RA,RB] = {
      val a: RA[Int] = choose(hs.a(),hd.a(), hd.eva)
      val b: RB[Int] = choose(hs.b(),hd.b(), hd.evb)
      new Mix(a,b, hd.eva, hd.evb)
    }
  }

  implicit def exposeDynHeader[RA[_],RB[_]](stat: StatHeader[RA,RB]): ExposeRep[DynHeader[RA,RB]] = new ExposeRep[DynHeader[RA,RB]]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => stat.eva.fresh() ++ stat.evb.fresh()
      val vec2t: Vector[Exp[_]] => DynHeader[RA,RB] = (in: Vector[Exp[_]]) => {
        def help[T[_],A: TypeRep](in: Vector[Rep[_]], statele: Option[T[A]], ev: IRep[T]): (Vector[Rep[_]],T[A]) = {
          val (vecafter, ele) = ev.fetch[A](in)
          val res: T[A] = ele.getOrElse(statele.get)
          (vecafter,res)
        }
        val (aa, a) = help(in,stat.a(),stat.eva)
        val (ab, b) = help(in,stat.b(),stat.evb)
        new DynHeader[RA,RB](a,b,stat.eva,stat.evb)
      }
      val t2vec: DynHeader[RA,RB] => Vector[Exp[_]] = (in: DynHeader[RA,RB]) => {
        def help[T[_],A](ele: Option[T[A]], ev: IRep[T]): Vector[Rep[_]] = {
          if (ev.isRep()) Vector(ev.getRep(ele.get).get) else Vector.empty
        }
        help(in.a, in.eva) ++ help(in.b, in.evb)
      }
    }





  def genf[RA[_],RB[_]](hs: StatHeader[RA,RB])
                       (implicit eva: IRep[RA],
                        evb: IRep[RB],
                        gnuma: NumericOps[RA[Int]],
                        gnumb: NumericOps[RB[Int]]
                       ): StagedFunction[DynHeader[RA,RB], Rep[Int]]= {

    val expdyn: ExposeRep[DynHeader[RA,RB]] = exposeDynHeader(hs)
    val exprepint = exposeRepFromRep[Int]

    def stageme(hd: DynHeader[RA,RB]): Rep[Int] = {
      val mix: Mix[RA,RB] = Mix.apply[RA,RB](hs,hd)
      val a = gnuma.times(mix.a,mix.a)
      val b = gnumb.times(mix.b,mix.b)

      // split again here!
      val newmix = new Mix(a,b, mix.eva, mix.evb)
      val newstat = newmix.getStat()
      val newdyn = newmix.getDyn()
      val sf = genf(newstat)
      sf(newdyn)
    }

    val sf: StagedFunction[DynHeader[RA,RB], Rep[Int]] = doGlobalLambda(stageme,true)(expdyn,exprepint)
    sf
  }


  def bla[RA[_],RB[_]](a: RA[Int], b: RB[Int])(implicit ea: IRep[RA], eb: IRep[RB]): (RB[Int], RB[Int]) = {

    ???


  }


  def start(i: Rep[Int]):Rep[Int] = {
    val mix = new Mix(i,i,isRep,isRep)
    val s = mix.getStat()
    //val s: StatHeader[Rep,NoRep]  = ???
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

