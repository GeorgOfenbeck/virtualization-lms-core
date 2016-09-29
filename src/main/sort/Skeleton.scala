package sort


import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class ComplexVector

class Complex


trait Skeleton extends Sort_DSL {
  type SRep[T] = Either[Rep[T], NoRep[T]]
  type NoRep[T] = T

  def mkSRep[T](x: T): SRep[T] = Right(x)

  trait IRep[T[_]] extends RepBase[T] with Conditionals[T] with StagedNum[T] with Comparisons[T] with RangeFold[T]

  implicit object isRep extends IRep[Rep] with isRepBase with RepNum with RepConditionals with RepComparisons with RepRangeFold

  implicit object isNoRep extends IRep[NoRep] with noRepBase with NoRepNum with NoRepConditionals with NoRepComparisons with NoRepRangeFold

  implicit def getSRepEv[T](in: SRep[T]) =
    new mayRep[T] with SRepNum with SRepConditionals with SRepComparisons with SRepRangeFold {
      val x: SRep[T] = in
    }

  implicit def toSRep[T[_], A: TypeRep](x: T[A])(implicit ev: IRep[T]): SRep[A] = ev.toSRep(x)

  trait RepBase[T[_]] {
    def isRep(): Boolean

    def toRep[A: TypeRep](x: T[A]): Rep[A]

    def getRep[A](x: T[A]): Option[Rep[A]]

    def getNoRep[A](x: T[A]): Option[A]

    def fresh[A: TypeRep](): Vector[Rep[_]]

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Option[T[A]])

    def toSRep[A: TypeRep](x: T[A]): SRep[A]
  }

  trait isRepBase extends RepBase[Rep] {
    val isRep = true

    def toRep[A: TypeRep](x: Rep[A]): Rep[A] = x

    def getRep[A](x: Rep[A]): Some[Rep[A]] = Some(x)

    def getNoRep[A](x: Rep[A]): Option[A] = None

    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector(Arg[A])

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Some[Rep[A]]) = (x.tail, Some(x.head.asInstanceOf[Rep[A]]))

    def toSRep[A: TypeRep](x: Rep[A]): SRep[A] = Left(x)
  }

  trait noRepBase extends RepBase[NoRep] {
    val isRep = false

    def toRep[A: TypeRep](x: NoRep[A]): Rep[A] = Const(x)

    def getRep[A](x: NoRep[A]): Option[Rep[A]] = None

    def getNoRep[A](x: NoRep[A]): Some[A] = Some(x)

    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector.empty

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Option[NoRep[A]]) = (x, None)

    def toSRep[A: TypeRep](x: NoRep[A]): SRep[A] = Right(x)
  }

  trait mayRep[T] extends RepBase[SRep] {
    val x: SRep[T]

    def isRep(): Boolean = x.isLeft

    def toRep[A: TypeRep](x: SRep[A]): Rep[A] = x.fold(fa => fa, fb => Const(fb))

    def getRep[A](x: SRep[A]): Option[Rep[A]] = x.fold(fa => Some(fa), fb => None)

    def getNoRep[A](x: SRep[A]): Option[A] = x.fold(fa => None, fb => Some(fb))

    def fresh[A: TypeRep](): Vector[Rep[_]] = x.fold(fa => Vector(Arg[A]), fb => Vector.empty)

    def fetch[A: TypeRep](in: Vector[Rep[_]]): (Vector[Rep[_]], Option[SRep[A]]) = x.fold(fa => (in.tail, Some(Left(in.head.asInstanceOf[Rep[A]]))), fb => (in, None))

    def toSRep[A: TypeRep](x: SRep[A]): SRep[A] = x
  }


  trait Conditionals[T[_]] {
    def _if[A](cond: T[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A
  }

  trait RepConditionals extends Conditionals[Rep] {
    def _if[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = myifThenElse(cond, thenp, elsep)
  }

  trait NoRepConditionals extends Conditionals[NoRep] {
    def _if[A](cond: NoRep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = if (cond) thenp else elsep
  }

  trait SRepConditionals extends Conditionals[SRep] {
    def _if[A](cond: SRep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A =
      cond.fold(fa => myifThenElse(fa, thenp, elsep), fb => if (fb) thenp else elsep)
  }

  trait RangeFold[T[_]] {
    implicit def mkRangeOps(lhs: T[Int]): Ops = new Ops(lhs)

    class Ops(lhs: T[Int]) {
      def until(rhs: T[Int]): T[Range] = unt(lhs, rhs)
    }

    implicit def mkFoldOps(lhs: T[Range]): OpsF = new OpsF(lhs)
    class OpsF(r: T[Range]) {
      def foldLeft[B](ini: B)(body: ((B, T[Int])) => B)(implicit exposeRep: ExposeRep[B]): B = rangefold(r, ini, exposeRep)(body)
    }

    def unt(from: T[Int], to: T[Int]): T[Range]

    def rangefold[B](range: T[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, T[Int])) => B): B
  }

  trait RepRangeFold extends RangeFold[Rep] {
    def unt(from: Rep[Int], to: Rep[Int]): Rep[Range] = range_create(from, to)

    def rangefold[B](range: Rep[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, Rep[Int])) => B): B = range_foldLeft(range,ini,body)(exposeRep)
  }

  trait NoRepRangeFold extends RangeFold[NoRep] {
    def unt(from: NoRep[Int], to: NoRep[Int]): NoRep[Range] = Range(from, to)
    def rangefold[B](range: NoRep[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, NoRep[Int])) => B): B = {
      val f: (B,Int) => B = (b: B,i : Int) => body((b,i))
      range.foldLeft(ini)(f)
    }
  }

  trait SRepRangeFold extends RangeFold[SRep] {
    def unt(from: SRep[Int], to: SRep[Int]): SRep[Range] = srdecomp(from, to, range_create, (x: Int, y: Int) => Range(x, y))
    def rangefold[B](range: SRep[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, SRep[Int])) => B): B = {
      range.fold(
        fa => {
          val f: ((B,Rep[Int])) => B = (tup) => {
            val t: SRep[Int] = Left(tup._2)
            body((tup._1,t))
          }
          range_foldLeft(fa,ini,f)(exposeRep)
        },
        fb => {
          val f: (B,Int) => B = (b: B,i : Int) => body((b,mkSRep(i)))
          fb.foldLeft(ini)(f)
        })

    }
  }


  trait Comparisons[T[_]] {
    implicit def mkComparisonOps[A: Ordering : Manifest](lhs: T[A]): Ops[A] = new Ops[A](lhs)

    class Ops[A: Ordering : Manifest](lhs: T[A]) {
      def ==(rhs: T[A]): T[Boolean] = equiv(lhs, rhs)

      def <(rhs: T[A]): T[Boolean] = less(lhs, rhs)
    }

    def equiv[A: Ordering : Manifest](lhs: T[A], rhs: T[A]): T[Boolean]

    def less[A: Ordering : Manifest](lhs: T[A], rhs: T[A]): T[Boolean]
  }

  trait RepComparisons extends Comparisons[Rep] {
    def equiv[T: Ordering : Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean] = ordering_equiv(lhs, rhs)

    def less[T: Ordering : Manifest](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean] = ordering_lt(lhs, rhs)
  }

  trait NoRepComparisons extends Comparisons[NoRep] {
    def equiv[T: Ordering : Manifest](lhs: NoRep[T], rhs: NoRep[T]): NoRep[Boolean] = lhs == rhs

    def less[T: Ordering : Manifest](lhs: NoRep[T], rhs: NoRep[T]): NoRep[Boolean] = implicitly[Ordering[T]].lt(lhs, rhs)
  }

  trait SRepComparisons extends Comparisons[SRep] {
    def equiv[T: Ordering : Manifest](lhs: SRep[T], rhs: SRep[T]): SRep[Boolean] = {
      val x = lhs
      val y = rhs
      x.fold(xfa => y.fold(yfa => Left(ordering_equiv(xfa, yfa)), yfb => Left(ordering_equiv(xfa, y.toRep(y)))),
        xfb => y.fold(yfa => Left(ordering_equiv(x.toRep(x), yfa)), yfb => Right(xfb == yfb)))
    }

    def less[T: Ordering : Manifest](lhs: SRep[T], rhs: SRep[T]): SRep[Boolean] = {
      val x = lhs
      val y = rhs
      x.fold(xfa => y.fold(yfa => Left(ordering_lt(xfa, yfa)), yfb => Left(ordering_lt(xfa, y.toRep(y)))),
        xfb => y.fold(yfa => Left(ordering_lt(x.toRep(x), yfa)), yfb => Right(implicitly[Ordering[T]].lt(xfb, yfb))))
    }
  }

  trait StagedNum[T[_]] extends RepBase[T] {
    implicit def mkNumericOps(lhs: T[Int]): Ops = new Ops(lhs)

    class Ops(lhs: T[Int]) {

      def +(rhs: T[Int]) = plus(lhs, rhs)

      def -(rhs: T[Int]) = minus(lhs, rhs)

      def /(rhs: T[Int]) = div(lhs,rhs)
    }

    def plus[A[_]](lhs: T[Int], rhs: A[Int])(implicit evl: StagedNum[T], evr: StagedNum[A]): SRep[Int] = {
      val t1 = evl.toSRep(lhs)
      val t2 = evr.toSRep(rhs)
      val ev = getSRepEv(t1)
      ev.plus(t1, t2)
    }

    def minus[A[_]](lhs: T[Int], rhs: A[Int])(implicit evl: StagedNum[T], evr: StagedNum[A]): SRep[Int] = {
      val t1 = evl.toSRep(lhs)
      val t2 = evr.toSRep(rhs)
      val ev = getSRepEv(t1)
      ev.minus(t1, t2)
    }

    def div[A[_]](lhs: T[Int], rhs: A[Int])(implicit evl: StagedNum[T], evr: StagedNum[A]): SRep[Int] = {
      val t1 = evl.toSRep(lhs)
      val t2 = evr.toSRep(rhs)
      val ev = getSRepEv(t1)
      ev.div(t1, t2)
    }

    def plus(lhs: T[Int], rhs: T[Int]): T[Int]

    def minus(lhs: T[Int], rhs: T[Int]): T[Int]
    def div(lhs: T[Int], rhs: T[Int]): T[Int]
  }

  trait RepNum extends StagedNum[Rep] {
    def plus(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_plus(lhs, rhs)

    def minus(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_minus(lhs, rhs)
    def div(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_divide(lhs, rhs)
  }

  trait NoRepNum extends StagedNum[NoRep] {
    def plus(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = lhs + rhs

    def minus(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = lhs - rhs
    def div(lhs: NoRep[Int], rhs: NoRep[Int]): NoRep[Int] = lhs / rhs
  }


  def srdecomp[T: TypeRep, A](x: SRep[T], y: SRep[T], sf: (Rep[T], Rep[T]) => Rep[A], f: (T, T) => A): SRep[A] = {
    x.fold(xfa => y.fold(yfa => Left(sf(xfa, yfa)), yfb => Left(sf(xfa, y.toRep(y)))),
      xfb => y.fold(yfa => Left(sf(x.toRep(x), yfa)), yfb => Right(f(xfb, yfb))))
  }

  trait SRepNum extends StagedNum[SRep] {
    def plus(lhs: SRep[Int], rhs: SRep[Int]): SRep[Int] = srdecomp(lhs, rhs, int_plus, (x: Int, y: Int) => x + y)

    def minus(lhs: SRep[Int], rhs: SRep[Int]): SRep[Int] = srdecomp(lhs, rhs, int_minus, (x: Int, y: Int) => x - y)
    def div(lhs: SRep[Int], rhs: SRep[Int]): SRep[Int] = srdecomp(lhs, rhs, int_divide, (x: Int, y: Int) => x / y)
  }


  trait RepSelector {
    val rrep: Boolean

    def repselect[A[_], T](a: A[T], ev: IRep[A]): Option[A[T]] =
      if (rrep) {
        if (ev.isRep()) Some(a) else None
      } else if (!ev.isRep()) None else Some(a)
  }

  trait DynSelector {
    val rrep: Boolean = true
  }

  trait StatSelector {
    val rrep: Boolean = false
  }

  abstract class Base[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int],
                                        eva: IRep[A], evb: IRep[B], evc: IRep[C])

  abstract class SortHeader[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int],
                                              eva: IRep[A], evb: IRep[B], evc: IRep[C])
    extends Base(start, end, basesize, eva, evb, evc) with RepSelector {
    def start(): Option[A[Int]] = repselect(start, eva)

    def end(): Option[B[Int]] = repselect(end, evb)

    def basesize(): Option[C[Int]] = repselect(basesize, evc)
  }


  class DynHeader[A[_], B[_], C[_]](val x: Rep[Vector[Int]], start: A[Int], end: B[Int], basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends SortHeader(start, end, basesize, eva, evb, evc) with DynSelector

  object StatHeader{
    def apply[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int])(implicit eva: IRep[A], evb: IRep[B], evc: IRep[C]): StatHeader[A,B,C] =
      new StatHeader[A,B,C](start,end,basesize,eva,evb,evc)
  }

  class StatHeader[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends SortHeader(start, end, basesize, eva, evb, evc) with StatSelector{
    def genSig(): String = {
      val s = start().get match{
        case x: NoRep[Int] => x.toString
        case _ => ""
      }
      val e = end().get match{
        case x: NoRep[Int] => x.toString
        case _ => ""
      }
      val b = basesize().get match{
        case x: NoRep[Int] => x.toString
        case _ => ""
      }
      s ++ e ++ b
    }
  }

  class MixSortHeader[A[_], B[_], C[_]](val x: Rep[Vector[Int]], val start: A[Int], val end: B[Int], val basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends Base(start, end, basesize, eva, evb, evc) {
    def getDynHeader(): DynHeader[A,B,C] = new DynHeader[A,B,C](x,start,end,basesize,eva,evb,evc)
    def getStatHeader(): StatHeader[A,B,C] = new StatHeader[A,B,C](start,end,basesize,eva,evb,evc)

    def split(): (StatHeader[A,B,C], DynHeader[A,B,C]) = (getStatHeader(),getDynHeader())
  }

  object MixSortHeader {
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    def apply[RA[_], RB[_], RC[_]](hs: StatHeader[RA, RB, RC], hd: DynHeader[RA, RB, RC]): MixSortHeader[RA, RB, RC] = {
      val a: RA[Int] = choose(hs.start(), hd.start(), hs.eva)
      val b: RB[Int] = choose(hs.end(), hd.end(), hs.evb)
      val c: RC[Int] = choose(hs.basesize(), hd.basesize(), hs.evc)
      new MixSortHeader[RA, RB, RC](hd.x,a, b, c, hs.eva, hs.evb, hs.evc)
    }
    def apply[RA[_], RB[_], RC[_]](x: Rep[Vector[Int]], start: RA[Int], end: RB[Int], basesize: RC[Int])(implicit eva: IRep[RA], evb: IRep[RB], evc: IRep[RC]):MixSortHeader[RA,RB,RC] = new MixSortHeader[RA,RB,RC](x,start,end,basesize,eva,evb,evc)
  }


  implicit def exposeDynHeader[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): ExposeRep[DynHeader[A, B, C]] =
    new ExposeRep[DynHeader[A, B, C]]() {

      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[Vector[Int]]) ++ stat.eva.fresh[Int]() ++ stat.evb.fresh[Int]() ++ stat.evc.fresh[Int]()
      val vec2t: Vector[Exp[_]] => DynHeader[A, B, C] = (in: Vector[Exp[_]]) => {
        def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: Option[T[A]], ev: IRep[T]): (Vector[Rep[_]], T[A]) = {
          val (vecafter, ele) = ev.fetch[A](in)
          val res: T[A] = ele.getOrElse(statele.get)
          (vecafter, res)
        }
        val x = in.head.asInstanceOf[Rep[Vector[Int]]]
        val (ostart, outstart) = help(in.tail, stat.start(), stat.eva)
        val (oend, outend) = help(ostart, stat.end(), stat.evb)
        val (obs, outbs) = help(oend, stat.basesize(), stat.evc)
        new DynHeader[A, B, C](x, outstart, outend, outbs, stat.eva, stat.evb, stat.evc)
      }

      val t2vec: DynHeader[A, B, C] => Vector[Exp[_]] = (in: DynHeader[A, B, C]) => {
        def help[T[_], A](ele: Option[T[A]], ev: IRep[T]): Vector[Exp[_]] = {
          ele.map(p => ev.getRep(p).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
        }
        Vector(in.x) ++ help(in.start(), in.eva) ++ help(in.end(), in.evb) ++ help(in.basesize(), in.evc)
      }

    }


  implicit def exposeTuple[A: TypeRep, B: TypeRep](): ExposeRep[(Rep[A], Rep[B])] = new ExposeRep[(Rep[A], Rep[B])] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[A], Arg[B])
    val vec2t: Vector[Exp[_]] => ((Exp[A], Exp[B])) = (in: Vector[Exp[_]]) => {
      val a = in.head.asInstanceOf[Exp[A]]
      val b = in.tail.head.asInstanceOf[Exp[B]]
      (a, b)
    }
    val t2vec: ((Exp[A], Exp[B])) => Vector[Exp[_]] = (in: ((Exp[A], Exp[B]))) => Vector(in._1, in._2)
  }

}

