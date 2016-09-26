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
  trait IRep[T[_]] extends Conditionals[T]{
    def isRep(): Boolean
    def getRep[A](x: T[A]): Option[Rep[A]]
    def getNoRep[A](x: T[A]): Option[A]
    def fresh[A: TypeRep](): Vector[Rep[_]]
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Option[T[A]])
  }

  implicit object isRep extends IRep[Rep] with RepConditionals{
    val isRep = true
    def getRep[A](x: Rep[A]): Some[Rep[A]] = Some(x)
    def getNoRep[A](x: Rep[A]): Option[A] = None
    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector(Arg[A])
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Some[Rep[A]]) = (x.tail,Some(x.head.asInstanceOf[Rep[A]]))
  }
  implicit object noRep extends IRep[NoRep] with NoRepConditionals{
    val isRep = false
    def getRep[A](x: NoRep[A]): Option[Rep[A]] = None
    def getNoRep[A](x: NoRep[A]): Some[A] = Some(x)
    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector.empty
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Option[NoRep[A]]) = (x,None)
  }


  trait Conditionals[T[_]]{
    def _if[A](cond: T[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A
  }

  trait RepConditionals extends Conditionals[Rep] {
    def _if[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = myifThenElse(cond,thenp,elsep)
  }
  trait NoRepConditionals extends Conditionals[NoRep] {
    def _if[A](cond: NoRep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = if(cond) thenp else elsep
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

  class StatHeader[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends SortHeader(start, end, basesize, eva, evb, evc) with StatSelector

  class MixSortHeader[A[_], B[_], C[_]](val start: A[Int], val end: B[Int], val basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends Base(start, end, basesize, eva, evb, evc)

  object MixSortHeader {
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    def apply[RA[_], RB[_], RC[_]](hs: StatHeader[RA, RB, RC], hd: DynHeader[RA, RB, RC]): MixSortHeader[RA, RB, RC] = {
      val a: RA[Int] = choose(hs.start(), hd.start(), hs.eva)
      val b: RB[Int] = choose(hs.end(), hd.end(), hs.evb)
      val c: RC[Int] = choose(hs.basesize(), hd.basesize(), hs.evc)
      new MixSortHeader[RA, RB, RC](a, b, c, hs.eva, hs.evb, hs.evc)
    }
  }


  implicit def exposeDynHeader[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): ExposeRep[DynHeader[A, B, C]] =
    new ExposeRep[DynHeader[A, B, C]]() {

      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[Vector[Int]]) ++ stat.eva.fresh() ++ stat.evb.fresh() ++ stat.evc.fresh()
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

