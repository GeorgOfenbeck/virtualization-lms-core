package sort

import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

trait SortHeader extends Skeleton {
  abstract class Base[G,A[_], B[_], C[_], AB[_]](start: A[Int], end: B[Int], basesize: C[Int],
                                                 gtyp: TypeRep[G], vtyp: TypeRep[Array[G]], eva: IRep[A], evb: IRep[B], evc: IRep[C], evab: IRep[AB],
                                                 lub1: Lub[A, B, AB], lub2: Lub[AB, B, AB], lub3: Lub[A, AB, AB], lub4: Lub[AB, AB, AB])

  abstract class SortHeader[G,A[_], B[_], C[_], AB[_]](start: A[Int], end: B[Int], basesize: C[Int],
                                                       gtyp: TypeRep[G], vtyp: TypeRep[Array[G]], eva: IRep[A], evb: IRep[B], evc: IRep[C], evab: IRep[AB],
                                                       lub1: Lub[A, B, AB], lub2: Lub[AB, B, AB], lub3: Lub[A, AB, AB], lub4: Lub[AB, AB, AB])
    extends Base(start, end, basesize, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4) with RepSelector {
    def start(): Option[A[Int]] = repselect(start, eva)

    def end(): Option[B[Int]] = repselect(end, evb)

    def basesize(): Option[C[Int]] = repselect(basesize, evc)
  }

  case class InlineInfo(inline: Boolean, maxfunctions: Int, compareinline: Boolean, consider_inline: Boolean)




  class DynHeader[G,A[_], B[_], C[_], AB[_]](val x: Rep[Array[G]], start: A[Int], end: B[Int], basesize: C[Int], val gtyp: TypeRep[G],val vtyp: TypeRep[Array[G]], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C], evab: IRep[AB], lub1: Lub[A, B, AB], lub2: Lub[AB, B, AB], lub3: Lub[A, AB, AB], lub4: Lub[AB, AB, AB]) extends SortHeader(start, end, basesize, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4) with DynSelector

  class DynHeaderPlus[G,A[_], B[_], C[_], AB[_]](val cinline: Rep[Boolean], override val x: Rep[Array[G]], start: A[Int], end: B[Int], basesize: C[Int], override val gtyp: TypeRep[G], override val vtyp: TypeRep[Array[G]], override val eva: IRep[A], override val evb: IRep[B], override val evc: IRep[C], evab: IRep[AB], lub1: Lub[A, B, AB], lub2: Lub[AB, B, AB], lub3: Lub[A, AB, AB], lub4: Lub[AB, AB, AB]) extends DynHeader(x, start, end, basesize, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4)

  object StatHeader {
    def apply[G, A[_], B[_], C[_], AB[_]](start: A[Int], end: B[Int], basesize: C[Int], comp: ((Rep[G],Rep[G])) => Rep[Int], inline: InlineInfo)(implicit gtyp: TypeRep[G],vtyp: TypeRep[Array[G]], eva: IRep[A], evb: IRep[B], evc: IRep[C], evab: IRep[AB], lub1: Lub[A, B, AB], lub2: Lub[AB, B, AB], lub3: Lub[A, AB, AB], lub4: Lub[AB, AB, AB]): StatHeader[G,A, B, C, AB] =
      new StatHeader[G,A, B, C, AB](start, end, basesize, comp, inline, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4)
  }

  class StatHeader[G, A[_], B[_], C[_], AB[_]](start: A[Int], end: B[Int], basesize: C[Int], val comp: ((Rep[G],Rep[G])) => Rep[Int], val inline: InlineInfo, val gtyp: TypeRep[G], val vtyp: TypeRep[Array[G]], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C], val evab: IRep[AB],val lub1: Lub[A, B, AB], val lub2: Lub[AB, B, AB], val lub3: Lub[A, AB, AB], val lub4: Lub[AB, AB, AB]) extends SortHeader(start, end, basesize, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4) with StatSelector {
    def genSig(): String = {
      val s = start() match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }
      val e = end() match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }
      val b = basesize() match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }
      "start:" ++ s ++ "end:" ++ e ++ "base:" ++ b ++ inline.toString
    }
  }

  case class MixSortHeader[G,A[_], B[_], C[_], AB[_]](val x: Rep[Array[G]], val start: A[Int], val end: B[Int], val basesize: C[Int], val comp: ((Rep[G],Rep[G])) => Rep[Int], val inline: InlineInfo, val gtyp: TypeRep[G], val vtyp: TypeRep[Array[G]], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C], val evab: IRep[AB], val lub1: Lub[A, B, AB], val lub2: Lub[AB, B, AB], val lub3: Lub[A, AB, AB], val lub4: Lub[AB, AB, AB]
                                                     ) extends Base(start, end, basesize, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4) {
    def getDynHeader(): DynHeader[G,A, B, C, AB] = new DynHeader[G,A, B, C, AB](x, start, end, basesize, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4)
    def getStatHeader(): StatHeader[G,A, B, C, AB] = new StatHeader[G,A, B, C, AB](start, end, basesize, comp, inline, gtyp, vtyp, eva, evb, evc, evab, lub1, lub2, lub3, lub4)

    def split(): (StatHeader[G,A, B, C, AB], DynHeader[G,A, B, C, AB]) = (getStatHeader(), getDynHeader())
  }

  object MixSortHeader {
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    def apply[G,RA[_], RB[_], RC[_], RAB[_]](hs: StatHeader[G,RA, RB, RC, RAB], hd: DynHeader[G,RA, RB, RC, RAB]): MixSortHeader[G,RA, RB, RC, RAB] = {
      val a: RA[Int] = choose(hs.start(), hd.start(), hs.eva)
      val b: RB[Int] = choose(hs.end(), hd.end(), hs.evb)
      val c: RC[Int] = choose(hs.basesize(), hd.basesize(), hs.evc)
      new MixSortHeader[G,RA, RB, RC, RAB](hd.x, a, b, c, hs.comp, hs.inline, hs.gtyp, hs.vtyp, hs.eva, hs.evb, hs.evc, hs.evab, hs.lub1, hs.lub2, hs.lub3, hs.lub4)
    }

    //def apply[RA[_], RB[_], RC[_], RAB[_]](x: Rep[Vector[Int]], start: RA[Int], end: RB[Int], basesize: RC[Int], inline: Boolean)(implicit eva: IRep[RA], evb: IRep[RB], evc: IRep[RC], evab: IRep[RAB], lub1: Lub[RA, RB, RAB], lub2: Lub[RAB, RB, RAB], lub3: Lub[RA, RAB, RAB], lub4: Lub[RAB,RAB,RAB]): MixSortHeader[RA, RB, RC, RAB] = new MixSortHeader[RA, RB, RC, RAB](x, start, end, basesize, inline, eva, evb, evc, evab, lub1, lub2, lub3, lub4)
  }

  implicit def exposeDynHeaderPlus[G,A[_], B[_], C[_], AB[_]](stat: StatHeader[G,A, B, C, AB]): ExposeRep[DynHeaderPlus[G,A, B, C, AB]] =
    new ExposeRep[DynHeaderPlus[G,A, B, C, AB]]() {
      val superexpose = exposeDynHeader(stat)
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[Boolean]) ++ superexpose.freshExps()
      val vec2t: Vector[Exp[_]] => DynHeaderPlus[G,A, B, C, AB] = (inwi: Vector[Exp[_]]) =>  {
        def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: Option[T[A]], ev: IRep[T]): (Vector[Rep[_]], T[A]) = {
          val (vecafter, ele) = ev.fetch[A](in)
          val res: T[A] = ele.getOrElse(statele.get)
          (vecafter, res)
        }
        val cinline = inwi.head.asInstanceOf[Rep[Boolean]]
        val in = inwi.tail
        val x = in.head.asInstanceOf[Rep[Array[G]]]
        val (ostart, outstart) = help(in.tail, stat.start(), stat.eva)
        val (oend, outend) = help(ostart, stat.end(), stat.evb)
        val (obs, outbs) = help(oend, stat.basesize(), stat.evc)
        new DynHeaderPlus[G,A, B, C, AB](cinline,x, outstart, outend, outbs, stat.gtyp, stat.vtyp, stat.eva, stat.evb, stat.evc, stat.evab, stat.lub1, stat.lub2, stat.lub3, stat.lub4)
      }

      val t2vec: DynHeaderPlus[G,A, B, C, AB] => Vector[Exp[_]] = (in: DynHeaderPlus[G,A, B, C, AB]) => {
        def help[T[_], A](ele: Option[T[A]], ev: IRep[T]): Vector[Exp[_]] = {
          ele.map(p => ev.getRep(p).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
        }
        Vector(in.cinline) ++ Vector(in.x) ++ help(in.start(), in.eva) ++ help(in.end(), in.evb) ++ help(in.basesize(), in.evc)
      }
    }

  implicit def exposeDynHeader[G,A[_], B[_], C[_], AB[_]](stat: StatHeader[G,A, B, C, AB]): ExposeRep[DynHeader[G,A, B, C, AB]] =
    new ExposeRep[DynHeader[G,A, B, C, AB]]() {
      implicit val gmf = stat.gtyp.mf
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[Array[G]]) ++ stat.eva.fresh[Int]() ++ stat.evb.fresh[Int]() ++ stat.evc.fresh[Int]()
      val vec2t: Vector[Exp[_]] => DynHeader[G,A, B, C, AB] = (in: Vector[Exp[_]]) => {
        def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: Option[T[A]], ev: IRep[T]): (Vector[Rep[_]], T[A]) = {
          val (vecafter, ele) = ev.fetch[A](in)
          val res: T[A] = ele.getOrElse(statele.get)
          (vecafter, res)
        }
        val x = in.head.asInstanceOf[Rep[Array[G]]]
        val (ostart, outstart) = help(in.tail, stat.start(), stat.eva)
        val (oend, outend) = help(ostart, stat.end(), stat.evb)
        val (obs, outbs) = help(oend, stat.basesize(), stat.evc)
        new DynHeader[G,A, B, C, AB](x, outstart, outend, outbs, stat.gtyp, stat.vtyp, stat.eva, stat.evb, stat.evc, stat.evab, stat.lub1, stat.lub2, stat.lub3, stat.lub4)
      }

      val t2vec: DynHeader[G,A, B, C, AB] => Vector[Exp[_]] = (in: DynHeader[G,A, B, C, AB]) => {
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
