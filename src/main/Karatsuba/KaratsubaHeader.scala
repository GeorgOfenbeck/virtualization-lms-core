package Karatsuba

import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


trait KaratsubaHeader extends sort.Skeleton{


  case class InlineInfo(inline: Boolean, maxfunctions: Int, compareinline: Boolean, consider_inline: Boolean)


  abstract class Base[A[_],B[_],AB[_],C[_],D[_],CD[_]](alength: A[Int], blength: B[Int], asignum: C[Int], bsignum: D[Int], eva: IRep[A], evb: IRep[B], evc: IRep[C], evd: IRep[D], evab: IRep[AB], evcd: IRep[CD], ab1: Lub[A, B, AB], ab2: Lub[AB, B, AB], ab3: Lub[A, AB, AB], ab4: Lub[AB, AB, AB], cd1: Lub[C, D, CD], cd2: Lub[CD, D, CD], cd3: Lub[C, CD, CD], cd4: Lub[CD, CD, CD])
  abstract class KaratsubaHeader[A[_],B[_],AB[_],C[_],D[_],CD[_]](alength: A[Int], blength: B[Int], asignum: C[Int], bsignum: D[Int], eva: IRep[A], evb: IRep[B], evc: IRep[C], evd: IRep[D], evab: IRep[AB], evcd: IRep[CD], ab1: Lub[A, B, AB], ab2: Lub[AB, B, AB], ab3: Lub[A, AB, AB], ab4: Lub[AB, AB, AB], cd1: Lub[C, D, CD], cd2: Lub[CD, D, CD], cd3: Lub[C, CD, CD], cd4: Lub[CD, CD, CD]) extends Base(alength,blength,asignum,bsignum,eva,evb,evc,evd, evab, evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4) with RepSelector {
    def alength(): Option[A[Int]] = repselect(alength, eva)

    def blength(): Option[B[Int]] = repselect(blength, evb)

    def asignum(): Option[C[Int]] = repselect(asignum, evc)

    def bsignum(): Option[D[Int]] = repselect(bsignum, evd)
  }

  class DynKaratsubaHeader[A[_],B[_],AB[_],C[_],D[_],CD[_]](val a: Rep[MyBigInt], val b: Rep[MyBigInt],alength: A[Int], blength: B[Int], asignum: C[Int], bsignum: D[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C], val evd: IRep[D], val evab: IRep[AB], val evcd: IRep[CD], ab1: Lub[A, B, AB], ab2: Lub[AB, B, AB], ab3: Lub[A, AB, AB], ab4: Lub[AB, AB, AB], cd1: Lub[C, D, CD], cd2: Lub[CD, D, CD], cd3: Lub[C, CD, CD], cd4: Lub[CD, CD, CD]) extends KaratsubaHeader(alength,blength,asignum,bsignum,eva,evb,evc,evd,evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4) with DynSelector


  object StatKaratsubaHeader{
    def apply[A[_],B[_],AB[_],C[_],D[_],CD[_]](alength: A[Int], blength: B[Int], asignum: C[Int], bsignum: D[Int], inlineinfo: InlineInfo)(implicit eva: IRep[A], evb: IRep[B], evc: IRep[C], evd: IRep[D], evab: IRep[AB], evcd: IRep[CD], ab1: Lub[A, B, AB], ab2: Lub[AB, B, AB], ab3: Lub[A, AB, AB], ab4: Lub[AB, AB, AB], cd1: Lub[C, D, CD], cd2: Lub[CD, D, CD], cd3: Lub[C, CD, CD], cd4: Lub[CD, CD, CD]): StatKaratsubaHeader[A,B,AB,C,D,CD] = new StatKaratsubaHeader[A,B,AB,C,D,CD](alength,blength,asignum,bsignum,inlineinfo,eva,evb,evc,evd, evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4)
  }

  class StatKaratsubaHeader[A[_],B[_],AB[_],C[_],D[_],CD[_]](alength: A[Int], blength: B[Int], asignum: C[Int], bsignum: D[Int], val inlineinfo: InlineInfo, val eva: IRep[A], val evb: IRep[B], val evc: IRep[C], val evd: IRep[D], val evab: IRep[AB], val evcd: IRep[CD], val ab1: Lub[A, B, AB], val ab2: Lub[AB, B, AB], val ab3: Lub[A, AB, AB], val ab4: Lub[AB, AB, AB], val cd1: Lub[C, D, CD], val cd2: Lub[CD, D, CD], val cd3: Lub[C, CD, CD], val cd4: Lub[CD, CD, CD]) extends KaratsubaHeader(alength,blength,asignum,bsignum,eva,evb,evc,evd,evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4) with StatSelector {
    def genSig(): String = {
      val al = alength() match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }
      val bl = blength() match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }
      val as = asignum() match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }
      val bs = bsignum() match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }


      "alength_" ++ al ++ "blength_" ++ bl ++ "asignum" ++ as ++ "bsignum" ++ bs ++ inlineinfo.toString
    }
  }

  case class MixKaratsubaHeader[A[_],B[_],AB[_],C[_],D[_],CD[_]](val a: Rep[MyBigInt], val b: Rep[MyBigInt], val alength: A[Int], val blength: B[Int], val asignum: C[Int], val bsignum: D[Int], val inlineInfo: InlineInfo, val eva: IRep[A], val evb: IRep[B], val evc: IRep[C], val evd: IRep[D], val evab: IRep[AB], val evcd: IRep[CD], val ab1: Lub[A, B, AB], val ab2: Lub[AB, B, AB], val ab3: Lub[A, AB, AB], val ab4: Lub[AB, AB, AB], val cd1: Lub[C, D, CD], val cd2: Lub[CD, D, CD], val cd3: Lub[C, CD, CD], val cd4: Lub[CD, CD, CD]) extends Base(alength,blength,asignum,bsignum,eva,evb,evc,evd,evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4)
  {
    def getDynHeader(): DynKaratsubaHeader[A,B,AB,C,D,CD] = new DynKaratsubaHeader[A,B,AB,C,D,CD](a,b,alength,blength,asignum,bsignum,eva,evb,evc,evd,evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4)
    def getStatHeader(): StatKaratsubaHeader[A,B,AB,C,D,CD] = new StatKaratsubaHeader[A,B,AB,C,D,CD](alength,blength,asignum,bsignum, inlineInfo,eva,evb,evc,evd, evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4)

    def split(): (StatKaratsubaHeader[A,B,AB,C,D,CD], DynKaratsubaHeader[A,B,AB,C,D,CD]) = (getStatHeader(), getDynHeader())
  }


  object helpme{
    def apply[A[_],B[_],AB[_],C[_],D[_],CD[_]](a: Rep[MyBigInt], b: Rep[MyBigInt],alength: A[Int], blength: B[Int], asignum: C[Int], bsignum: D[Int], inlineinfo: InlineInfo)(implicit eva: IRep[A], evb: IRep[B], evc: IRep[C], evd: IRep[D], evab: IRep[AB], evcd: IRep[CD], ab1: Lub[A, B, AB], ab2: Lub[AB, B, AB], ab3: Lub[A, AB, AB], ab4: Lub[AB, AB, AB], cd1: Lub[C, D, CD], cd2: Lub[CD, D, CD], cd3: Lub[C, CD, CD], cd4: Lub[CD, CD, CD]): MixKaratsubaHeader[A,B,AB,C,D,CD] = MixKaratsubaHeader[A,B,AB,C,D,CD](a,b,alength,blength,asignum,bsignum, inlineinfo,eva,evb,evc,evd, evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4)
  }

  object MixKaratsubaHeader {
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    def apply[A[_],B[_],AB[_],C[_],D[_],CD[_]](hs: StatKaratsubaHeader[A,B,AB,C,D,CD], hd: DynKaratsubaHeader[A,B,AB,C,D,CD]): MixKaratsubaHeader[A,B,AB,C,D,CD] = {
      import hs._
      val al: A[Int] = choose(hs.alength(), hd.alength(), hs.eva)
      val bl: B[Int] = choose(hs.blength(), hd.blength(), hs.evb)
      val as: C[Int] = choose(hs.asignum(), hd.asignum(), hs.evc)
      val bs: D[Int] = choose(hs.bsignum(), hd.bsignum(), hs.evd)
      new MixKaratsubaHeader[A,B,AB,C,D,CD](hd.a, hd.b, al, bl, as, bs , hs.inlineinfo, eva,evb,evc,evd,evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4)
    }

  }

  implicit def exposeDynHeader[A[_],B[_],AB[_],C[_],D[_],CD[_]](stat: StatKaratsubaHeader[A,B,AB,C,D,CD]): ExposeRep[DynKaratsubaHeader[A,B,AB,C,D,CD]] =
    new ExposeRep[DynKaratsubaHeader[A,B,AB,C,D,CD]]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[MyBigInt]) ++ Vector(Arg[MyBigInt]) ++ stat.eva.fresh[Int]() ++ stat.evb.fresh[Int]() ++ stat.evc.fresh[Int]() ++stat.evd.fresh[Int]()
      val vec2t: Vector[Exp[_]] => DynKaratsubaHeader[A,B,AB,C,D,CD] = (in: Vector[Exp[_]]) => {
        def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: Option[T[A]], ev: IRep[T]): (Vector[Rep[_]], T[A]) = {
          val (vecafter, ele) = ev.fetch[A](in)
          val res: T[A] = ele.getOrElse(statele.get)
          (vecafter, res)
        }
        val a = in.head.asInstanceOf[Rep[MyBigInt]]
        val b = in.tail.head.asInstanceOf[Rep[MyBigInt]]

        val (o_alength, oal ) = help(in.tail.tail, stat.alength, stat.eva)
        val (o_blength, obl) = help(o_alength, stat.blength, stat.evb)
        val (o_asig, oasig) = help(o_blength, stat.asignum, stat.evc)
        val (o_bsig, obsig) = help(o_asig, stat.bsignum, stat.evd)
        import stat._
        new DynKaratsubaHeader[A,B,AB,C,D,CD](a,b, oal, obl, oasig, obsig, eva,evb,evc,evd,evab,evcd, ab1, ab2, ab3, ab4, cd1, cd2, cd3, cd4)
      }

      val t2vec: DynKaratsubaHeader[A,B,AB,C,D,CD] => Vector[Exp[_]] = (in:  DynKaratsubaHeader[A,B,AB,C,D,CD] ) => {
        def help[T[_], A](ele: Option[T[A]], ev: IRep[T]): Vector[Exp[_]] = {
          ele.map(p => ev.getRep(p).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
        }
        Vector(in.a) ++ Vector(in.b) ++ help(in.alength(), in.eva) ++ help(in.blength(), in.evb) ++ help(in.asignum(), in.evc) ++ help(in.bsignum(), in.evd)
      }

    }




}
