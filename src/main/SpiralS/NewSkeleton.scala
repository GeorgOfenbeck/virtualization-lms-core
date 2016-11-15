
package SpiralS

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


trait NewSkeleton extends Spiral_DSL {


  object SInt {
    def apply(nos: Int): SInt = SInt(Right(nos))

    def apply(nos: Rep[Int]): SInt = SInt(Left(nos))
  }

  case class SInt(i: Either[Rep[Int], Int]) {
    def +(that: SInt): SInt = {
      val t: Either[Rep[Int], Int] = i.fold(fa => {
        val r: Rep[Int] = that.i.fold(ifa => fa + ifa, ifb => fa + unit(ifb))
        Left(r)
      }, fb => {
        val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) + ifa), ifb => Right(fb + ifb))
        r
      })
      SInt(t)
    }

    def -(that: SInt): SInt = {
      val t: Either[Rep[Int], Int] = i.fold(fa => {
        val r: Rep[Int] = that.i.fold(ifa => fa - ifa, ifb => fa - unit(ifb))
        Left(r)
      }, fb => {
        val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) - ifa), ifb => Right(fb - ifb))
        r
      })
      SInt(t)
    }

    def *(that: SInt) = {
      val t: Either[Rep[Int], Int] = i.fold(fa => {
        val r: Rep[Int] = that.i.fold(ifa => fa * ifa, ifb => fa * unit(ifb))
        Left(r)
      }, fb => {
        val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) * ifa), ifb => Right(fb * ifb))
        r
      })
      SInt(t)
    }

    def /(that: SInt) = {
      val t: Either[Rep[Int], Int] = i.fold(fa => {
        val r: Rep[Int] = that.i.fold(ifa => fa / ifa, ifb => fa / unit(ifb))
        Left(r)
      }, fb => {
        val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) / ifa), ifb => Right(fb / ifb))
        r
      })
      SInt(t)
    }

    def %(that: SInt) = {
      val t: Either[Rep[Int], Int] = i.fold(fa => {
        val r: Rep[Int] = that.i.fold(ifa => fa % ifa, ifb => fa % unit(ifb))
        Left(r)
      }, fb => {
        val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) % ifa), ifb => Right(fb % ifb))
        r
      })
      SInt(t)
    }


    def toRep(): Rep[Int] = i.fold(fa => fa, fb => unit(fb))
  }

  case class Complex(val _im: Rep[Double], val _re: Rep[Double]) {
    def plus(x: Complex, y: Complex) = Complex(x._re + y._re, x._im + y._im)

    def minus(x: Complex, y: Complex) = Complex(x._re - y._re, x._im - y._im)

    def times(x: Complex, y: Complex) = {
      val m1 = x._re * y._re
      val m2 = x._im * y._im
      val m3 = x._re * y._im
      val m4 = x._im * y._re
      Complex(m1 - m2, m3 + m4)
    }
  }


  case class TwiddleScaling(n: SInt, d: SInt, k: SInt) {
    def getDynTwiddleScaling(): DynTwiddleScaling = {
      val on: Option[Rep[Int]] = n.i.fold(fa => Some(fa), fb => None)
      val ok: Option[Rep[Int]] = k.i.fold(fa => Some(fa), fb => None)
      val od: Option[Rep[Int]] = d.i.fold(fa => Some(fa), fb => None)
      DynTwiddleScaling(on, ok, od)
    }

    def getStatTwiddleScaling(): StatTwiddleScaling = {
      val on: Option[Int] = n.i.fold(fa => None, fb => Some(fb))
      val ok: Option[Int] = k.i.fold(fa => None, fb => Some(fb))
      val od: Option[Int] = d.i.fold(fa => None, fb => Some(fb))
      StatTwiddleScaling(on, ok, od)
    }
  }

  case class StatTwiddleScaling(n: Option[Int], d: Option[Int], k: Option[Int]){
    def freshExps(): Vector[Exp[_]] = Vector(n,d,k).foldLeft(Vector.empty[Exp[_]]){ (acc,ele) => if (ele.isEmpty) acc :+ Arg[Int] else acc}

    def vec2t(v: Vector[Exp[_]]): (DynTwiddleScaling, Vector[Exp[_]]) = {
      val (on, an) = if (n.isEmpty) (Some(v.head.asInstanceOf[Exp[Int]]), v.tail) else (None,v)
      val (od, ad) = if (d.isEmpty) (Some(an.head.asInstanceOf[Exp[Int]]), an.tail) else (None,an)
      val (ok, ak) = if (k.isEmpty) (Some(ad.head.asInstanceOf[Exp[Int]]), ad.tail) else (None,ad)
      (DynTwiddleScaling(on,od,ok),ak)
    }
  }

  case class DynTwiddleScaling(n: Option[Rep[Int]], d: Option[Rep[Int]], k: Option[Rep[Int]]){

    def t2vec(): Vector[Exp[_]] = Vector(n,d,k).flatten
  }


  object TwiddleScaling {
    def apply(stat: StatTwiddleScaling, dyn: DynTwiddleScaling): TwiddleScaling = {

      val n: SInt = (stat.n, dyn.n) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      val d: SInt = (stat.d, dyn.d) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      val k: SInt = (stat.k, dyn.k) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      TwiddleScaling(n, d, k)
    }
  }


  object IMH {
    def apply(s: StatIMH, d: DynIMH): IMH = {
      val nb = (s.base, d.base) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      IMH(nb, d.strides)
    }
  }

  //case class IMH(base: SInt, strides: Vector[SInt])
  case class IMH(base: SInt, strides: Rep[Vector[Int]]) {

    def getDynIMH() = base.i.fold(fa => DynIMH(Some(fa), strides), fb => DynIMH(None, strides))

    def getStatIMH() = base.i.fold(fa => StatIMH(None), fb => StatIMH(Some(fb)))
  }

  case class DynIMH(base: Option[Rep[Int]], strides: Rep[Vector[Int]]) {
    def t2vec(): Vector[Exp[_]] = if (base.isEmpty) Vector(strides) else Vector(base.get, strides)
  }

  case class StatIMH(base: Option[Int]) {
    def freshExps(): Vector[Exp[_]] = if (base.isEmpty) Vector(Arg[Int]) ++ Vector(Arg[Vector[Int]]) else Vector(Arg[Vector[Int]])

    def vec2t(v: Vector[Exp[_]]): (DynIMH, Vector[Exp[_]]) = {
      if (base.isEmpty) {
        val b = v.head.asInstanceOf[Exp[Int]]
        val s = v.tail.head.asInstanceOf[Exp[Vector[Int]]]
        (DynIMH(Some(b), s), v.tail.tail)
      } else {
        val s = v.head.asInstanceOf[Exp[Vector[Int]]]
        (DynIMH(None, s), v.tail)
      }
    }
  }


  //case class IMH(base: SInt, strides: Vector[SInt])

  //case class GTSkeletonFull(x: Single, y: Single, n: SInt, g: IMH, s: IMH, v: Vector[Rep[Int]])

  case class ParInfo(p: Int, cls: Int)

  case class VecInfo(u: Int, applied: Boolean)


  case class DynGTSkeleton(x: Single, y: Single, n: Option[Rep[Int]], loopbound: Option[Rep[Int]], im: DynIM, v: Rep[Vector[Int]], dynTwiddleScaling: Option[DynTwiddleScaling])

  case class InlineInfo(inline: Boolean, maxfunctions: Int, compareinline: Boolean, consider_inline: Boolean, specialize: Boolean, spezialize_done: Int)

  case class StatGTSkeleton(n: Option[Int], loopbound: Option[Int], im: StatIM, parInfo: Option[ParInfo], statTwiddleScaling: Option[StatTwiddleScaling], vecinfo_stat: Option[VecInfo] , val inline: InlineInfo){
    def genSig(): String = {
      val ix = n.fold("")(_.toString)
      val iy = loopbound.fold("")(_.toString)
      val as = im match{
        case Stat_GT_IM(g,s) => s"g${g.base.fold("")(_.toString)}s${s.base.fold("")(_.toString)}"
        case Stat_GTI_IM(g,s) => s"g${g.base.fold("")(_.toString)}s${s.base.fold("")(_.toString)}"
        case _ => ???
      }
      val bs = ""
      val cs = statTwiddleScaling.fold("")(p => {
        "tp" + p.d.fold("")(_.toString) +
        "tk" + p.k.fold("")(_.toString) +
        "tn" + p.n.fold("")(_.toString)
      })
      val ds = ""
      val es = s"inline${inline.inline}"
      s"_ix${ix}iy${iy}a${as}b${bs}c${cs}d${ds}e${es}f"
    }
  }

  object GTSkeletonFull {
    def apply(s: StatGTSkeleton, d: DynGTSkeleton): GTSkeletonFull = {
      val na = (s.n, d.n) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      val nl = (s.loopbound, d.loopbound) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      val tw: Option[TwiddleScaling] = (s.statTwiddleScaling,d.dynTwiddleScaling) match {
        case (Some(x),Some(y)) => Some(TwiddleScaling(x,y))
        case (None,None) => None
        case _ => ???
      }
      val vecinfo = s.vecinfo_stat

      GTSkeletonFull(d.x, d.y, na, nl, IM(s.im,d.im), d.v, s.parInfo, tw, vecinfo, s.inline)

    }
  }


  object IM{
    def apply(s: StatIM, d: DynIM): IM = {
      (s,d) match {
        case (gts: Stat_GT_IM, gtd: Dyn_GT_IM) => GT_IM(IMH(gts.g, gtd.g), IMH(gts.s, gtd.s))
        case (gts: Stat_GTI_IM, gtd: Dyn_GTI_IM) => GTI_IM(IMH(gts.im, gtd.im), IMH(gts.twim, gtd.twim))
        case _ => ???
      }
    }
  }

  abstract class IM {
    def getDynIM(): DynIM
    def getStatIM(): StatIM
    def gather(): IMH
    def scatter(): IMH
  }
  case class GT_IM (g: IMH, s: IMH) extends IM
  {
    def getDynIM(): Dyn_GT_IM = new Dyn_GT_IM(g.getDynIMH(),s.getDynIMH())
    def getStatIM(): Stat_GT_IM = new Stat_GT_IM(g.getStatIMH(),s.getStatIMH())
    def gather() = g
    def scatter() = s
  }
  case class GTI_IM(im: IMH, twim: IMH) extends IM
  {
    def getDynIM(): Dyn_GTI_IM = new Dyn_GTI_IM(im.getDynIMH(), twim.getDynIMH())
    def getStatIM(): Stat_GTI_IM = new Stat_GTI_IM(im.getStatIMH(), twim.getStatIMH())
    def gather() = im
    def scatter() = im
  }

  implicit def exposeIM(im: IM): ExposeRep[IM] = {
    new ExposeRep[IM]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) =>  im.getStatIM().freshExps()
      val vec2t: Vector[Exp[_]] => IM = (in: Vector[Exp[_]]) => {
        val dyn = im.getStatIM().vec2t(in)._1
        IM(im.getStatIM(),dyn)
      }
      val t2vec: IM => Vector[Exp[_]] = (in: IM) =>  in.getDynIM().t2vec()
    }
  }

  abstract class DynIM
  {
    def t2vec(): Vector[Exp[_]]
  }
  case class Dyn_GT_IM(g: DynIMH, s: DynIMH) extends DynIM
  {
    def t2vec(): Vector[Exp[_]] = g.t2vec() ++ s.t2vec()
  }
  case class Dyn_GTI_IM(im: DynIMH, twim: DynIMH) extends DynIM
  {
    def t2vec(): Vector[Exp[_]] = im.t2vec() ++ twim.t2vec()
  }
  abstract class StatIM{
    def freshExps(): Vector[Exp[_]]
    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]])
  }
  case class Stat_GT_IM(g: StatIMH, s: StatIMH) extends StatIM
  {
    def freshExps(): Vector[Exp[_]] = g.freshExps() ++ s.freshExps()
    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a,b) = g.vec2t(v)
      val (c,d) = s.vec2t(b)
      (new Dyn_GT_IM(a,c),d)
    }
  }
  case class Stat_GTI_IM( im: StatIMH, twim: StatIMH) extends StatIM
  {
    def freshExps(): Vector[Exp[_]] = im.freshExps() ++ twim.freshExps()
    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a,b) = im.vec2t(v)
      val (c,d) = twim.vec2t(b)
      (new Dyn_GTI_IM(a,c),d)
    }
  }


  //, twiddle: Option[Twi]
  case class GTSkeletonFull(x: Single, y: Single, n: SInt, loopbound: SInt, im: IM, v: Rep[Vector[Int]], parInfo: Option[ParInfo], twiddleScaling: Option[TwiddleScaling], vecinfo: Option[VecInfo], val inline: InlineInfo) {
    def getDynSkel() = {
      val on: Option[Rep[Int]] = n.i.fold(fa => Some(fa), fb => None)
      val ol: Option[Rep[Int]] = loopbound.i.fold(fa => Some(fa), fb => None)
      val to: Option[DynTwiddleScaling] = twiddleScaling.map( e => e.getDynTwiddleScaling())
      //DynGTSkeleton(x, y, on, ol, g.getDynIMH(), s.getDynIMH(), v, to)
      val dynim = im.getDynIM()
      DynGTSkeleton(x, y, on, ol, dynim, v, to)
    }

    def getStatSkel() = {
      val on: Option[Int] = n.i.fold(fa => None, fb => Some(fb))
      val ol: Option[Int] = loopbound.i.fold(fa => None, fb => Some(fb))
      val to: Option[StatTwiddleScaling] = twiddleScaling.map( e => e.getStatTwiddleScaling())
      val statim = im.getStatIM()
      StatGTSkeleton(on, ol, statim, parInfo, to, vecinfo, inline)
    }

    def split() = (getStatSkel(), getDynSkel())
  }

  implicit def exposeDynGTSkeleton(stat: StatGTSkeleton): ExposeRep[DynGTSkeleton] = {
    new ExposeRep[DynGTSkeleton]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
        val fn = if (stat.n.isEmpty) Vector(Arg[Int]) else Vector.empty
        val fl = if (stat.loopbound.isEmpty) Vector(Arg[Int]) else Vector.empty

        val tw: Vector[Exp[_]] = stat.statTwiddleScaling match {
          case Some(x) => x.freshExps()
          case None => Vector.empty
        }

        exposeSingle.freshExps() ++ exposeSingle.freshExps() ++
          fn ++ fl ++ stat.im.freshExps() ++ Vector(Arg[Vector[Int]]) ++ tw
        //stat.g.freshExps() ++ stat.s.freshExps()
      }
      val vec2t: Vector[Exp[_]] => DynGTSkeleton = (in: Vector[Exp[_]]) => {
        assert(in.size >= 2)
        val x = exposeSingle.vec2t(in)
        val singlesize = exposeSingle.t2vec(x).size
        val outx = in.drop(singlesize)
        val y = exposeSingle.vec2t(outx)
        val outy = outx.drop(singlesize)
        val (n, outn) = if (stat.n.isEmpty) (Some(outy.head.asInstanceOf[Rep[Int]]), outy.tail) else (None, outy)
        val (l, outl) = if (stat.loopbound.isEmpty) (Some(outn.head.asInstanceOf[Rep[Int]]), outn.tail) else (None, outn)
        //val (g, outg) = stat.g.vec2t(outl)
        val (im, outs) = stat.im.vec2t(outl)
        val v = outs.head.asInstanceOf[Exp[Vector[Int]]]
        val (tw, outtw): (Option[DynTwiddleScaling],Vector[Exp[_]]) = stat.statTwiddleScaling match {
          case Some(stattw) => {
            val t = stattw.vec2t(outs.tail)
            (Some(t._1), t._2)
          }
          case None => (None,outs.tail)
        }
        DynGTSkeleton(x, y, n, l, im, v, tw)
      }
      val t2vec: DynGTSkeleton => Vector[Exp[_]] = (in: DynGTSkeleton) => {
        val vn = in.n.map(p => Vector(p)).getOrElse(Vector.empty)
        val vl = in.loopbound.map(p => Vector(p)).getOrElse(Vector.empty)
        val tw = in.dynTwiddleScaling match {
          case Some(dyn) => dyn.t2vec()
          case None => Vector.empty
        }

        Vector(in.x.y, in.y.y) ++ vn ++ vl ++ in.im.t2vec() ++ Vector(in.v) ++ tw
        //in.g.t2vec() ++ in.s.t2vec() ++ Vector(in.v) ++ tw
      }
    }
  }

  implicit val exposeTIntSingle = new ExposeRep[ISingle]() {
    val freshExps = (u: Unit) => Vector(Arg[ComplexVector], Arg[Int])
    val vec2t: Vector[Exp[_]] => ISingle = (in: Vector[Exp[_]]) => {
      val t = in(1).asInstanceOf[Rep[Int]]
      val s = Single(in(0).asInstanceOf[Rep[ComplexVector]]) //exposeSingle.vec2t(in.tail)
      ISingle(s, t)
    }
    val t2vec: ISingle => Vector[Exp[_]] = (in: ISingle) => {
      val t: Vector[Exp[_]] = Vector(in.i)
      val s = Vector(in.s.y)
      t ++ s
    }
  }


  implicit val exposeSingle = new ExposeRep[Single]() {
    val freshExps = (u: Unit) => Vector(Arg[ComplexVector])
    val vec2t: Vector[Exp[_]] => Single = (in: Vector[Exp[_]]) => Single(in.head.asInstanceOf[Rep[ComplexVector]])
    val t2vec: Single => Vector[Exp[_]] = (in: Single) => Vector(in.y)
  }


}