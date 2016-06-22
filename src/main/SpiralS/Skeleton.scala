package SpiralS

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


trait Skeleton extends Spiral_DSL {

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

  case class DynGTSkeleton(x: Single, y: Single, n: Option[Rep[Int]], loopbound: Option[Rep[Int]], g: DynIMH, s: DynIMH, v: Rep[Vector[Int]])

  case class StatGTSkeleton(n: Option[Int], loopbound: Option[Int], g: StatIMH, s: StatIMH, parInfo: Option[ParInfo])

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
      GTSkeletonFull(d.x, d.y, na, nl, IMH(s.g, d.g), IMH(s.s, d.s), d.v, s.parInfo)
    }
  }

  case class GTSkeletonFull(x: Single, y: Single, n: SInt, loopbound: SInt, g: IMH, s: IMH, v: Rep[Vector[Int]], parInfo: Option[ParInfo]) {


    def getDynSkel() = {
      val on: Option[Rep[Int]] = n.i.fold(fa => Some(fa), fb => None)
      val ol: Option[Rep[Int]] = loopbound.i.fold(fa => Some(fa), fb => None)
      DynGTSkeleton(x, y, on, ol, g.getDynIMH(), s.getDynIMH(), v)
    }

    def getStatSkel() = {
      val on: Option[Int] = n.i.fold(fa => None, fb => Some(fb))
      val ol: Option[Int] = loopbound.i.fold(fa => None, fb => Some(fb))
      StatGTSkeleton(on, ol, g.getStatIMH(), s.getStatIMH(), parInfo)
    }
  }

  implicit def exposeDynGTSkeleton(stat: StatGTSkeleton): ExposeRep[DynGTSkeleton] = {
    new ExposeRep[DynGTSkeleton]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
        val fn = if (stat.n.isEmpty) Vector(Arg[Int]) else Vector.empty
        val fl = if (stat.loopbound.isEmpty) Vector(Arg[Int]) else Vector.empty

        exposeSingle.freshExps() ++ exposeSingle.freshExps() ++
          fn ++ fl ++ stat.g.freshExps() ++ stat.s.freshExps() ++ Vector(Arg[Vector[Int]])
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
        val (g, outg) = stat.g.vec2t(outl)
        val (s, outs) = stat.s.vec2t(outg)
        val v = outs.head.asInstanceOf[Exp[Vector[Int]]]
        DynGTSkeleton(x, y, n, l, g, s, v)
      }
      val t2vec: DynGTSkeleton => Vector[Exp[_]] = (in: DynGTSkeleton) => {
        val vn = in.n.map(p => Vector(p)).getOrElse(Vector.empty)
        val vl = in.loopbound.map(p => Vector(p)).getOrElse(Vector.empty)
        Vector(in.x.y, in.y.y) ++ vn ++ vl ++ in.g.t2vec() ++ in.s.t2vec() ++ Vector(in.v)
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
