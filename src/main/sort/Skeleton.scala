package sort


import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


trait Skeleton extends Sort_DSL {


  object SInt {
    def apply(nos: Int): SInt = SInt(Right(nos))

    def apply(nos: Rep[Int]): SInt = SInt(Left(nos))
  }

  implicit def sint2rep(x: SInt): Rep[Int] = x.toRep()
  implicit def int2sint(x: Int): SInt = SInt(x)

  implicit def exposeSInt(sint: SInt): ExposeRep[SInt] = new ExposeRep[SInt]() {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) =>  sint.i.fold(fa => Vector(Arg[Int]), fb => Vector.empty)
    val vec2t: Vector[Exp[_]] => SInt = (in: Vector[Exp[_]]) => sint.i.fold(fa => SInt(in.head.asInstanceOf[Rep[Int]]), fb=> sint)
    val t2vec: SInt => Vector[Exp[_]] = (in: SInt) =>  in.i.fold(fa => Vector(fa), fb => Vector.empty)
  }

  //implicit def repint2sint(x: Rep[Int]) = SInt(x)

  //def myifThenElse[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A =
  def _if[A](cond: SBool, thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A =
    cond.i.fold(fa => myifThenElse(fa,thenp,elsep) , fb => if(fb) thenp else elsep)


  def choose_base(size: Int) = 0
  def choose_base(size: SInt): SInt = size.i.fold(fa => SInt(choose_base(fa)), fb => choose_base(fb))

  def choose_sort(size: Int) = 0
  def choose_sort(size: SInt): SInt = size.i.fold(fa => SInt(choose_sort(fa)), fb => choose_sort(fb))


  case class SBool(i: Either[Rep[Boolean], Boolean])

  case class SInt(i: Either[Rep[Int], Int]) {

    def ==(that: SInt): SBool = {
      val t: Either[Rep[Boolean], Boolean] = i.fold(fa => {
        val r: Rep[Boolean] = that.i.fold(ifa => fa equiv ifa, ifb => fa equiv unit(ifb))
        Left(r)
      }, fb => {
        val r: Either[Rep[Boolean], Boolean] = that.i.fold(ifa => Left(unit(fb) equiv ifa), ifb => Right(fb == ifb))
        r
      })
      SBool(t)
    }

    def <(that: SInt): SBool = {
      val t: Either[Rep[Boolean], Boolean] = i.fold(fa => {
        val r: Rep[Boolean] = that.i.fold(ifa => fa < ifa, ifb => fa < unit(ifb))
        Left(r)
      }, fb => {
        val r: Either[Rep[Boolean], Boolean] = that.i.fold(ifa => Left(unit(fb) < ifa), ifb => Right(fb < ifb))
        r
      })
      SBool(t)
    }

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

    def stage_range(a: Rep[Int], b: Rep[Int]): Rep[Range] = range_create(a,b)

    def until(that: SInt): SRange = {
      val t: SRange = i.fold(fa => {
        val r: Rep[Range] = that.i.fold(ifa => stage_range(fa,ifa), ifb => stage_range(fa,unit(ifb)))
        SRange(Left(r))
      }, fb => {
        val r: SRange = that.i.fold(ifa => SRange(Left(stage_range(unit(fb),ifa))), ifb => SRange(Right(fb until ifb)))
        r
      })
      t
    }
    def toRep(): Rep[Int] = i.fold(fa => fa, fb => unit(fb))
  }



  case class SRange(r: Either[Rep[Range],Range]){

    def foldLeft[B](ini: B)(body: ((B, SInt)) => B)(implicit exposeRep: ExposeRep[B]): B = {
     r.fold(
       fa => {
         val f: ((B,Rep[Int])) => B = (tup: ((B,Rep[Int]))) => body((tup._1,SInt(tup._2)))
         range_foldLeft(fa,ini,f)
       },
       fb => {
        val f: (B,Int) => B = (b: B,i : Int) => body((b,SInt(i)))
        fb.foldLeft(ini)(f)
      }
     )
    }
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


}

