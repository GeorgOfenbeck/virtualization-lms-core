/*
package scala.virtualization.lms

import common._

import java.io.PrintWriter
import org.scalatest._

import scala.virtualization.lms.internal.{Traverser, Traversal, CodeMotion}


trait Trig extends Base {

  //todo removed
  //implicit def unit(x: Double): Rep[Double]

  def sin(x: Rep[Double]): Rep[Double]
  def cos(x: Rep[Double]): Rep[Double]

}

trait TrigExp extends Trig with BaseExp {

  case class Sin(x: Exp[Double]) extends Def[Double]
  case class Cos(x: Exp[Double]) extends Def[Double]

  def sin(x: Exp[Double]) = Sin(x)
  def cos(x: Exp[Double]) = Cos(x)
}

trait TrigExpOpt extends TrigExp {

  override def sin(x: Exp[Double]) = x match {
    case Const(x) => unit(math.sin(x))
    case _ => super.sin(x)
  }

  override def cos(x: Exp[Double]) = x match {
    case Const(x) => unit(math.cos(x))
    case _ => super.cos(x)
  }

}

trait FFT { this: PrimitiveOps with Trig with VectorOps with VectorwoSizeOps with PureFunctions with Reify=>
  
  def omega(k: Int, N: Int): Complex = {
    val kth = unit(-2.0 * k * math.Pi / N)
    Complex(cos(kth), sin(kth))
  }

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex( infix_+(this.re,that.re), infix_+(this.im,that.im))
    def -(that: Complex) = Complex(infix_-(this.re,that.re), infix_-(this.im,that.im))
    def *(that: Complex) = Complex(
      infix_-(infix_*(this.re,that.re), infix_*(this.im,that.im)),
      infix_+(infix_*(this.re,that.im), infix_*(this.im,that.re))
    )
  }

  def splitEvenOdd[T](xs: List[T]): (List[T], List[T]) = (xs: @unchecked) match {
    case e :: o :: xt =>
      val (es, os) = splitEvenOdd(xt)
      ((e :: es), (o :: os))
    case Nil => (Nil, Nil)
    // cases?
  }

  def mergeEvenOdd[T](even: List[T], odd: List[T]): List[T] = ((even, odd): @unchecked) match {
    case (Nil, Nil) =>
      Nil
    case ((e :: es), (o :: os)) =>
      e :: (o :: mergeEvenOdd(es, os))
    // cases?
  }

  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val o: Complex = omega(k, N)
          val z = o * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

  def fft_wrapper(n: Int): (Rep[VectorwoSize[Double]] => Rep[Vector[Double]]) = {

    val f: (Rep[VectorwoSize[Double]] => Rep[Vector[Double]]) = (in: Rep[VectorwoSize[Double]]) => {
      val clist: List[Complex] = (for (i <- 0 until n) yield {
        val idx1: Rep[Int] = unit(i * 2)
        val idx2: Rep[Int] = unit(i * 2 + 1)
        val re: Rep[Double] = in(idx1)
        val im: Rep[Double] = in(idx2)
        Complex(re, im)
      }).toList

      val res = fft(clist)
      val dlist = res.flatMap(x => List(x.re, x.im))
      val ret: Rep[Vector[Double]] = Vector(dlist)
      ret
    }
    f
  }

}






import org.scalatest.FunSpec



class TestFFT extends FunSpec{



  import common._

  describe("check reify") {
      class FooBar extends FFT with PrimitiveOpsExp with TrigExp with VectorOpsExp with VectorwoSizeOpsExp with PureFunctionsExp with Reify{
        val IR: FooBar.this.type = FooBar.this
      }

      val dsl = new FooBar

      val lamdait = dsl.fun(dsl.fft_wrapper(2))

      val reified = dsl.reifyProgramX(dsl.fft_wrapper(2))
      val cm = CodeMotion(reified)

      val traverser = Traversal(cm)

      val forward = traverser.getForwardIterator()


      def trav (t: Traverser): Unit = {
        t.scheduleoptions.map(x => println(x._1))
        println("--------")

        if(!t.scheduleoptions.isEmpty)
          trav(t.scheduleoptions.head._2())
      }

      trav(forward)
    }
}
*/
