package sort

/**
  * Georg Ofenbeck
  * First created:
  * Date: 23/09/2016
  * Time: 13:50 
  */
object MixTypeClass extends App{

  type NoRep[T] = T
  class Rep[T](x : T){
    def + (that: Rep[T]) = that
  }

  def const[T](x: T): Rep[T] = new Rep(x)
  abstract class Helpme[T]
  case class Yes[T](x: Rep[T]) extends Helpme[T]
  case class No[T](x: NoRep[T]) extends Helpme[T]

  trait Num[T[_],I]{
    //def plus(x: T[I], y: T[I]): Helpme[I]
    def plus[V[_]](x: T[I], y: V[I])(implicit evy: Num[V,I]): Helpme[I]
  }



  implicit object NoRepNum extends Num[NoRep,Int]{
    //def plus(x: Int, y: Int) = x + y
    def plus[V[_]](x: NoRep[Int], y: V[Int])(implicit evy: Num[V,Int]): Helpme[Int] = evy.plus(y,x)
    def plus(x: NoRep[Int], y: NoRep[Int]):Helpme[Int] = No(x + y)
    def plus(x: NoRep[Int], y: Rep[Int]):Helpme[Int] = Yes( const(x) + y)
  }

  implicit object RepNum extends Num[Rep,Int]{
    //def plus(x: Rep[Int], y: Rep[Int]) = ???
    def plus[V[_]](x: Rep[Int], y: V[Int])(implicit evy: Num[V,Int]): Helpme[Int] = evy.plus(y,x)
    def plus(x: Rep[Int], y: NoRep[Int]):Helpme[Int] = Yes( x + const(y))
    def plus(x: Rep[Int], y: Rep[Int]):Helpme[Int] = Yes(x + y)
  }

  def bla[A[_],B[_]](x: A[Int], y: B[Int])(implicit eva: Num[A,Int], evb: Num[B,Int]): A[Int] = {
    //val t = eva.plus(x,y)
    bla(x,y)
  }


}
