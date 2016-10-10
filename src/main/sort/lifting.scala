object Example extends App {

  trait ExtraImplicits {
    /** These implicits create conversions from a value for which an implicit Numeric
      *  exists to the inner class which creates infix operations.  Once imported, you
      *  can write methods as follows:
      *  {{{
      *  def plus[T: Numeric](x: T, y: T) = x + y
      *  }}}
      */
    implicit def infixOps[T[_]](x: T[Int])(implicit num: MayRep[T]): MayRep[T]#Ops = new num.Ops(x)
  }
  object Implicits extends ExtraImplicits { }

  type NoRep[T] = T
  case class Rep[T](x: T) 

  def NoRep[T](x:T): NoRep[T] = x

  // encode least upper bound relation as implicit
  trait Lub[A[_],B[_],C[_]] {
    implicit def fromA[T](x:A[T]):C[T]
    implicit def fromB[T](x:B[T]):C[T]
  }

  implicit def NoRepNoRep: Lub[NoRep,NoRep,NoRep] = new Lub[NoRep,NoRep,NoRep] { def fromA[T](x:T) = x; def fromB[T](x:T) = x }
  implicit def RepNoRep:   Lub[Rep,  NoRep,  Rep] = new Lub[Rep,  NoRep,  Rep] { def fromA[T](x:Rep[T]) = x; def fromB[T](x:T) = Rep(x) }
  implicit def NoRepRep:   Lub[NoRep,  Rep,  Rep] = new Lub[NoRep,  Rep,  Rep] { def fromA[T](x:T) = Rep(x); def fromB[T](x:Rep[T]) = x }
  implicit def RepRep:     Lub[  Rep,  Rep,  Rep] = new Lub[  Rep,  Rep,  Rep] { def fromA[T](x:Rep[T]) = x; def fromB[T](x:Rep[T]) = x }

  //def getLub[X[_],Y[_],Z[_]](x: X[_], y: Y[_])(implicit lub: Lub[X,Y,Z]) = lub

  /*class hm[A[_], B[_], C[_]](a:A[Int], b: B[Int], c: C[Int]){
    val ab = getLub(a,b)
    val bc = getLub(b,c)
    val ac = getLub(c,a)


  }*/


  trait MayRep[T[_]]
  {
    class Ops(lhs: T[Int]) {
      def op(rhs: T[Int]): T[Int] = op_same(lhs,rhs)
    }
    implicit def mkMayOps(lhs: T[Int]): Ops = new Ops(lhs)

    def op_same(me: T[Int], other: T[Int]): T[Int]
    def toRep(me: T[Int]): Rep[Int]
  }
  
  implicit object isRep extends MayRep[Rep]{
    def op_same(me: Rep[Int], other: Rep[Int]): Rep[Int] = me
    def toRep(me: Rep[Int]): Rep[Int] = me
  }
  implicit object isNoRep extends MayRep[NoRep] {
    def op_same(me: NoRep[Int], other: NoRep[Int]): NoRep[Int] = me
    def toRep(me: NoRep[Int]): Rep[Int] = Rep(me)
  }

  case class Header[A[_],B[_],AB[_], C[Int]](a: A[Int], b: B[Int], c: C[Int],
                                               lub1: Lub[A,B,AB], lub2: Lub[AB,B,AB],
                                               lub3: Lub[A,AB,AB], aev: MayRep[A], bev: MayRep[B], cev: MayRep[C], abev: MayRep[AB])


  def foo[A[_],B[_],AB[_], C[Int]](a: A[Int], b: B[Int], c: C[Int])
                                  (implicit lub1: Lub[A,B,AB], lub2: Lub[AB,B,AB],lub3: Lub[A,AB,AB],
                                   aev: MayRep[A], bev: MayRep[B], cev: MayRep[C], abev: MayRep[AB]): Rep[Int] = {
    val h = Header(a,b,c,lub1,lub2,lub3,aev,bev,cev, abev)
    foo_internal(h)
  }

  def foo_internal[A[_],B[_],AB[_], C[Int]](h: Header[A,B,AB,C]):Rep[Int] = {
    import h._
    import lub1._
    val c = abev.op_same(a,b)
    val d = aev.op_same(a,a)

    abev.toRep(c)
  }
  var count = 1
  val x1 = foo(NoRep(1),NoRep(2), NoRep(3))
  val x2 = foo(NoRep(1),Rep(2), NoRep(3))
  val x3 = foo(Rep(1),NoRep(2),NoRep(3))
  val x4 = foo(Rep(1),Rep(2),NoRep(3))
  val x5 = foo(NoRep(1),NoRep(2), NoRep(4))
  val x6 = foo(NoRep(1),Rep(2), NoRep(4))
  val x7 = foo(Rep(1),NoRep(2),NoRep(4))
  val x8 = foo(Rep(1),Rep(2),NoRep(4))


  println(x1,x2,x3,x4, x5, x6, x7 ,x8)


}




