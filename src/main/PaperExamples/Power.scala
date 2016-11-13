/*
package PaperExamples

import org.scala_lang.virtualized.SourceContext

/**
  * Created by rayda on 10-Nov-16.
  */
trait Power extends sort.Sort_DSL {

  implicit def toRangeOps[R[_]](x: R[Range])(implicit ev: IRep[R]): IRep[R]#RangeOps = new ev.RangeOps(x)

  implicit def toIntOps[R[_]](x: R[Int])(implicit ev: IRep[R]): IRep[R]#Ops = new ev.Ops(x)


  trait IRep[T[_]] {
    def plus(lhs: T[Int], rhs: T[Int]): T[Int]

    def isRep(x: T[Int]): Boolean = ???
    def _if[A](cond: T[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = ???

    def const(x: Int): T[Int]

    implicit def toIRep(x: Int): T[Int] = const(x)

    implicit def toRep(x: T[Int]): Rep[Int] = ???

    class Ops(lhs: T[Int]) {

      def isRep(): Boolean = ???
      def eq(rhs: T[Int]): T[Boolean] = ???

      def +(rhs: T[Int]): T[Int] = ???

      def until(rhs: T[Int]): T[Range] = ???

      def +(rhs: Int): T[Int] = this.+(toIRep(rhs))
    }

    class RangeOps(lhs: T[Range]) {
      def foldLeft[B](ini: B)(body: (B, T[Int]) => B)(implicit exposeRep: ExposeRep[B]): B = rangefold_a2(lhs, ini, exposeRep)(body)

      /*def foldLeft[B](ini: B)(body: ((B, T[Int])) => B)(implicit exposeRep: ExposeRep[B]): B = rangefold(lhs, ini, exposeRep)(body)*/
    }

    def rangefold_a2[B](range: T[Range], ini: B, exposeRep: ExposeRep[B])(body: (B, T[Int]) => B): B = {
      def helper(inf: (B, T[Int]) => B): (((B, T[Int])) => B) = {
        ???
      }
      rangefold(range, ini, exposeRep)(helper(body))
    }

    def rangefold[B](range: T[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, T[Int])) => B): B

  }

  implicit object isRep extends IRep[Rep] {
    def plus(lhs: Rep[Int], rhs: Rep[Int]): Rep[Int] = int_plus(lhs, rhs)

    def const(x: Int): Rep[Int] = Const(x)

    def rangefold[B](range: Rep[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, Rep[Int])) => B): B = range_foldLeft(range, ini, body)(exposeRep)


  }

  type NR[T] = T

  implicit object isNoRep extends IRep[NR] {
    def plus(lhs: NR[Int], rhs: NR[Int]): NR[Int] = lhs + rhs

    def const(x: Int): NR[Int] = x

    def rangefold[B](range: NR[Range], ini: B, exposeRep: ExposeRep[B])(body: ((B, NR[Int])) => B): B = {
      val f: (B, Int) => B = (b: B, i: Int) => body((b, i))
      range.foldLeft(ini)(f)
    }
  }

  //implicit def toOps[R[_],T](x: R[T])(implicit ev: IRep[R]): IRep[R]#Ops[T] = new ev.Ops(x)
/*


  def plus_meta(l: Int, r: Int): Int = l + r

  def plus_target(l: Rep[Int], r: Rep[Int]): Rep[Int] = {
    //l + r
    ???
  }


  def plus_poly[R[_] : IRep](l: R[Int], r: R[Int]): R[Int] = {
    l + r
  }

  def poly[R[_] : IRep](x: R[Int]): R[Int] = {
    x + x
    x + 3
  }


  def foo_nonpoly() = {
    implicit def conv(x: Int): Rep[Int] = ???
    val t: Rep[Int] = ???
    plus_target(t, 3)
    plus_target(2, 3)
    plus_meta(2, 3)
    //plus_meta(t,3)  //yields compile error
  }

  def foo_poly() = {
    implicit def Const(x: Int): Rep[Int] = ???
    implicit def Const2(x: Int): NR[Int] = ???
    val s: Rep[Int] = ???
    val c1: NR[Int] = 2
    plus_poly(s, Const(c1))
    plus_poly[Rep](2, 3)
    plus_poly(c1, c1)
  }


  def poly_sin[R[_] : IRep](x: R[Int]): R[Int] = ???

  def tiling[R[_] : IRep](a: Rep[Array[Int]], tiling: R[Int]): Rep[Int] = {
    val s = a.length
    val ev = implicitly[IRep[R]]
    import ev._
    val tiled: Rep[Int] = s / toRep(tiling)
    (Const(0) until tiled).foldLeft(Const(0))(
      (acc, idx_staged) => (ev.const(0) until tiling).foldLeft(acc)(
        (acc2, idx_poly) => {
          val idx = idx_staged * tiling + idx_poly
          acc2 + a(idx) + poly_sin(idx_poly)
        }
      )
    )
  }


  def loop_poly[R[_] : IRep](x: R[Int]): R[Int] = {
    /*implicit def r2rep(x: Range): R[Range] = ???
    val r: R[Range] = (1 until 10)
    r.foldLeft(x)( _ + _ )*/
    ???
  }

  def check(): Unit = {
    val r = 0 until 0
    val x = 10
    r.foldLeft(x)((acc, ele) => acc + x)
  }

  val s: Rep[Int] = ???
  val meta: Rep[Int] => Rep[Int] = (in: Rep[Int]) => in + Const(3)
  meta(meta(s))

  val target: Rep[Int => Int] = meta
  target(target(s))


  case class MayInline[A, R](f: Either[Rep[A] => Rep[R], Rep[A => R]]) {
    def apply(arg: Rep[A]): Rep[R] = f.fold(fa => fa(arg), fb => fb(arg))
  }

  val mf: MayInline[Int, Int] = ???
  mf(Const(3))


  abstract class AbstractContainer[R[_] : IRep, T] {
    def apply(i: R[Int]): Rep[T]

    def apply_plus1(i: R[Int]) = apply(i + 1)

    def update(i: R[Int], y: Rep[T])
  }

  class StagedArray extends
    AbstractContainer[Rep, Double] {
    val data: Rep[Array[Double]] = ???

    def apply(i: Rep[Int]): Rep[Double] = data(i)

    def update(i: Rep[Int], y: Rep[Double]) = {
      data(i) = y
    }
  }

  class MetaArrayofScalars
    extends AbstractContainer[NR, Double] {
    val data: Array[Rep[Double]] = ???

    def apply(i: Int) = data(i)

    def update(i: Int, y: Rep[Double]) = {
      data(i) = y
    }
  }

  def convert[R1: IRep, R2: IRep, T](l: AbstractContainer[R1, T],
                                     r: AbstractContainer[R2, T]) {
    val ev1 = implicitly[IRep[R1]]
    val ev2 = implicitly[IRep[R2]]
    for (i <- 0 until 2)
      l(ev1.const(i)) = r(ev2.const(i))
  }

  val l: MetaArrayofScalars = ???
  val r: StagedArray = ???
  convert(l, r)

  //this will actually scalarize


  case class Complex(re: Rep[Double], im: Rep[Double])

  val complex_val: Complex = ???
  val complex_f: Complex => Complex = ???
  implicit val exposeComplex: ExposeRep[Complex] = ???
  val staged_complex_f = doLambda(complex_f, false, None)
  val result: Complex = staged_complex_f(complex_val)


  def bla123() = {
    implicit def exposeRepFromRep[T](implicit tag: TypeRep[T]): ExposeRep[Rep[T]] = ???
    def doLambda[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = ???
  }


  def stage_pseudo[A, R](f: Function[A, R], args: ExposeRep[A], returns: ExposeRep[R]) = {
    def create_IR_Node(a: Any, b: Any) = ???
    def store_into_StagedFunction(a: Any, b: Any) = ???

    //create the lambda node first
    val symbolic_inputs = args.freshExps()
    val symbolic_as_meta_container = args.vec2t(symbolic_inputs)
    val lambda_result = f(symbolic_as_meta_container)
    val IR_node: Def[_ => _] = create_IR_Node(symbolic_inputs, lambda_result)
    //return a class containing all the infos on the created lambda Node
    val staged_function = store_into_StagedFunction(f, IR_node)


  }

  def doApply_pseudo[A, R](fun: StagedFunction[A, R], arg: A,
                           args: ExposeRep[A], returns: ExposeRep[R]): R = {
    def link_apply_with_newsyms(a: Any, b: Any) = ???
    def create_apply_node(a: Any) = ???
    val newsyms = returns.freshExps()
    val applyNode = create_apply_node(arg)
    val applyreturns = link_apply_with_newsyms(newsyms, newsyms)
    returns.vec2t(applyreturns)
  }

  def regular_dyn2static: Unit = {
    (0 until 10).foldLeft(Const(0))((acc, i) => acc + Const(i))
  }

  case class DynRTyp[T](x: Rep[T])

  def exposedyntyp[T]() = new ExposeRep[DynRTyp[T]] {
    val freshExps: Unit => Vector[Exp[_]] = ???
    val vec2t: Vector[Exp[_]] => DynRTyp[T] = ???
    val t2vec: DynRTyp[T] => Vector[Exp[_]] = ???
  }

  def recurse[T](x: DynRTyp[T]): DynRTyp[T] = {
    ???
  }

  case class ComplexV(v: Vector[Complex])

  def exposeComplexVector(length: Int, exComp: ExposeRep[Complex])
  : ExposeRep[ComplexV] = new ExposeRep[ComplexV] {
    def chop(in: Vector[Exp[_]],
             taker: Vector[Exp[_]] => Complex): Vector[Complex] = ???

    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) =>
      (0 until length).flatMap(t => exComp.freshExps()).toVector
    val vec2t: Vector[Exp[_]] => ComplexV = (in: Vector[Exp[_]]) =>
      ComplexV(chop(in, p => exComp.vec2t(p)))
    val t2vec: ComplexV => Vector[Exp[_]] = (inv: ComplexV) => {
      inv.v.flatMap(p => exComp.t2vec(p))
    }


    def f(x: ComplexV): ComplexV = ???

    val evidence = exposeComplexVector(1024, exposeComplex)
    val sf = doLambda(f, false, None)(evidence, evidence)
  }



  def specialize[R[_]: IRep](x: R[Int], a: Rep[Array[Int]]): Rep[Array[Int]] = {
    val ev = implicitly[IRep[R]]
    ev._if(x eq ev.const(0), {
      specialize(Const(0),a)
    }, {
      //do something use full
      ???
    })
  }

  //call with 8 ini
*/


  def check[R[_]: IRep](x: R[Int]): Rep[Int] = {
    val ev = implicitly[IRep[R]]
    if (x.isRep())
      ev._if(x eq ev.const(64), {
        binsearch(ev.toRep(x),0,64)
      }, {
        alg(x)
      })
    else alg(x)
  }
  def alg[R[_]: IRep](x: R[Int]): Rep[Int] = ???



  def binsearch[R[_]: IRep](check: Rep[Int], low: Int, high: Int): Rep[Int] = {
    val mid = low + (high - low)
    if ((high - low) == 0) {
      //we found the base case size
      check[NR](mid)
    }
    else {
      myifThenElse(check < Const(mid), {
        binsearch(check, low, mid)
      }, {
        binsearch(check, mid, high)
      })
    }
  }


  def dncalg[R[_]: IRep](x: R[Int], a: Rep[Array[Int]]): Rep[Array[Int]] = {
    val ev = implicitly[IRep[R]]
    if (x.isRep())
    ev._if(x eq ev.const(64), {
       binsearch(ev.toRep(x),0,64,a)
    }, {
      //do something use full
      ???
    })
    else
      ???
  }

}
*/
