
package scala.virtualization.lms
package common

import java.io.PrintWriter


import scala.virtualization.lms.util.ClosureCompare
import scala.reflect.runtime.universe._

import shapeless.HList

import scala.virtualization.lms.internal.Expressions
import scala.virtualization.lms.util.ClosureCompare
import scala.reflect.runtime.universe._
import common._




trait PureFunctions extends Base {



  //def doLambda[A,R](f: Function1[A,R])(implicit args: TypeRep[A], returns: TypeRep[R]): Rep[A => R]
  //implicit def fun[A,R](f: Function1[A,R])(implicit args: TypeRep[A], returns: TypeRep[R]): Rep[A=>R] = doLambda(f)

  def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _]
  implicit def fun[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _] = doLambda(f)


  /*
  implicit def toLambdaOps[A:TypeRep,B:TypeRep](fun: Rep[A => B]) = new LambdaOps(fun)
  class LambdaOps[A:TypeRep,B:TypeRep](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = doApply(f,x)
  }
  def doApply[A:TypeRep,B:TypeRep](fun: Rep[A => B], arg: Rep[A]): Rep[B] */
}



trait PureFunctionsExp extends PureFunctions with BaseExp{



  //case class Lambda[A:TypeRep,B:TypeRep](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B] { val mA = typeTag[A]; val mB = typeTag[B] }
  //case class Apply[A:TypeRep,B:TypeRep](f: Exp[A => B], arg: Exp[A]) extends Def[B] { val mA = typeTag[A]; val mB = typeTag[B] }

  // unboxedFresh and unbox are hooks that can be overridden to
  // implement multiple-arity functions with tuples. These two methods
  // should be overridden consistently. unboxedFresh is used when
  // creating an abstraction, and unbox when applying it. See
  // TupledFunctionsExp for an example.                                                             o

  def unboxedFresh[A:TypeRep] : Exp[A] = fresh[A]
  def unbox[A:TypeRep](x : Exp[A]) : Exp[A] = x


  implicit def liftFunction2[T, R](implicit t: TypeRep[T], r: TypeRep[R]): TypeRep[T => R] = {
    //implicit val mf = t.mf
    //implicit val rmf = r.mf
    typeRep[T => R]
  }


  case class Lambda[A,R <: HList, CA, CR](f: Function1[A,R], x: A, y: Block[R], args: ExposeRep[CA], returns: ExposeRep[CR]) extends Def[_ => _]

  def doLambdaDef[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]) : Def[_ => _] = {
    val x = args.freshSyms()
    //val y = returns.freshSym()

    val hlistf: (args.hlist) => returns.hlist = (in: args.hlist ) => {
      val container = args.hlist2t(in)
      val tres = f(container)
      val hres: returns.hlist = returns.t2hlist(tres)
      hres
    }

    val res = hlistf(x)
    val block = Block(res)

    //val y = Block(f(x1))


    Lambda(hlistf, x, block, args, returns)
    //val x = unboxedFresh[A]
    //val y = reifyEffects(f(x)) // unfold completely at the definition site.
    //val y = Block(f(x))

    //???
  }

  //def doLambda[A,R](f: Function1[A,R])(implicit args: TypeRep[A], returns: TypeRep[R]): Rep[A => R]
  override def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Exp[_ => _] = {
    val x = args.freshSyms()
    //val x = doLambdaDef(f)(args, returns)
    //val y = returns.freshSym()
    val y = doLambdaDef(f)(args, returns)
    //toAtom(y)(liftFunction2(args,returns))
    toAtom(y)
  }
  /*
    override def doApply[A:TypeRep,B:TypeRep](f: Exp[A => B], x: Exp[A]): Exp[B] = {
      val x1 = unbox(x)
      Apply(f, x1)
    }*/

  /*  override def syms(e: Any): Vector[Sym[Any]] = e match {
      case Lambda(f, x, y) => syms(y)
      case _ => super.syms(e)
    }

    override def boundSyms(e: Any): Vector[Sym[Any]] = e match {
      case Lambda(f, x, y) => syms(x) ++ effectSyms(y)
      case _ => super.boundSyms(e)
    }*/

  // TODO: right now were trying to hoist as much as we can out of functions.
  // That might not always be appropriate. A promising strategy would be to have
  // explicit 'hot' and 'cold' functions.

  /*
    override def hotSyms(e: Any): List[Sym[Any]] = e match {
      case Lambda(f, x, y) => syms(y)
      case _ => super.hotSyms(e)
    }
  */
  /*
    override def symsFreq(e: Any): Vector[(Sym[Any], Double)] = e match {
      case Lambda(f, x, y) => freqHot(y)
      case _ => super.symsFreq(e)
    }*/
}
