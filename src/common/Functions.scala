
package scala.virtualization.lms
package common

import java.io.PrintWriter


import scala.virtualization.lms.util.ClosureCompare
import scala.reflect.runtime.universe._


trait PureFunctions extends ImplicitOps {

  def doLambda[A:TypeTag,B:TypeTag](fun: Rep[A] => Rep[B]): Rep[A => B]
  implicit def fun[A:TypeTag,B:TypeTag](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
  implicit def toLambdaOps[A:TypeTag,B:TypeTag](fun: Rep[A => B]) = new LambdaOps(fun)
  class LambdaOps[A:TypeTag,B:TypeTag](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = doApply(f,x)
  }



  def doApply[A:TypeTag,B:TypeTag](fun: Rep[A => B], arg: Rep[A]): Rep[B]
}


trait PureFunctionsExp extends PureFunctions with ImplicitOpsExp{
  case class Lambda[A:TypeTag,B:TypeTag](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B] { val mA = typeTag[A]; val mB = typeTag[B] }
  case class Apply[A:TypeTag,B:TypeTag](f: Exp[A => B], arg: Exp[A]) extends Def[B] { val mA = typeTag[A]; val mB = typeTag[B] }

  // unboxedFresh and unbox are hooks that can be overridden to
  // implement multiple-arity functions with tuples. These two methods
  // should be overridden consistently. unboxedFresh is used when
  // creating an abstraction, and unbox when applying it. See
  // TupledFunctionsExp for an example.

  def unboxedFresh[A:TypeTag] : Exp[A] = fresh[A]
  def unbox[A:TypeTag](x : Exp[A]) : Exp[A] = x

  def doLambdaDef[A:TypeTag,B:TypeTag](f: Exp[A] => Exp[B]) : Def[A => B] = {
    val x = unboxedFresh[A]
    //val y = reifyEffects(f(x)) // unfold completely at the definition site.
    val y = Block(f(x))
    Lambda(f, x, y)
  }

  override def doLambda[A:TypeTag,B:TypeTag](f: Exp[A] => Exp[B]): Exp[A => B] =
    doLambdaDef(f)

  override def doApply[A:TypeTag,B:TypeTag](f: Exp[A => B], x: Exp[A]): Exp[B] = {
    val x1 = unbox(x)
    Apply(f, x1)
  }

  override def syms(e: Any): Vector[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): Vector[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(x) ++ effectSyms(y)
    case _ => super.boundSyms(e)
  }

// TODO: right now were trying to hoist as much as we can out of functions.
// That might not always be appropriate. A promising strategy would be to have
// explicit 'hot' and 'cold' functions.

/*
  override def hotSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.hotSyms(e)
  }
*/

  override def symsFreq(e: Any): Vector[(Sym[Any], Double)] = e match {
    case Lambda(f, x, y) => freqHot(y)
    case _ => super.symsFreq(e)
  }
}


trait PureFunctionsRecursiveExp extends PureFunctionsExp with ClosureCompare {
  var funTable: List[(Sym[_], Any)] = List()
  override def doLambda[A:TypeTag,B:TypeTag](f: Exp[A] => Exp[B]): Exp[A => B] = {
    val can = canonicalize(f)
    funTable.find(_._2 == can) match {
      case Some((funSym, _)) =>
        funSym.asInstanceOf[Exp[A=>B]]
      case _ =>
        val funSym = fresh[A=>B]
        funTable = (funSym,can)::funTable
        createDefinition(funSym, doLambdaDef(f))
        funSym
    }
  }

}


