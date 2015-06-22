package scala.lms
package internal

import scala.virtualization.lms.util.ClosureCompare


trait ExposeRepBase extends Expressions{
  trait ExposeRep[T] {
    val freshExps: Unit => Vector[Exp[_]]
    val vec2t: Vector[Exp[_]] => T
    val t2vec: T=> Vector[Exp[_]]
  }
}

trait InternalFunctions extends Base with ExposeRepBase{

 var fun2tp: Map[Any, TP[_]] = Map.empty

 def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _]
 implicit def fun[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _] = doLambda(f)
 def doApply[A,R](fun: Rep[_ => _], arg: Rep[A])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[R]
}


trait InternalFunctionsExp extends InternalFunctions with BaseExp with ClosureCompare{
 implicit def liftFunction2[T, R](implicit t: TypeRep[T], r: TypeRep[R]): TypeRep[T => R] = typeRep[T => R]
 case class InternalLambda[CA, CR](f: Function1[Vector[Exp[_]],Vector[Exp[_]]], x: Vector[TP[_]], y: Block, args: ExposeRep[CA], returns: ExposeRep[CR]) extends Def[_ => _]

 def doLambdaDef[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]) : Def[_ => _] = {
   val freshexps = args.freshExps()
   val tps = freshexps map (exp => id2tp(exp.id))
   val vecf = (in: Vector[Exp[_]]) => {
    val container = args.vec2t(in)
    val tres = f(container)
    val hres = returns.t2vec(tres)
    //println(hres)
    hres
   }
   val explist = vecf(freshexps)
   val block = Block(explist)
   InternalLambda(vecf, tps, block, args, returns)
 }

 override def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Exp[_ => _] = {
  val can = canonicalize(f)
  val existingf = fun2tp.get(can)
  if (existingf.isDefined) {
   existingf.get.sym.asInstanceOf[Exp[_ => _]]
  }
  else  {
   val y = doLambdaDef(f)(args, returns)
   val exp = toAtom(y)
   val tp = id2tp(exp.id)
   fun2tp = fun2tp + (can -> tp)
   exp
  }
 }

 override def doApply[A,R](fun: Exp[_ => _], arg: Exp[A])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Exp[R] = {
  ???
 }

 override def syms(e: Any): Vector[Exp[_]] = e match {
  case InternalLambda(f,x,y,args,returns) => {
   Vector.empty
  }
  case _ => {
   super.syms(e)
  }
 }

 override def boundExps(e: Any): Vector[Exp[_]] = e match {
  case l@InternalLambda(f, x, y,_,_) => {
   val exps = l.x map (tp => tp.sym)
   val t = syms(exps)
   t
  }
  case _ => {
   super.boundExps(e)
  }
 }




}
