
package scala.virtualization.lms
package common


import shapeless.HList
import internal._


trait PureFunctions extends Base {
  def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _]
  implicit def fun[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _] = doLambda(f)
}


trait PureFunctionsExp extends PureFunctions with BaseExp{

  implicit def liftFunction2[T, R](implicit t: TypeRep[T], r: TypeRep[R]): TypeRep[T => R] = typeRep[T => R]

  case class Lambda[A <: HList,R <: HList, CA, CR](f: Function1[A,R], x: A, y: Block, args: ExposeRep[CA], returns: ExposeRep[CR]) extends Def[_ => _]

  def doLambdaDef[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]) : Def[_ => _] = {
    val x = args.freshExps()

    val hlistf: (args.hlist) => returns.hlist = (in: args.hlist ) => {
      val container = args.hlist2t(in)
      val tres = f(container)
      val hres: returns.hlist = returns.t2hlist(tres)
      hres
    }

    val res = hlistf(x)
    val explist = returns.hlist2Exps(res)
    val block = Block(explist)
    Lambda(hlistf, x, block, args, returns)
  }


  override def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Exp[_ => _] = {
    val x = args.freshExps()
    val y = doLambdaDef(f)(args, returns)
    toAtom(y)
  }

}
