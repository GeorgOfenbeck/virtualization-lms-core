
package scala.virtualization.lms
package common



import internal._


trait PureFunctions extends Base {
  def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _]
  implicit def fun[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[_ => _] = doLambda(f)
  def doApply[A,R](fun: Rep[_ => _], arg: Rep[A])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Rep[R]
}


trait PureFunctionsExp extends PureFunctions with BaseExp{

  implicit def liftFunction2[T, R](implicit t: TypeRep[T], r: TypeRep[R]): TypeRep[T => R] = typeRep[T => R]

  case class Lambda[CA, CR](f: Function1[Vector[Exp[_]],Vector[Exp[_]]], x: Vector[TP[_]], y: Block, args: ExposeRep[CA], returns: ExposeRep[CR]) extends Def[_ => _]

  def doLambdaDef[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]) : Def[_ => _] = {
    val freshexps = args.freshExps()
    val tps = freshexps map ( exp => id2tp(exp.id))
      val hlistf: (args.hlist) => returns.hlist = (in: args.hlist ) => {
        val container = args.hlist2t(in)
        val tres = f(container)
        val hres: returns.hlist = returns.t2hlist(tres)
        hres
      }

      val explist = hlistf(freshexps)
      val block = Block(explist)

      Lambda(hlistf, tps, block, args, returns)
    }


  override def doLambda[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Exp[_ => _] = {
    val y = doLambdaDef(f)(args, returns)
    toAtom(y)
  }

  override def doApply[A,R](fun: Exp[_ => _], arg: Exp[A])(implicit args: ExposeRep[A], returns: ExposeRep[R]): Exp[R] = {
    ???
  }


  override def boundExps(e: Any): List[Exp[Any]] = e match {
  case l@Lambda(f, x, y,_,_) => {
  val exps = l.x map (tp => tp.exp)
  val t = syms(exps)
  t
    }
    case _ => {
      super.boundExps(e)
    }
  }




}
