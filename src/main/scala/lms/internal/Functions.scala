package scala.lms
package internal

import scala.virtualization.lms.util.ClosureCompare


trait ExposeRepBase extends Expressions {

  trait ExposeRep[T] {
    val freshExps: Unit => Vector[Exp[_]]
    val vec2t: Vector[Exp[_]] => T
    val t2vec: T => Vector[Exp[_]]
  }

}

trait InternalFunctions extends Base with ExposeRepBase {
  def doLambda[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): (A => R)

  implicit def fun[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): (A => R) = doLambda(f)

  //def doApply[A,R](fun: Rep[_ => _], arg: A)(implicit args: ExposeRep[A], returns: ExposeRep[R]): R
}


trait InternalFunctionsExp extends InternalFunctions with BaseExp with ClosureCompare {
  implicit def liftFunction2[T, R](implicit t: TypeRep[T], r: TypeRep[R]): TypeRep[T => R] = typeRep[T => R]

  case class ReturnArg(f: Exp[_], newsym: Exp[_], pos: Int, tuple: Boolean, last: Boolean) extends Def[Any] //seems we can only put any here since we cannot know at compile time

  case class InternalLambda[CA, CR](f: Function1[Vector[Exp[_]], Vector[Exp[_]]], x: Vector[TP[_]], y: Block, args: ExposeRep[CA], returns: ExposeRep[CR]) extends Def[_ => _] {
    def createApply(lambda: Exp[_ => _]): CA => CR = {
      val f = (applyargs: CA) => {
        val newsyms = returns.freshExps()
        val applynode = InternalApply(lambda, args.t2vec(applyargs)) //, Block(newsyms))
        val applynodeexp = toAtom(applynode)
        val returnNodes = if (newsyms.size > 1) {
          newsyms.zipWithIndex.map(fsym => {
            //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
            val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
            val cc: Def[Any] = ReturnArg(applynodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2+1)
            val newx = toAtom(cc)(tag,null)
            newx
          })
        } else {
          newsyms.zipWithIndex.map(fsym => {
            val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
            val cc: Def[Any] = ReturnArg(applynodeexp, fsym._1, fsym._2, false, true)
            val newx = toAtom(cc)(tag,null)
            newx
          })
        }
        //returns.vec2t(newsyms)
        returns.vec2t(returnNodes)
      }
      f
    }
  }

  case class InternalApply[CA, CR](f: Exp[_ => _], arg: Vector[Exp[_]]) extends Def[Any]

  //RF (any?)


  /*
    def doApply[A,R](fun: Exp[_ => _], arg: A)(implicit args: ExposeRep[A], returns: ExposeRep[R]): R = {
      val ftp = id2tp(fun.id)
      ftp.rhs match {
        case InternalLambda(f,x,y,args,returns) => returns.vec2t(y.res).asInstanceOf[R]
        case _ => ???
      }
    }
  */


  def doLambdaDef[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): InternalLambda[A, R] = {
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

  override def doLambda[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): (A => R) = {
    val y = doLambdaDef(f)(args, returns)
    val exp = toAtom(y)
    val tp = id2tp(exp.id)
    val returnf = y.createApply(exp)
    fun2tp = fun2tp + (returnf -> tp)
    tp2fun = tp2fun + (tp -> returnf)
    returnf
  }

  override def syms(e: Any): Vector[Exp[_]] = e match {
    case InternalLambda(f, x, y, args, returns) => {
      Vector.empty
    }
    case _ => {
      super.syms(e)
    }
  }

  override def boundExps(e: Any): Vector[Exp[_]] = e match {
    case a@InternalApply(f, arg) => {
      Vector.empty
    }
    case l@InternalLambda(f, x, y, _, _) => {
      val exps = l.x map (tp => tp.sym)
      val t = syms(exps)
      t
    }
    case _ => {
      super.boundExps(e)
    }
  }


}
