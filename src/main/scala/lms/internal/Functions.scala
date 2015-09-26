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

trait Functions extends Base with ExposeRepBase {

  def doLambda[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]
  def doInternalLambda[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R]

  implicit def fun[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = doLambda(f)
  implicit def internalfun[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = doInternalLambda(f)
  //def doApply[A,R](fun: Rep[_ => _], arg: A)(implicit args: ExposeRep[A], returns: ExposeRep[R]): R

  case class StagedFunction[A, R](f: A => R, exp: Rep[_ => _], args: ExposeRep[A], returns: ExposeRep[R])
  implicit def toLambdaOps[A, R](fun: StagedFunction[A, R]) = new LambdaOps(fun)

  class LambdaOps[A, R](f: StagedFunction[A, R]) {
    def apply(x: A): R = doApplySF(f, x, f.args, f.returns)
  }

  def doApplySF[A, R](fun: StagedFunction[A, R], arg: A, args: ExposeRep[A], returns: ExposeRep[R]): R
}


trait FunctionsExp extends Functions with BaseExp with ClosureCompare {
  implicit def liftFunction2[T, R](implicit t: TypeRep[T], r: TypeRep[R]): TypeRep[T => R] = typeRep[T => R]
  implicit def exposeFunction[A, R](implicit args: ExposeRep[A], returns: ExposeRep[R]): ExposeRep[StagedFunction[A, R]] =
    new ExposeRep[StagedFunction[A, R]]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
        def helper[T]()(implicit tag: TypeRep[T]): TypeRep[T] = {
          tag match {
            case x@TypeExp(mf, dynTags) => {
              val f: Unit => (Vector[TypeRep[_]], Vector[TypeRep[_]]) = (u: Unit) => {
                val a = args.freshExps().map(ele => exp2tp(ele).tag)
                val r = returns.freshExps().map(ele => exp2tp(ele).tag)
                (a, r)
              }
              x.copy(dynTags = Some(f))
            }
            case _ => {
              assert(false, "this should never match")
              tag
            }
          }
        }
        val tagnew = helper[_ => _]
        val lambda: Exp[Function[_, _]] = Arg[_ => _](tagnew)
        Vector(lambda)
      }
      val vec2t: Vector[Exp[_]] => StagedFunction[A, R] = (in: Vector[Exp[_]]) => {
        val f: (A => R) = (ina: A) => ???
        StagedFunction(f, in.head.asInstanceOf[Rep[_ => _]], args, returns)
      }
      val t2vec: StagedFunction[A, R] => Vector[Exp[_]] = (in: StagedFunction[A, R]) => {
        Vector(in.exp)
      }
    }

  override def reset() = {
    super.reset()
  }

  case class ReturnArg(f: Exp[_], newsym: Exp[_], pos: Int, tuple: Boolean, last: Boolean) extends Def[Any]
  abstract class AbstractLambda[CA, CR](val f: Function1[Vector[Exp[_]], Vector[Exp[_]]], val x: Vector[TP[_]], val y: Block,
                               val args: ExposeRep[CA], val returns: ExposeRep[CR]) extends Def[_ => _]

  //seems we can only put any here since we cannot know at compile time
  case class InternalLambda[CA, CR](override val f: Function1[Vector[Exp[_]], Vector[Exp[_]]], override val x: Vector[TP[_]],
                                    override val y: Block,
                                    override val args: ExposeRep[CA],
                                    override val returns: ExposeRep[CR]) extends AbstractLambda(f,x,y,args,returns)

  //seems we can only put any here since we cannot know at compile time
  case class ExternalLambda[CA, CR](override val f: Function1[Vector[Exp[_]], Vector[Exp[_]]], override val x: Vector[TP[_]],
                                    override val y: Block,
                                    override val args: ExposeRep[CA],
                                    override val returns: ExposeRep[CR]) extends AbstractLambda(f,x,y,args,returns)


  case class InternalApply[CA, CR](f: Exp[_ => _], arg: Vector[Exp[_]]) extends Def[Any]

  //RF (any?)

  def doLambdaDef[A, R](f: Function1[A, R], internal: Boolean)(implicit args: ExposeRep[A], returns: ExposeRep[R]): AbstractLambda[A, R] = {
    addBlockTPBuffer()
    val freshexps = args.freshExps()
    val tps = freshexps map (exp => id2tp(exp.id))
    val vecf = (in: Vector[Exp[_]]) => {
      val container = args.vec2t(in)
      val tres = f(container)
      val hres = returns.t2vec(tres)
      hres
    }
    val explist = vecf(freshexps)
    val block = new Block(explist)
    block2tps = block2tps + (block -> getBlockTPBuffer())
    if (internal)
      InternalLambda(vecf, tps, block, args, returns)
    else
      ExternalLambda(vecf, tps, block, args, returns)
  }

  def doApplySF[A, R](fun: StagedFunction[A, R], arg: A, args: ExposeRep[A], returns: ExposeRep[R]): R = {
    val newsyms = returns.freshExps()
    val funexp = fun.exp
    val applynode = InternalApply(funexp, args.t2vec(arg)) //, Block(newsyms))
    val applynodeexp = toAtom(applynode)
    val returnNodes = if (newsyms.size > 1) {
      newsyms.zipWithIndex.map(fsym => {
        //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
        val otp = exp2tp(fsym._1)
        val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(applynodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
        val newx = toAtom(cc)(tag, null)
        /*if (tag.mf.toString().contains("Function")) {
          val newtp = exp2tp(newx)
          println(newtp)
        }*/
        newx
      })
    } else {
      newsyms.zipWithIndex.map(fsym => {
        val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
        val cc: Def[Any] = ReturnArg(applynodeexp, fsym._1, fsym._2, false, true)
        val newx = toAtom(cc)(tag, null)
        newx
      })
    }
    returns.vec2t(returnNodes)
  }


  def doAbstractLambda[A, R](f: Function1[A, R], internal: Boolean)(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = {
    def helper[T](d: Def[T])(implicit tag: TypeRep[T]): TypeRep[T] = {
      tag match {
        case x@TypeExp(mf, dynTags) => {
          val f: Unit => (Vector[TypeRep[_]], Vector[TypeRep[_]]) = (u: Unit) => {
            val a = args.freshExps().map(ele => exp2tp(ele).tag)
            val r = returns.freshExps().map(ele => exp2tp(ele).tag)
            (a, r)
          }
          x.copy(dynTags = Some(f))
        }
        case _ => {
          assert(false, "this should never match")
          tag
        }
      }
    }
    val y = doLambdaDef(f, internal)(args, returns)
    val tag = helper(y)
    val exp = toAtom(y)(tag, null)
    val tp = id2tp(exp.id).asInstanceOf[TP[_ => _]]
    val stagedFunction: StagedFunction[A, R] = StagedFunction(f, exp, y.args, y.returns)
    stagedFunction
  }

  override def doLambda[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = {
    doAbstractLambda(f,false)
  }
  override def doInternalLambda[A, R](f: Function1[A, R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): StagedFunction[A, R] = {
    doAbstractLambda(f,true)
  }

  override def syms(e: Any): Vector[Exp[_]] = e match {
    case InternalLambda(f, x, y, args, returns) => {
      Vector.empty
    }
    case ExternalLambda(f, x, y, args, returns) => {
      Vector.empty
    }
    case _ => {
      super.syms(e)
    }
  }

  override def boundSyms(e: Any): Vector[Exp[_]] = e match {
    case a@InternalApply(f, arg) => {
      Vector.empty
    }
    case l@InternalLambda(f, x, y, _, _) => {
      val exps = l.x map (tp => tp.sym)
      val t = syms(exps)
      t
    }
    case _ => {
      super.boundSyms(e)
    }
  }


}

