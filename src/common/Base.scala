package scala.virtualization.lms
package common

import internal._
import scala.reflect.runtime.universe._

import shapeless._
/**
 * This trait automatically lifts any concrete instance to a representation.
 */
trait LiftAll extends Base {
  protected implicit def __unit[T:TypeTag](x: T) = unit(x)
}


trait TypeRepBase{
//we pack this into a trait such that its automatically mixed into every potential DSL
  trait TypeRep[T]  {
    def tag: TypeTag[T]

  }
  implicit def convertFromTypeTag[T](tag: TypeTag[T]): TypeRep[T] //= TypeExp(tag)
  implicit def typeRepFromTypeTag[T](implicit tag: TypeTag[T]): TypeRep[T] //= TypeExp(tag)
}

trait ExposeRepBase extends Expressions{
  trait ExposeRep[T] {
    type hlist = Vector[Exp[_]]
    val freshExps: Unit => hlist
    val hlist2t: hlist => T
    val t2hlist: T=> hlist
  }
}



/**
 * The Base trait defines the type constructor Rep, which is the higher-kinded type that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1 
 */
trait Base extends TypeRepBase with ExposeRepBase{
  type API <: Base
  type Rep[T]
  protected def unit[T:TypeRep](x: T): Rep[T]
  // always lift Unit and Null (for now)
  implicit def unitToRepUnit(x: Unit) = unit(x)
  implicit def nullToRepNull(x: Null) = unit(x)
  def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T]
}

/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Expressions with Blocks with ExposeRepBase/*with Transforming*/ {
  type Rep[T] = Exp[T]
  protected def unit[T:TypeRep](x: T) = Const(x)

  case class TypeExp[T](tag: TypeTag[T]) extends TypeRep[T]
  def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T] = tr

  implicit def convertFromTypeTag[T](tag: TypeTag[T]): TypeRep[T] = TypeExp(tag)
  implicit def typeRepFromTypeTag[T](implicit tag: TypeTag[T]): TypeRep[T] = TypeExp(tag)


  implicit def exposeRepFromRep[T](implicit tag: TypeTag[T]): ExposeRep[Rep[T]] = new ExposeRep[Exp[T]](){
    val freshExps: Unit => hlist = (u: Unit) => Vector(Arg[T](tag))
    val hlist2t: hlist => Exp[T] = (v: Vector[Exp[_]]) => v.head.asInstanceOf[Exp[T]]   //RF!!!!
    val t2hlist: Exp[T] => hlist = (x: Rep[T]) => Vector(x)
  }
}

trait BlockExp extends BaseExp

