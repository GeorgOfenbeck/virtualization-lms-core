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

trait ExposeRepBase{
  trait ExposeRep[T] {
    type hlist <: HList
    val freshSyms: Unit => hlist
    val hlist2t: hlist=> T
    val t2hlist: T=>hlist
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
}

/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Expressions with Blocks /*with Transforming*/ {
  type Rep[T] = Exp[T]
  protected def unit[T:TypeRep](x: T) = Const(x)

  case class TypeExp[T](tag: TypeTag[T]) extends TypeRep[T]
  def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T] = tr

  implicit def convertFromTypeTag[T](tag: TypeTag[T]): TypeRep[T] = TypeExp(tag)
  implicit def typeRepFromTypeTag[T](implicit tag: TypeTag[T]): TypeRep[T] = TypeExp(tag)

  private def helper1[T](u : Unit)(implicit tag: TypeTag[T]): ::[Rep[T], HNil] = fresh[T](tag) :: HNil
  private def helper2[T](x : ::[Rep[T], HNil]): Rep[T] = x.head
  private def helper3[T](x: Rep[T]): ::[Rep[T], HNil] = x :: HNil

  implicit def exposeRepFromRep[T](implicit tag: TypeTag[T]): ExposeRep[Rep[T]] = new ExposeRep[Exp[T]](){
    type hlist = ::[Exp[T], HNil]
    val freshSyms: Unit => hlist = helper1
    val hlist2t: hlist => Exp[T] = helper2
    val t2hlist: Exp[T] => hlist = helper3
  }
}

trait BlockExp extends BaseExp

