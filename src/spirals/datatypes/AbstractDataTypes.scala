package ch.ethz.spirals.datatypes

import scala.virtualization.lms.common._

import scala.reflect.runtime.universe._


abstract class NumericOps[T](implicit val tag: TypeTag[T]) {
  //def cast[U: TypeRep](x: T): U

  def plus(x: T, y: T): T

  def minus(x: T, y: T): T

  def times(x: T, y: T): T

  def fromInt(x: Int): T

  class Ops(lhs: T) {
    def +(rhs: T) = plus(lhs, rhs)

    def -(rhs: T) = minus(lhs, rhs)

    def *(rhs: T) = times(lhs, rhs)
  }

  implicit def mkNumericOps(lhs: T): Ops = new Ops(lhs)
}


abstract class ElementOps[ElementClass[_], T] (implicit val numeric: NumericOps[T]) {
  self =>

  def plus(x: ElementClass[T], y: ElementClass[T]): ElementClass[T]

  def minus(x: ElementClass[T], y: ElementClass[T]): ElementClass[T]

  def times(x: ElementClass[T], y: ElementClass[T]): ElementClass[T]

  //def create(l: Vector[T]): ElementClass[T]

  //def extract(x: ElementClass[T]): Vector[T]

  class Ops(lhs: ElementClass[T]) {
    def +(rhs: ElementClass[T]) = plus(lhs, rhs)

    def -(rhs: ElementClass[T]) = minus(lhs, rhs)

    def *(rhs: ElementClass[T]) = times(lhs, rhs)
  }

  implicit def mkElementOps(lhs: ElementClass[T]): Ops = new Ops(lhs)
}

/**
 * @tparam V Staged container or not
 * @tparam A type of container (e.g. Array)
 * @tparam R Element staged or not
 * @tparam T Primitives type
 */
abstract class ArrayOps[V[_], A[_], R[_], T] {
  def alloc(s: V[Int]): V[A[T]]

  def apply(x: V[A[T]], i: V[Int]): R[T]

  def update(x: V[A[T]], i: V[Int], y: R[T])
}

abstract class LiftOps[R[_]] {
  def apply[T](x: T)(implicit tag: TypeRep[T]): R[T]
}


abstract class CVector[VectorRep[_], ElementClass[_], R[_], T]
(implicit
 val vrep: LiftOps[VectorRep],
 val erep: ElementOps[ElementClass, R[T]],
 val nrep: NumericOps[R[T]]
  )
{
  self =>
  def apply(i: VectorRep[Int]): ElementClass[R[T]]

  def create(s: VectorRep[Int]): CVector[VectorRep, ElementClass, R, T]

  def update(i: VectorRep[Int], y: ElementClass[R[T]])

  def size(): VectorRep[Int]
}


