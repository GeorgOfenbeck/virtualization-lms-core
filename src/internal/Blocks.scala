package scala.virtualization.lms
package internal

import scala.annotation.unchecked.uncheckedVariance


trait Blocks extends Expressions {

  case class Block[+T](val res: Exp[T]) { def tp: Manifest[T @uncheckedVariance] = res.tp } // variance ...
  //RF!

  def blocks(e: Any): Vector[Block[Any]] = e match {
    case b: Block[Any] => Vector(b)
    case p: Product => p.productIterator.toVector.flatMap(blocks(_))
    case _ => Vector()
  }

  def getBlockResult[A](s: Block[A]): Exp[A] = getBlockResultFull(s) // = s.res
  def getBlockResultFull[A](s: Block[A]): Exp[A] = s.res //RF why comment in original?

}