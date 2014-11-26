package scala.virtualization.lms
package internal


trait Blocks extends Expressions { //why?



  case class Block(val res: Vector[Exp[_]])  {
    require(res.isEmpty == false)
  }


  /*def blocks(e: Any): Vector[Block[Any]] = e match {
    case b: Block[Any] => Vector(b)
    case p: Product => p.productIterator.toVector.flatMap(blocks(_))
    case _ => Vector()
  }

  def getBlockResult[A](s: Block[A]): Exp[A] = getBlockResultFull(s) // = s.res
  def getBlockResultFull[A](s: Block[A]): Exp[A] = s.res //RF why comment in original?*/

}