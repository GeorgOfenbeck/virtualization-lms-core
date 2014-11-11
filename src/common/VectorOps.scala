package scala.virtualization.lms
package common


import scala.reflect.runtime.universe._



trait VectorwoSizeOps extends ImplicitOps{
  type VectorwoSize[+A] = scala.collection.immutable.Vector[A]

  implicit def varToVectorwoSizeOps[T:TypeTag](x: Rep[VectorwoSize[T]]) = new VectorwoSizeOpsCls(x)

  class VectorwoSizeOpsCls[T:TypeTag](a: Rep[VectorwoSize[T]]) {
    def apply(n: Rep[Int]) = vector_apply(a, n)
  }
  def vector_apply[T:TypeTag](x: Rep[VectorwoSize[T]], n: Rep[Int]): Rep[T]
}

trait VectorwoSizeOpsExp extends VectorwoSizeOps with ImplicitOpsExp{

  case class VectorApply[T:TypeTag](a: Exp[VectorwoSize[T]], n: Exp[Int]) extends Def[T]
  def vector_apply[T:TypeTag](x: Exp[VectorwoSize[T]], n: Exp[Int]): Exp[T] = VectorApply(x, n)
}


trait VectorOps extends ImplicitOps{
  object Vector {
    def apply[T:TypeTag](xs: Rep[T]*) = vector_obj_fromseq(xs)
    def apply[T:TypeTag](xs: List[Rep[T]]) = vector_obj_fromseq(xs)
  }

  def vector_obj_fromseq[T:TypeTag](xs: Seq[Rep[T]]): Rep[Vector[T]]
}


trait VectorOpsExp extends VectorOps with ImplicitOpsExp{

  case class VectorFromSeq[T:TypeTag](xs: Seq[Exp[T]]) extends Def[Vector[T]]
  def vector_obj_fromseq[T:TypeTag](xs: Seq[Exp[T]]) = VectorFromSeq(xs)

}
