package scala.virtualization.lms
package common

import scala.reflect.SourceContext




trait VectorwoSizeOps extends ImplicitOps{
  type VectorwoSize[+A] = scala.collection.immutable.Vector[A]

  implicit def varToVectorwoSizeOps[T:Manifest](x: Rep[VectorwoSize[T]]) = new VectorwoSizeOpsCls(x)

  class VectorwoSizeOpsCls[T:Manifest](a: Rep[VectorwoSize[T]]) {
    def apply(n: Rep[Int])(implicit pos: SourceContext) = vector_apply(a, n)
  }
  def vector_apply[T:Manifest](x: Rep[VectorwoSize[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
}

trait VectorwoSizeOpsExp extends VectorwoSizeOps with ImplicitOpsExp{

  case class VectorApply[T:Manifest](a: Exp[VectorwoSize[T]], n: Exp[Int]) extends Def[T]
  def vector_apply[T:Manifest](x: Exp[VectorwoSize[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = VectorApply(x, n)
}


trait VectorOps extends ImplicitOps{
  object Vector {
    def apply[T:Manifest](xs: Rep[T]*) = vector_obj_fromseq(xs)
    def apply[T:Manifest](xs: List[Rep[T]]) = vector_obj_fromseq(xs)
  }

  def vector_obj_fromseq[T:Manifest](xs: Seq[Rep[T]]): Rep[Vector[T]]
}


trait VectorOpsExp extends VectorOps with ImplicitOpsExp{

  case class VectorFromSeq[T:Manifest](xs: Seq[Exp[T]]) extends Def[Vector[T]]
  def vector_obj_fromseq[T:Manifest](xs: Seq[Exp[T]]) = VectorFromSeq(xs)

}
