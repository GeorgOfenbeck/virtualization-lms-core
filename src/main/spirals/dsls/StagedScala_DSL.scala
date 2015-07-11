package ch.ethz.spirals.dsls

import scala.lms._
import ops._
import scala.lms.internal.InternalFunctionsExp


case class MyComplex(re: Double, im: Double){
  def +(that: MyComplex) = MyComplex(this.re + that.re, this.im + that.im)
  def -(that: MyComplex) = MyComplex(this.re - that.re, this.im - that.im)
}

trait StagedScala_Exp  extends PureNumericOpsExp with BaseExp with InternalFunctionsExp{
  case class StagedComplex(re: Exp[Double], im: Exp[Double]){
    def +(that: StagedComplex) = StagedComplex(this.re + that.re, this.im + that.im)
    def -(that: StagedComplex) = StagedComplex(this.re - that.re, this.im - that.im)
  }

  import scala.reflect.runtime.universe._
  implicit def exposeRepFromStagedComplex(implicit tag: Manifest[Double]): ExposeRep[StagedComplex] = new ExposeRep[StagedComplex](){
    val freshExps = (u: Unit) => Vector(Arg[Double](tag), Arg[Double](tag))
    val vec2t = (v: Vector[Exp[_]]) => {
      val re: Exp[Double] = v(0).asInstanceOf[Exp[Double]] //RF: use the Manifest to make this safe
      val im: Exp[Double] = v(1).asInstanceOf[Exp[Double]]
      StagedComplex(re,im)
    }
    val t2vec = (c: StagedComplex) => Vector(c.re,c.im)
  }
  case class ComplexVector(val vec: Vector[StagedComplex])
  implicit def exposeRepFromVComplex( instance_size: Int)(implicit tag: Manifest[Double] , exposeComplex: ExposeRep[StagedComplex]
                                         ): ExposeRep[ComplexVector] = new ExposeRep[ComplexVector](){
    val freshExps = (u: Unit) => {
      val t = for(i <- 0 until instance_size) yield exposeComplex.freshExps()
      t.foldLeft(Vector.empty[Exp[_]])((acc,ele) => { acc ++ ele })
    }
    val vec2t: (Vector[Exp[_]] => ComplexVector) = (h: Vector[Exp[_]]) => ComplexVector(h.grouped(2).foldLeft(Vector.empty[StagedComplex])((acc,ele) => { acc :+ exposeComplex.vec2t(ele)}))
    val t2vec = (v: ComplexVector) => v.vec.foldLeft(Vector.empty[Exp[_]])((acc,ele) => { acc ++ exposeComplex.t2vec(ele)})
  }
}


class StagedScala_DSL extends StagedScala_Exp