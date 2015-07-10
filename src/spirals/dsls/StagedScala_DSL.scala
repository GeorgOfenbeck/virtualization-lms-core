package ch.ethz.spirals.dsls



case class MyComplex(re: Double, im: Double){
  def +(that: MyComplex) = MyComplex(this.re + that.re, this.im + that.im)
  def -(that: MyComplex) = MyComplex(this.re - that.re, this.im - that.im)
}




import scala.virtualization.lms.common._
trait StagedScala_Exp  extends NumericOpsExp with BaseExp with PureFunctionsExp{

  case class StagedComplex(re: Exp[Double], im: Exp[Double]){
    def +(that: StagedComplex) = StagedComplex(this.re + that.re, this.im + that.im)
    def -(that: StagedComplex) = StagedComplex(this.re - that.re, this.im - that.im)
  }


















  import scala.reflect.runtime.universe._
  implicit def exposeRepFromStagedComplex[T](implicit tag: TypeTag[T]): ExposeRep[StagedComplex] = new ExposeRep[StagedComplex](){
    val freshExps: Unit => hlist = (u: Unit) => Vector(Arg[T](tag), Arg[T](tag))
    val hlist2t: hlist => StagedComplex = (v: hlist) => {
      val re: Exp[Double] = v(0).asInstanceOf[Exp[Double]] //RF!!!!
      val im: Exp[Double] = v(1).asInstanceOf[Exp[Double]]
      StagedComplex(re,im)
    }
    val t2hlist: StagedComplex => hlist = (c: StagedComplex) => Vector(c.re,c.im)
  }

  case class ComplexVector[T](val vec: Vector[StagedComplex])


  implicit def exposeRepFromVComplex[T]( instance_size: Int)(implicit tag: TypeTag[T] ,
                                        exposeComplex: ExposeRep[StagedComplex]
                                         ): ExposeRep[ComplexVector[T]] = new ExposeRep[ComplexVector[T]](){

    val freshExps: Unit => hlist = (u: Unit) => {
      val t = for(i <- 0 until instance_size) yield exposeComplex.freshExps()
      val con: hlist = t.foldLeft(Vector.empty[Exp[_]])((acc,ele) => {
        acc ++ ele
      })
      con

    }
    val hlist2t: hlist => ComplexVector[T] = (h: hlist) => {
      val vc = h.grouped(2)
      val v = vc.foldLeft(Vector.empty[StagedComplex])((acc,ele) => {
        val c = exposeComplex.hlist2t(ele)
        acc :+ c
      })
      ComplexVector(v)
    }
    val t2hlist: ComplexVector[T] => hlist = (v: ComplexVector[T]) => {
      v.vec.foldLeft(Vector.empty[Exp[_]])((acc,ele) => {
        val part = exposeComplex.t2hlist(ele)
        acc ++ part
      })
    }
  }

}


class StagedScala_DSL extends StagedScala_Exp