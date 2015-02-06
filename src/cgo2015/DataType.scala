package ch.ethz.spirals.cgo2015


case class MyComplex(re: Double, im: Double){
  def +(that: MyComplex) = MyComplex(this.re + that.re, this.im + that.im)
  def -(that: MyComplex) = MyComplex(this.re - that.re, this.im - that.im)

}