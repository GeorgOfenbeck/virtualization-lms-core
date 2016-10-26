package sort

case class MyComplex (re: Double, im: Double){
  def cmp(that: MyComplex): Int = {
    if (re > that.re) 1
    else
      if (re < that.re) -1
    else
      if (im > that.im) 1
    else if (im < that.im) -1
    else 0
  }
}
