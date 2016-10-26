package sort

/**
  * Created by rayda on 14-Oct-16.
  */
object RegularSort extends App{

  case class Complex(re: Double, im: Double)
  def sortit(input: Array[Complex],  f: (Complex,Complex) => Boolean): Array[Complex] = input.sortWith(f)

}
