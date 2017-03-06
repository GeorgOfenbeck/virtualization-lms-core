package SpiralSThesisGui

/**
  * Created by rayda on 01-Feb-17.
  */
object test extends App{
  val t = BreakDown.RadixFreedom()
  val x = t(9)
  println(x.size)
  ///x.map(p => println(p))
  /*for (i <- 0 until x.size)
    println(x(i))*/


  def compute(x: Int) = ???
  def blub() = {

    for (i <- 0 until 10) {
      compute(i)
      for (j <- 0 until 20) {
        compute(i*j)
        for (k <- 0 until 30) {
          compute(j*k)
        }
        for (k <- 0 until 30) {
          compute(k)
        }
      }
    }




  }
}


