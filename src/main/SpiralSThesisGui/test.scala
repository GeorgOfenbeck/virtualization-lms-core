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
}
