package SpiralS2

/**
  * Created by rayda on 07-Jan-17.
  */
object VecHTest extends App {

  case class IM(base: Int, s0: Int, s1: Int)

  def resolveH(h: IM, i: Int, v: Int): Int = h.base + (h.s0 * i) + (h.s1 * v)

  def fuseIM(r: IM, s: IM, lv: Int): IM = IM((r.base + (r.s0 * s.base)) + r.s1 * lv, r.s0 * s.s0, (0 + (r.s0 * s.s1)))

  val outdim = 2

  val right = IM(0, 1, 2)
  val left = IM(0, 2, 1)

  /*for (j <- 0 until outdim) {
    for (i <- 0 until 2) {
      //F2
      println(resolveH(right, i, j))
    }
    println("----")
  }


  println(" MIDDLE")
  for (j <- 0 until outdim) {
    for (i <- 0 until 2) {
      //F2
      println(resolveH(left, i, j))
    }
    println("----")
  }*/

  println("SIZE 8 ----------------------------------------")

  val outrh = IM(0, 1, 4)
  val outlh = IM(0, 1, 4)
  for (lk <- 0 until 2) {

    val fusedr = fuseIM(outrh, right, lk)
    for (j <- 0 until outdim) {

      for (i <- 0 until 2) {
        //F2
        println(resolveH(fusedr, i, j))
      }
      println("----")
    }


    println(" Inner MIDDLE")


    val fusedleft = fuseIM(outlh, left, lk)
    for (j <- 0 until outdim) {
      for (i <- 0 until 2) {
        //F2
        println(resolveH(fusedleft, i, j))
      }
      println("----")
    }

    println("XXX ITeration")
  }

  println(" MIDDLE")

  val f28 = IM(0, 4, 1)
  for (lk <- 0 until 4) {
    for (i <- 0 until 2) {
      //F2
      println(resolveH(f28, i, lk))
    }
  }


  println("VECTORS--------------------------------------")

  {
    val v = 2


    val outrhv = IM(0, 1, 4 / v)
    val outlhv = IM(0, 1, 4 / v)
    for (lk <- 0 until 2 / v) {

      val fusedr = fuseIM(outrh, right, lk)
      for (j <- 0 until outdim) {


        for (i <- 0 until 2) {
          //F2
          println(resolveH(fusedr, i, j))
        }
        println("----")
      }


      println(" Inner MIDDLE")


      val fusedleft = fuseIM(outlh, left, lk)
      for (j <- 0 until outdim) {
        for (i <- 0 until 2) {
          //F2
          println(resolveH(fusedleft, i, j))
        }
        println("----")
      }

      println("XXX ITeration")
    }


    println(" MIDDLE")

    val f28 = IM(0, 4, 1)
    for (lk <- 0 until 4/v ) {

      val lv = IM(0,1,v)
      val fuse = fuseIM(f28,lv,lk)
      for (lv <- 0 until v) {


        //F2
        println(" + " + resolveH(fuse, 0, lv))
        println(" - " + resolveH(fuse, 1, lv))


      }
    }
  }

}

