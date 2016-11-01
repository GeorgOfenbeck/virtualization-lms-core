package Filter

import scala.collection.mutable

/**
  * Created by rayda on 01-Nov-16.
  */
object Principles extends App{

  blocking(129,129,8,8)

  def blocking(x: Int, y: Int, blockingx: Int, blockingy: Int) = {

    def init (nRows: Int, nCols: Int) = Array.tabulate(nRows,nCols)( (x,y) => false )

    var a: Array[Array[Boolean]] = init(x,y)


    val iterations_x = x / blockingx //dyn
    val iterations_y = y / blockingy //dyn

    val rest_x = x % blockingx
    val rest_y = y % blockingy

    //main block
    for (i <- 0 until iterations_x)
      for (j <- 0 until iterations_y)
        {
          for (ii <- (i*blockingx) until (i*blockingx)+blockingx)
            for (jj <- (j*blockingy) until (j*blockingy)+blockingy)
              {
                a(ii)(jj) = true
              }
        }
    //bottom border
    for (r <- iterations_x*blockingx until x)
      for (c <- 0 until iterations_y)
        for (cb <- (c * blockingy) until (c* blockingy)+blockingy)
          {
            a(r)(cb) = true
          }


    //right border
    for (r <- 0 until iterations_x)
      for (rb <- (r*blockingx) until (r*blockingx)+blockingx)
        for (c <- (iterations_y*blockingy) until y)
          a(rb)(c) = true



       //bottom right border

       for (i <- (iterations_x * blockingx) until x)
         for (j <- (iterations_y * blockingy) until y)
           a(i)(j) = true



    val all = a.foldLeft(true){
      (acc,ele) => ele.foldLeft(acc){
        (acc2,ele2) => acc2 && ele2
      }
    }
    println(all)
  }


  case class Bla(x: Int)


  def prep(a: Int, b: Int, c: Int, d: Int) = {

    var map = scala.collection.immutable.HashMap.empty[Int, Bla]

    map = map + (0 -> Bla(a))
    map = map + (1 -> Bla(b))
    map = map + (2 -> Bla(c))
    map = map + (3 -> Bla(d))


    val reverse = map.foldLeft(scala.collection.immutable.HashMap.empty[Bla, Vector[Int]]){
      (acc,ele) => {
        if (acc.contains(ele._2)) {
          acc + (ele._2 -> (acc(ele._2) ++ Vector(ele._1)))
        } else
          acc + (ele._2 -> Vector(ele._1))
      }
    }

  }


  def symetry(a: Int, b: Int, c: Int, d: Int, pv: Map[Int,Int], vp: Map[Int,Vector[Int]]) = {

    val input = Vector(1,2,3,4)

    def take(i: Int): Int = i match{
      case 0 => a
      case 1 => b
      case 2 => c
      case 3 => d
      case _ => ???
    }
    vp.foldLeft(0){
      (acc,ele) => acc + take(ele._1) * (ele._2.foldLeft(0){
        (acc2,ele2) => acc2 + input(ele2)
      })
    }
  }

}
