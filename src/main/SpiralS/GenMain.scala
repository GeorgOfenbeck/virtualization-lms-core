
package SpiralS

/**
  * Georg Ofenbeck
  * First created:
  * Date: 03/06/2016
  * Time: 13:09 
  */

class ComplexVector

class Complex

case class IMT(base: Int, strides: Vector[Int]) {

  def fuseIM(r: IMT, s: IMT): IMT = {
    val ss0 = r.strides.headOption.getOrElse(0) //ivecapply(r.strides, Const(0))
    val fbase = r.base + ss0 * s.base
    val fstrides = Vector(r.strides.headOption.getOrElse(0) * s.strides.headOption.getOrElse(0)) ++ r.strides.zipAll(s.strides, 0, 0).map(p => p._1 + r.strides.headOption.getOrElse(0) * p._2)
    IMT(fbase, fstrides)
  }
}

object VectorMult {
  def apply(base: Int, strides: Vector[Int], loopvars: Vector[Int]): Int = {
    val t = loopvars.reverse.zip(strides)
    val r = base + t.foldLeft(0)({ (acc, ele) => acc + (ele._1 * ele._2) })
    r
  }
}


  object GenMain extends App {

    def testIM(im: IMT, loopvars: Vector[Int]): Int = {
      VectorMult(im.base,im.strides,loopvars)
    }

    def f(j: Int, i: Int) = 1 * j + 2 * i
    def g(k: Int, i: Int) = 1 * k + 2 * i

    val dsl = new NewCore
    dsl.codeexport()

    /*val outer = IMT(0,Vector(2,1))
    val inner = IMT(0,Vector(2,1))
    val fused = IMT(0, Vector(4,2,1))
    println(inner.fuseIM(outer,inner))
*/

    /*for (k <- 0 until 2)
      for (j <- 0 until 2)
        for (i<- 0 until 2) {
          //println(testIM(inner,Vector(i,j,k)))
          println(f(j, i))
          println(g(k, i))
          //println(testIM(fused,Vector(i,j,k)))
        }*/
  }

