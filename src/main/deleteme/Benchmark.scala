/**
  * Created by rayda on 17-Oct-16.
  */

/*

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._

object Benchmark extends App{

  val min: Double = -100000.0
  val max: Double = min * -1.0

  for (size <- 500 until 501) {
    for (warmup <- 0 until 100) {
      val gen = containerOfN[List, Double](size*2, Gen.chooseNum(min,max))
      val l = gen.sample.get
      val a = l.toArray
      /*val comparray = new Array[sort.MyComplex](size)
      for (i <- 0 until size)
        comparray(i) = sort.MyComplex(a(i), a(i+1))*/
      val v = l.toVector
      val c = new testClass

      //val s2 = test(v,0,v.length)
      //val t1 = System.currentTimeMillis()
      //val s3 = Bla.sortFunctional(v)


      val t0 = System.nanoTime()

      val s4 = Bla.ref_quicksort(a,0,a.length-1)
      //val s4 = Bla.baseline_complex(a)
      //val s4 = Bla.msortfunctional(v)
      val t1 = System.nanoTime()

      //val s5 = c(a, 0, a.length, 16)
      val s5 = c(a, 16)
      val t2 = System.nanoTime()
      //val s6 = Bla.baseline_complex(comparray)
      val s6 = Bla.ref_quicksort(a,0,a.length-1)
      val t3 = System.nanoTime()

      val time_function = t1 - t0
      val time_mine = t2 - t1
      val time_base = t3 - t2

      val smallest = Math.min(Math.min(time_base,time_function),time_mine)

      println("func - mine - base: " + time_function/smallest + " - " + time_mine/smallest + " - " + time_base/smallest)
    }


  }
}
*/
