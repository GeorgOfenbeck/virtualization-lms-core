/**
  * Created by rayda on 19-Oct-16.
  */

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._

object Size500Benchmark extends App{

  val min: Double = -100000.0
  val max: Double = min * -1.0

  val repeats:Int = 100
  val report: Int = 98

  val timings1 = new Array[Double](repeats)
  val timings2 = new Array[Double](repeats)
  val timings3 = new Array[Double](repeats)
  val timings4 = new Array[Double](repeats)
  val timings5 = new Array[Double](repeats)
  var compilerstop: Double = 0

  val sizes = Vector(4, 8, 10, 16, 25, 32, 50, 64, 90, 128, 200, 256, 333, 512, 729, 1024, 1389, 2048, 3555, 4096, 7777, 8192, 13333, 16384, 20000, 32768, 47000, 65536, 99999, 131072, 191072, 262144, 524288, 1048576)

  def sort(x: Array[Double]): Array[Double] = x.sorted
  val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\results.txt"))
  for (size <- sizes) {
    //val size = Math.pow(2,pow).toInt
    //val gen = containerOfN[List, Double](size, Gen.chooseNum(min,max))
    val gen = containerOfN[List, Double](size,Gen.posNum[Double])
    val l = gen.sample.get

    for (warmup <- 0 until repeats) {


      val l1 = l.toArray
      val l2 = l.toArray
      val l3 = l.toArray
      val l4 = l.toArray
      val l5 = l.toArray
      //val a = l.toArray
      /*val comparray = new Array[sort.MyComplex](size)
      for (i <- 0 until size)
        comparray(i) = sort.MyComplex(a(i), a(i+1))*/



      //val s2 = test(v,0,v.length)
      //val t1 = System.currentTimeMillis()
      //val s3 = Bla.sortFunctional(v)


      val t0 = System.nanoTime()
      //val s1 = Bla.ref_poly(l1,0,l1.length)
      val s1 = sort(l1)
      //val s4 = Bla.ref_quicksort(a,0,a.length-1)
      //val s4 = Bla.baseline_complex(a)
      //val s4 = Bla.msortfunctional(v)
      val t1 = System.nanoTime()

      //val s5 = c(l2, 0, l2.length, 16)
      //val s7 = Bla.ref_poly(l2,0,l2.length-1)
      val c = new testClass
      val s2 = c(false,l2, 0, l2.length, 16)
      //val s2 = c(l2, 0, l2.length,16)
      //val s2 = Bla.ref_quicksort_r(l2,0,l2.length)
      //val s5 = c(a, 0, a.length-1,16)
      val t2 = System.nanoTime()
      //val s6 = Bla.baseline_complex(comparray)
      val s3 = Bla.ref_quicksort_r(l3,0,l3.length)
      //val s3 = Bla.ref_poly(l3,0,l3.length)
      val t3 = System.nanoTime()

      val s4 = Bla.ref_poly_wdispatch(l4,0,l4.length)
      //val s4 = Bla.ref_quicksort_r(l4,0,l4.length)
      val t4 = System.nanoTime()
      val s5 = c(true,l5, 0, l5.length, 16)
      val t5 = System.nanoTime()


      val time_function = t1 - t0
      val time_mine = t2 - t1
      val time_base = t3 - t2
      val time_mine2 = t4 - t3
      val time_inline = t5 - t4
      
      //val smallest = Math.min(Math.min(Math.min(Math.min(time_base,time_function),time_mine),time_mine2),time_inline)
      val smallest = time_function

      compilerstop = compilerstop + s1(warmup%size) + s2(warmup%size) + s3(warmup%size) + s4(warmup%size) + s5(warmup%size)

      val f1 = (1.0*time_function/smallest)
      val f2 = (1.0*time_mine/smallest)
      val f3 = (1.0*time_base/smallest)
      val f4 = (1.0*time_mine2/smallest)
      val f5 = (1.0*time_inline/smallest)
      
      timings1(warmup) = f1
      timings2(warmup) = f2
      timings3(warmup) = f3
      timings4(warmup) = f4
      timings5(warmup) = f5
      
      if (warmup > 0 && warmup % report == 0)
        {
          val f11 = timings1.slice(warmup-report,warmup).sum/report
          val f22 = timings2.slice(warmup-report,warmup).sum/report
          val f33 = timings3.slice(warmup-report,warmup).sum/report
          val f44 = timings4.slice(warmup-report,warmup).sum/report
          val f55 = timings5.slice(warmup-report,warmup).sum/report
          println(f"ref jdk - mine - ref quick only - ref poly alg w dispatch - inline: $f11%1.1f - $f22%1.1f - $f33%1.1f - $f44%1.1f - $f55%1.1f ==== $size" )
        }

    }
    /*val f11 = timings1.sum/timings1.size
    val f22 = timings2.sum/timings1.size
    val f33 = timings3.sum/timings1.size
    val f44 = timings4.sum/timings1.size
    val f55 = timings5.sum/timings1.size*/
    val f11 = {
      val z = (timings1.sorted)
      z(timings1.length/2)
    }
    val f22 = {
      val z = (timings2.sorted)
      z(timings1.length/2)
    }
    val f33 = {
      val z = (timings3.sorted)
      z(timings1.length/2)
    }
    val f44 = {
      val z = (timings4.sorted)
      z(timings1.length/2)
    }
    val f55 = {
      val z = (timings5.sorted)
      z(timings1.length/2)
    }
    println(f"ref jdk - mine - ref quick only - ref poly alg w dispatch - inline: $f11%1.1f - $f22%1.1f - $f33%1.1f - $f44%1.1f - $f55%1.1f ==== $size" )
    stream2.println(f"$size;$f11%1.2f;$f22%1.2f;$f33%1.2f;$f44%1.2f;$f55%1.2f;")
    println(compilerstop)
  }
  stream2.flush()
  stream2.close()
}