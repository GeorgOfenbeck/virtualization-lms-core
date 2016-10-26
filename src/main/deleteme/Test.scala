import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._
import sort.MyComplex

object Bla extends org.scalacheck.Properties("Sort") {
  val min: Double = -100000.0
  val max: Double = min * -1.0
  val size = 10000
  val genPosVector = containerOfN[List, Double](size, Gen.posNum[Double])
  //val genPosVector = containerOfN[List, Double](size, Gen.chooseNum(min,max))
  //val gen = containerOfN[List, Double](size, Gen.chooseNum(min,max))
  val maxvalue = 2147483647
  val buckets = 32


  var uglyglobalj: Int = 0

  def maxd(cur: Int): Int = if (maxvalue / Math.pow(buckets, cur) > 1) maxd(cur + 1) else cur

  val maxdiv = maxd(0)

  //val maxdiv =   1000000000
  property("startsWith") = forAll(genPosVector) { l =>
    val v1 = l.toArray
    val v11 = l.toArray
    val v2 = l.toArray
    val v3 = l.toArray
    val v4 = l.toArray

    val c = new testClass
    val s = c(false, v1, 0, v1.length, 16)
    val s1 = c(true, v11, 0, v11.length, 16)
    //val s = if (v.size > 0) ref_poly(v,0,v.length) else v
    //val s = if (v.size > 0) ref_quicksort_poly(v,0,v.length) else v
    //val s2 = test(v,0,v.length)
    val s4 = Bla.ref_poly_wdispatch(v2, 0, v2.length)
    val s3 = sortFunctional(v3.toVector).toArray
    val s5 = if (v4.size > 0) Bla.ref_quicksort_r(v4, 0, v4.length) else v4
    //val s4 = msortfunctional(v.toVector)
    //val s5 = msd_radix_sort_head(v.toVector)
    s3.corresponds(s) {
      _ == _
    } && s3.corresponds(s4) {
      _ == _
    } && s3.corresponds(s5) {
      _ == _
    } && s1.corresponds(s) {
      _ == _
    }

  }

  def chooseBase(size: Int): Int = 0

  def chooseSort(size: Int): Int = 0

  def baseline(input: Vector[Int]): Vector[Int] = input.sortWith(_ < _)

  def baseline_complex(input: Array[MyComplex]): Array[MyComplex] = {
    def sortcomp(x: MyComplex, y: MyComplex): Boolean = x.cmp(y) < 0
    input.sortWith((x, y) => sortcomp(x, y))
  }

  def baseline(input: Array[Int]): Array[Int] = input.sortWith(_ < _)

  def merge(xs: Vector[Int], ys: Vector[Int]): Vector[Int] = {
    if (xs.isEmpty) ys
    else if (ys.isEmpty) xs
    else {
      (xs, ys) match {
        case (x +: xs1, y +: ys1) =>
          if (x > y)
            x +: merge(xs1, ys)
          else
            y +: merge(xs, ys1)
      }
    }
  }


  def merge(xs: Array[Double], ys: Array[Double]): Array[Double] = ???

  def merge(xs: Array[sort.MyComplex], ys: Array[sort.MyComplex]): Array[sort.MyComplex] = ???

  def merge(xs: Array[Int], ys: Array[Int]): Array[Int] = {
    if (xs.isEmpty) ys
    else if (ys.isEmpty) xs
    else {
      val size = xs.size + ys.size

      val retarray = new Array[Int](size)


      var i = 0
      var j = 0
      var k = 0

      while (i < xs.length && j < ys.length) {
        if (xs(i) < ys(i)) {
          retarray(k) = xs(i)
          k = k + 1
          i = i + i
        }
        else {
          retarray(k) = ys(i)
          k = k + 1
          j = j + i
        }
      }

      while (i < xs.length) {
        retarray(k) = xs(i)
        k = k + 1
        i = i + i
      }

      while (j < ys.length) {
        retarray(k) = ys(j)
        k = k + 1
        j = j + i
      }
      /*
      (xs, ys) match {
        case (x +: xs1, y +: ys1) =>
          if (x > y)
            x +: merge(xs1, ys)
          else
            y +: merge(xs, ys1)
      }
      */
      retarray
    }
  }


  def digitsplit(xs: Vector[Int], pos: Int): Vector[Vector[Int]] = {
    val div: Int = Math.pow(buckets, maxdiv - 1 - pos).toInt
    val tmpstore = new Array[Vector[Int]](buckets)
    for (i <- 0 until tmpstore.size) tmpstore(i) = Vector.empty
    val t = xs.foldLeft(tmpstore) {
      (acc, ele) => {
        val killright = (ele / div).toInt
        val key = killright % buckets
        tmpstore(key) = tmpstore(key) :+ ele
        tmpstore
      }
    }
    t.reverse.toVector

  }

  def msd_radix_sort(xs: Vector[Int], pos: Int): Vector[Int] = {
    if (pos == maxdiv || xs.size < 2) xs
    else {
      val vlist = digitsplit(xs, pos)
      val plus1 = pos + 1
      vlist.flatMap(l => msd_radix_sort(l, plus1))
    }
  }


  def msd_radix_sort_head(xs: Vector[Int]): Vector[Int] = msd_radix_sort(xs, 0)


  def msortfunctional(xs: Vector[Int]): Vector[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msortfunctional(ys), msortfunctional(zs))
    }
  }


  def sortFunctional(xs: Vector[Double]): Vector[Double] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      val less = xs.filter(p => pivot > p)
      val equal = xs.filter(p => pivot == p)
      val greater = xs.filter(p => pivot < p)
      //sortFunctional(greater) ++ equal ++ sortFunctional(less)
      sortFunctional(less) ++ equal ++ sortFunctional(greater)
    }
  }

  def ref_dispatch: ((Array[Double], Int, Int)) => ((Array[Double])) =
    (helper: ((Array[Double], Int, Int))) => {
      val xs: Array[Double] = helper._1
      val start: Int = helper._2
      val end: Int = helper._3
      if (end - start < 16)
        ref_insertioncore(xs, start, end)
      else
        ref_quicksort_poly(xs, start, end)
    }

  def ref_poly_wdispatch: ((Array[Double], Int, Int)) => ((Array[Double])) =
    (helper: ((Array[Double], Int, Int))) => {
      val xs: Array[Double] = helper._1
      val start: Int = helper._2
      val end: Int = helper._3
      ref_dispatch(xs, start, end)
    }


  def ref_poly: ((Array[Double], Int, Int)) => ((Array[Double])) =
    (helper: ((Array[Double], Int, Int))) => {
      val xs: Array[Double] = helper._1
      val start: Int = helper._2
      val end: Int = helper._3
      if (end - start < 2)
        ref_insertioncore(xs, start, end)
      else
        ref_quicksort_poly(xs, start, end)
    }

  def ref_quicksort_poly: ((Array[Double], Int, Int)) => ((Array[Double])) =
    (helper: ((Array[Double], Int, Int))) => {
      val xs: Array[Double] = helper._1
      val start: Int = helper._2
      val end: Int = helper._3
      def swap(i: Int, j: Int) {
        val t = xs(i);
        xs(i) = xs(j);
        xs(j) = t
      }
      def sort1(l: Int, r: Int) {
        val pivot = xs((l + r) / 2)
        var i = l;
        var j = r
        while (i <= j) {
          while (xs(i) < pivot) i += 1
          while (xs(j) > pivot) j -= 1
          if (i <= j) {
            swap(i, j)
            i += 1
            j -= 1
          }
        }
        if (l < j) ref_poly(xs, l, j + 1)
        if (j < r) ref_poly(xs, i, r + 1)
      }
      sort1(start, end - 1)
      xs
    }


  //this is the one we use within the genereated code
  def ref_quicksort(xs: Array[Double], start: Int, end: Int, pivotidx: Int): Array[Double] = {
    def swap(i: Int, j: Int) {
      val t = xs(i);
      xs(i) = xs(j);
      xs(j) = t
    }
    def sort1(l: Int, r: Int, pivotele: Int) {
      val pivot = xs(pivotele)
      var i = l;
      var j = r
      while (i <= j) {
        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      uglyglobalj = j

    }
    sort1(start, end - 1, pivotidx)
    xs
  }

  def ref_quicksort_r(xs: Array[Double], start: Int, end: Int): Array[Double] = {
    def swap(i: Int, j: Int) {
      val t = xs(i);
      xs(i) = xs(j);
      xs(j) = t
    }
    def sort1(l: Int, r: Int) {
      val pivot = xs((l + r) / 2)
      var i = l;
      var j = r
      while (i <= j) {
        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort1(l, j)
      if (j < r) sort1(i, r)
    }
    sort1(start, end - 1)
    xs
  }


  def ref_insertioncore(a: Array[Double], start: Int, end: Int): Array[Double] = {
    for (i <- start + 1 until end) {
      // A[ i ] is added in the sorted sequence A[0, .. i-1]
      // save A[i] to make a hole at index iHole
      val item = a(i)
      var iHole = i
      // keep moving the hole to next smaller index until A[iHole - 1] is <= item
      while (iHole > start && a(iHole - 1) > item) {
        // move hole to next smaller index
        a(iHole) = a(iHole - 1)
        iHole = iHole - 1
      }
      // put item in the hole
      a(iHole) = item
    }
    a
  }

  def insertioncore[T: Ordering](acc: Array[T], ele: Int): Array[T] = {
    /*val orderingev = implicitly[Ordering[T]]
    val currele = acc(ele)
    val (sorted, rest) = acc.splitAt(ele)
    val bigger = sorted.takeWhile(p => orderingev.gt(p,currele))
    val smaller = sorted.drop(bigger.size)

    (bigger :+ rest.head) ++ smaller ++ rest.tail*/
    acc
  }

  /*def insertioncore(acc: Array[sort.MyComplex], ele: Int): Array[sort.MyComplex] = {
    //def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {
    val currele = acc(ele)
    val (sorted, rest) = acc.splitAt(ele)
    val bigger = sorted.takeWhile(p => p.cmp(currele) > 0)
    val smaller = sorted.drop(bigger.size)
    (bigger :+ rest.head) ++ smaller ++ rest.tail
  }


  def insertioncore(acc: Array[Int], ele: Int): Array[Int] = {
    //def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {
    val currele = acc(ele)
    val (sorted, rest) = acc.splitAt(ele)
    val bigger = sorted.takeWhile(p => p > currele)
    val smaller = sorted.drop(bigger.size)
    (bigger :+ rest.head) ++ smaller ++ rest.tail
  }*/

  def inserationsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {
    if (start < end && (end - start) > 1) {
      (start + 1 until end).foldLeft(v) {
        (acc, ele) => insertioncore(acc.toArray, ele).toVector
      }
    } else {
      v
    }


  }


  def selectionsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {

    (start until end).foldLeft(v) {
      (acc, ele) => {
        val swapele = acc(ele)
        val (value, pos) = (ele until end).foldLeft((swapele, ele)) {
          (acc2, k) => {
            val (value, pos) = acc2
            val currcheck = acc(k)
            if (swapele < currcheck)
              (currcheck, k)
            else
              (value, pos)
          }
        }
        val bx = acc(pos)
        val o1 = acc.updated(pos, swapele)
        val o2 = o1.updated(ele, value)
        o2
      }
    }
  }
}
//bla!
/*****************************************
  Emitting Generated Code                  
*******************************************/
class testClass extends (((Boolean,Array[Double],Int,Int,Int))=> ((Array[Double]))) {
def apply( helper: ((Boolean,Array[Double],Int,Int,Int))): ((Array[Double])) = {
val x1 : Boolean = helper._1
val x2 : Array[Double] = helper._2
val x3 : Int = helper._3
val x4 : Int = helper._4
val x5 : Int = helper._5
val x276 = if (x1) {
val x216 =  Sort_x6(x2, x3, x4, x5)

val x217 = x216
(x217)
} else {
val x273 =  Sort_x220(x2, x3, x4, x5)

val x274 = x273
(x274)
}
val x277 = x276

 (x277)
}
/*****************************************
  End Main                  
*******************************************/
def Sort_x220: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x221 : Array[Double] = helper._1
val x222 : Int = helper._2
val x223 : Int = helper._3
val x224 : Int = helper._4
val x270 =  Dispatch_x226(x221, x222, x223, x224)

val x271 = x270

 (x271)
}
def InserationSort_x25: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x26 : Array[Double] = helper._1
val x27 : Int = helper._2
val x28 : Int = helper._3
val x29 : Int = helper._4
val x30 = Bla.ref_insertioncore(x26 , x27 , x28)

 (x30)
}
def QuickSort_x184: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x185 : Array[Double] = helper._1
val x186 : Int = helper._2
val x187 : Int = helper._3
val x188 : Int = helper._4
val x189 = x187 - x186
val x190 = x189 / 2
val x191 = x186 + x190
val x192 = Bla.ref_quicksort(x185 , x186 , x187 , x191)
val x193 = Bla.uglyglobalj
val x194 = x193 + 1
val x196 =  Sort_x6(x192, x186, x194, x188)

val x197 = x196
val x199 =  Sort_x6(x197, x193, x187, x188)

val x200 = x199

 (x200)
}
def QuickSort_x78: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x79 : Array[Double] = helper._1
val x80 : Int = helper._2
val x81 : Int = helper._3
val x82 : Int = helper._4
val x83 = x81 - x80
val x84 = x83 / 2
val x85 = x80 + x84
val x86 = Bla.ref_quicksort(x79 , x80 , x81 , x85)
val x87 = Bla.uglyglobalj
val x88 = x87 + 1
val x125 =  Sort_x89(x86, x80, x88, x82)

val x126 = x125
val x128 =  Sort_x89(x126, x87, x81, x82)

val x129 = x128

 (x129)
}
def Dispatch_x164: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x165 : Array[Double] = helper._1
val x166 : Int = helper._2
val x167 : Int = helper._3
val x168 : Int = helper._4
val x169 = x167 - x166
val x170 = x169 < 16
val x205 = if (x170) {
val x180 =  InserationSort_x173(x165, x166, x167, x168)

val x181 = x180
(x181)
} else {
val x202 =  QuickSort_x184(x165, x166, x167, x168)

val x203 = x202
(x203)
}
val x206 = x205

 (x206)
}
def Sort_x89: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x90 : Array[Double] = helper._1
val x91 : Int = helper._2
val x92 : Int = helper._3
val x93 : Int = helper._4
val x94 = x92 - x91
val x95 = x94< 100
val x122 = if (x95) {
val x114 =  Dispatch_x98(x90, x91, x92, x93)

val x115 = x114
(x115)
} else {
val x119 =  Dispatch_x58(x90, x91, x92, x93)

val x120 = x119
(x120)
}
val x123 = x122

 (x123)
}
def Sort_x6: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x7 : Array[Double] = helper._1
val x8 : Int = helper._2
val x9 : Int = helper._3
val x10 : Int = helper._4
val x11 = x9 - x8
val x12 = x11< 100
val x211 = if (x12) {
val x160 =  Dispatch_x15(x7, x8, x9, x10)

val x161 = x160
(x161)
} else {
val x208 =  Dispatch_x164(x7, x8, x9, x10)

val x209 = x208
(x209)
}
val x212 = x211

 (x212)
}
def InserationSort_x173: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x174 : Array[Double] = helper._1
val x175 : Int = helper._2
val x176 : Int = helper._3
val x177 : Int = helper._4
val x178 = Bla.ref_insertioncore(x174 , x175 , x176)

 (x178)
}
def Sort_x49: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x50 : Array[Double] = helper._1
val x51 : Int = helper._2
val x52 : Int = helper._3
val x53 : Int = helper._4
val x54 = x52 - x51
val x55 = x54< 100
val x145 = if (x55) {
val x137 =  Dispatch_x58(x50, x51, x52, x53)

val x138 = x137
(x138)
} else {
val x142 =  Dispatch_x15(x50, x51, x52, x53)

val x143 = x142
(x143)
}
val x146 = x145

 (x146)
}
def Dispatch_x98: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x99 : Array[Double] = helper._1
val x100 : Int = helper._2
val x101 : Int = helper._3
val x102 : Int = helper._4
val x111 =  InserationSort_x104(x99, x100, x101, x102)

val x112 = x111

 (x112)
}
def Dispatch_x226: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x227 : Array[Double] = helper._1
val x228 : Int = helper._2
val x229 : Int = helper._3
val x230 : Int = helper._4
val x231 = x229 - x228
val x232 = x231 < 16
val x267 = if (x232) {
val x242 =  InserationSort_x235(x227, x228, x229, x230)

val x243 = x242
(x243)
} else {
val x264 =  QuickSort_x246(x227, x228, x229, x230)

val x265 = x264
(x265)
}
val x268 = x267

 (x268)
}
def InserationSort_x67: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x68 : Array[Double] = helper._1
val x69 : Int = helper._2
val x70 : Int = helper._3
val x71 : Int = helper._4
val x72 = Bla.ref_insertioncore(x68 , x69 , x70)

 (x72)
}
def InserationSort_x104: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x105 : Array[Double] = helper._1
val x106 : Int = helper._2
val x107 : Int = helper._3
val x108 : Int = helper._4
val x109 = Bla.ref_insertioncore(x105 , x106 , x107)

 (x109)
}
def Dispatch_x58: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x59 : Array[Double] = helper._1
val x60 : Int = helper._2
val x61 : Int = helper._3
val x62 : Int = helper._4
val x63 = x61 - x60
val x64 = x63 < 16
val x134 = if (x64) {
val x74 =  InserationSort_x67(x59, x60, x61, x62)

val x75 = x74
(x75)
} else {
val x131 =  QuickSort_x78(x59, x60, x61, x62)

val x132 = x131
(x132)
}
val x135 = x134

 (x135)
}
def InserationSort_x235: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x236 : Array[Double] = helper._1
val x237 : Int = helper._2
val x238 : Int = helper._3
val x239 : Int = helper._4
val x240 = Bla.ref_insertioncore(x236 , x237 , x238)

 (x240)
}
def QuickSort_x246: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x247 : Array[Double] = helper._1
val x248 : Int = helper._2
val x249 : Int = helper._3
val x250 : Int = helper._4
val x251 = x249 - x248
val x252 = x251 / 2
val x253 = x248 + x252
val x254 = Bla.ref_quicksort(x247 , x248 , x249 , x253)
val x255 = Bla.uglyglobalj
val x256 = x255 + 1
val x258 =  Sort_x220(x254, x248, x256, x250)

val x259 = x258
val x261 =  Sort_x220(x259, x255, x249, x250)

val x262 = x261

 (x262)
}
def QuickSort_x36: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x37 : Array[Double] = helper._1
val x38 : Int = helper._2
val x39 : Int = helper._3
val x40 : Int = helper._4
val x41 = x39 - x38
val x43 = x41 / 2
val x44 = x38 + x43
val x45 = Bla.ref_quicksort(x37 , x38 , x39 , x44)
val x46 = Bla.uglyglobalj
val x48 = x46 + 1
val x148 =  Sort_x49(x45, x38, x48, x40)

val x149 = x148
val x151 =  Sort_x49(x149, x46, x39, x40)

val x152 = x151

 (x152)
}
def Dispatch_x15: ((Array[Double],Int,Int,Int)) => ((Array[Double])) = 
(helper: ((Array[Double],Int,Int,Int))) =>{
val x16 : Array[Double] = helper._1
val x17 : Int = helper._2
val x18 : Int = helper._3
val x19 : Int = helper._4
val x20 = x18 - x17
val x22 = x20 < 16
val x157 = if (x22) {
val x32 =  InserationSort_x25(x16, x17, x18, x19)

val x33 = x32
(x33)
} else {
val x154 =  QuickSort_x36(x16, x17, x18, x19)

val x155 = x154
(x155)
}
val x158 = x157

 (x158)
}
//bla end!

}

