

package sort


import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class Core extends Skeleton {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }



  def sort[A[_], B[_], C[_], AB[_]](stat: StatHeader[A, B, C, AB]): StagedFunction[DynHeader[A, B, C, AB], Rep[Vector[Int]]] = {
    val sstring = stat.genSig()
    val exposarg: ExposeRep[DynHeader[A, B, C, AB]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]
    val stageme: (DynHeader[A, B, C, AB] => Rep[Vector[Int]]) = (dyn: DynHeader[A, B, C, AB]) => {
      val mix = MixSortHeader(stat, dyn)
      import mix._
      import lub1._
      import evab._
      val size = minus(end, start)
      val ret = _if(less(size, const(10)), {
        {
          val ret = _if(equiv(choose_algorithm(size), const(0)), {
            val f = inserationsort(stat)
            f(dyn)
          },{
            val f = selectionsort(stat)
            f(dyn)
          }
          )
          ret
        }
      },
        {
          val ret = _if(equiv(choose_algorithm(size), const(0)), {
            val f = quicksort(stat)
            f(dyn)
          },{
            val f = mergesort(stat)
            f(dyn)
          }
          )
         ret
        }
      )
      ret
    }

    val t: StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("Sort" + sstring),Some("Sort"))(exposarg, exposeret)
    t

  }


  def selectionsort[A[_],B[_],C[_], AB[_]](stat: StatHeader[A,B,C,AB]): StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A,B,C,AB]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]

    val stageme: (DynHeader[A,B,C,AB] => Rep[Vector[Int]]) = (dyn: DynHeader[A,B,C,AB]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      import lub1._
      implicit val itupexpose = exposeTuple[Int, Int]()
      evab.rangefold(evab.unt(start,end),dyn.x,exposeret) {
        case (acc, ele) => inserationcore(acc, evab.toRep(ele))
      }
      /*(start until end).foldLeft(x) {
        case (array, index) => {
          val swapele = array(index)
          val (value, pos) = ((index) until end).foldLeft((swapele, index.toRep())) {
            case ((value, pos), index2) => {
              val b = array(index2)
              val t: Rep[Boolean] = value < b
              myifThenElse[(Rep[Int], Rep[Int])](t, (b, index2.toRep()), (value, pos))
            }
          }
          val bx = array(pos)
          array.update(index, bx).update(pos, swapele)
        }
      }*/
    }
    val t: StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("SelectionSort" + stat.genSig()),Some("SelectionSort"))(exposarg,exposeret )
    t
  }



  def inserationsort[A[_],B[_],C[_], AB[_]](stat: StatHeader[A,B,C,AB]): StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A,B,C,AB]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]
    val stageme: (DynHeader[A,B,C,AB] => Rep[Vector[Int]]) = (dyn: DynHeader[A,B,C,AB]) => {
      val mix = MixSortHeader(stat, dyn)
      import mix._
      import lub1._
      import evab._
      val size = minus(end, start)
      val ret: Rep[Vector[Int]] = evab._if(equiv(size, const(1)), dyn.x, {
        val one = const(1)
        val s1 = plus(start, one)
        rangefold(unt(s1, end),dyn.x,exposeret) {
          case (acc, ele) => inserationcore(acc, toRep(ele))
        }
      })
      ret
    }
    val t: StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("InserationSort" + stat.genSig()),Some("InserationSort"))(exposarg,exposeret )
    t
  }

  def mergesort[A[_],B[_],C[_], AB[_]](stat: StatHeader[A,B,C,AB]): StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A,B,C,AB]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]

    val stageme: (DynHeader[A,B,C,AB] => Rep[Vector[Int]]) = (dyn: DynHeader[A,B,C,AB]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      import evab._
      import lub1._
      val fsize = minus(end, start)
      val half = div(fsize,const(2))

      val low_start = sh.start
      val low_end = half
      val high_start = plus(half,const(1))
      val high_end = sh.end

      val lessmix = MixSortHeader.apply[A,AB,C,AB](x,low_start,low_end,basesize)(sh.eva,sh.evab,sh.evc,sh.evab,lub3,lub4,lub3,lub4)
      val (statless,dynless) = lessmix.split()
      val lessf = sort(statless)
      val xl = lessf(dynless)
      val highmix = MixSortHeader.apply[AB,B,C,AB](x, high_start, high_end, basesize)(sh.evab,sh.evb,sh.evc,sh.evab,lub2,lub2,lub4,lub4)
      val (stathigh,dynhigh) = highmix.split()
      val highf = sort(stathigh)
      val xh = highf(dynhigh)

      merge(xl, xh)
    }
    val t: StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("MergeSort" + stat.genSig()),Some("MergeSort"))(exposarg,exposeret )
    t
  }


  def quicksort[A[_],B[_],C[_], AB[_]](stat: StatHeader[A,B,C,AB]): StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A,B,C,AB]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]

    val stageme: (DynHeader[A,B,C,AB] => Rep[Vector[Int]]) = (dyn: DynHeader[A,B,C,AB]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      import lub1._
      import evab._

      val fsize = minus(end, start)
      val half = div(fsize,const(2))
      val pivot = x(toRep(half))
      val less = filter[Int](x, p => pivot > p)
      val equal = filter[Int](x, p => pivot equiv p)
      val greater = filter[Int](x, p => pivot < p)

      val lesssize = size(less)
      val greatersize = size(greater)
      val zz: NoRep[Int] = 0
      val lessmix = MixSortHeader[NoRep,Rep,C,Rep](less,zz,lesssize,basesize)(cNoRep,cRep,evc,cRep,NoRepRep,RepRep,NoRepRep,RepRep)
      val greatermix = MixSortHeader[NoRep,Rep,C,Rep](greater,zz,greatersize,basesize)(cNoRep,cRep,evc,cRep,NoRepRep,RepRep,NoRepRep,RepRep)
      val (statless,dynless) = lessmix.split()
      val lessf = sort(statless)
      val xl = lessf(dynless)

      val (statgreater,dyngreater) = greatermix.split()
      val greaterf = sort(statgreater)
      val xg = greaterf(dyngreater)

      concat(concat(xl, equal), xg)

    }
    val t: StagedFunction[DynHeader[A,B,C,AB], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("QuickSort" + stat.genSig()),Some("QuickSort"))(exposarg,exposeret )
    t
  }



  def tmp[A[_],B[_],C[_], AB[_]](stat: StatHeader[A,B,C,AB]): (DynHeader[A,B,C,AB] => Rep[Vector[Int]]) = {
    val outer: (DynHeader[A,B,C,AB] => Rep[Vector[Int]]) = (dyn: DynHeader[A,B,C,AB]) => {
      val f = sort(stat)
      f(dyn)
    }
    outer
  }




  def graphvizexport() = {
    val ini: StatHeader[NoRep,NoRep,NoRep,NoRep] = StatHeader(None, None, None)
    //val ini: StatSelectionHeader = StatSelectionHeader(None, None, None)
    val (code, cm) = emitGraph.emitDepGraphf(tmp(ini))(exposeDynHeader(ini), exposeRepFromRep[Vector[Int]])
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    //stream.println(code)
    stream.flush()
    stream.close()
  }

  def codeexport() = {
    val ev: IRep[Rep] = cRep
    //val ini: StatHeader[Rep,Rep,Rep] = StatHeader.apply[Rep,Rep,Rep](Const(1),Const(1),Const(1))
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println("import org.scalacheck._\nimport org.scalacheck.Properties\nimport org.scalacheck.Prop.forAll\nimport org.scalacheck.Gen._\n\nobject Bla extends org.scalacheck.Properties(\"Sort\") {\n\n  val genPosVector = containerOf[List,Int](Gen.posNum[Int])\n  val maxvalue = 2147483647\n  val buckets = 32\n\n  def maxd(cur: Int): Int = if (maxvalue / Math.pow(buckets,cur) > 1) maxd(cur + 1) else cur\n  val maxdiv = maxd(0)\n\n  //val maxdiv =   1000000000\n  property(\"startsWith\") = forAll(genPosVector) { l =>\n    val v = l.toVector\n    val c = new testClass\n    val s = c(v, 0, v.length, 16)\n    //val s2 = test(v,0,v.length)\n    val s3 = sortFunctional(v)\n    val s4 = msortfunctional(v)\n    val s5 = msd_radix_sort_head(v)\n    s3.corresponds(s) {\n      _ == _\n    } && s3.corresponds(s4) {\n      _ == _\n    }&& s3.corresponds(s5) {\n      _ == _\n    }\n\n  }\n\n  def merge(xs: Vector[Int], ys: Vector[Int]): Vector[Int] = {\n    if (xs.isEmpty) ys\n    else if (ys.isEmpty) xs\n    else {\n      (xs, ys) match {\n        case (x +: xs1, y +: ys1) =>\n          if (x > y)\n            x +: merge(xs1, ys)\n          else\n            y +: merge(xs, ys1)\n      }\n    }\n  }\n\n\n\n\n\n\n  def digitsplit(xs: Vector[Int], pos: Int): Vector[Vector[Int]] = {\n    val div:Int  = Math.pow(buckets,maxdiv-1-pos).toInt\n    val tmpstore = new Array[Vector[Int]](buckets)\n    for (i <- 0 until tmpstore.size) tmpstore(i) = Vector.empty\n    val t = xs.foldLeft(tmpstore){\n      (acc,ele) => {\n        val killright = (ele / div).toInt\n        val key = killright % buckets\n        tmpstore(key) = tmpstore(key) :+ ele\n        tmpstore\n      }\n    }\n    t.reverse.toVector\n\n  }\n\n  def msd_radix_sort(xs: Vector[Int], pos: Int): Vector[Int] = {\n    if (pos == maxdiv || xs.size < 2) xs\n    else {\n      val vlist = digitsplit(xs, pos)\n      val plus1 = pos + 1\n      vlist.flatMap(l => msd_radix_sort(l, plus1))\n    }\n  }\n\n\n  def msd_radix_sort_head(xs: Vector[Int]): Vector[Int] = msd_radix_sort(xs,0)\n\n\n  def msortfunctional(xs: Vector[Int]): Vector[Int] = {\n    val n = xs.length / 2\n    if (n == 0) xs\n    else {\n      val (ys, zs) = xs splitAt n\n      merge(msortfunctional(ys), msortfunctional(zs))\n    }\n  }\n\n\n  def sortFunctional(xs: Vector[Int]): Vector[Int] = {\n    if (xs.length <= 1) xs\n    else {\n      val pivot = xs(xs.length / 2)\n      val less = xs.filter(p => pivot > p)\n      val equal = xs.filter(p => pivot == p)\n      val greater = xs.filter(p => pivot < p)\n      sortFunctional(greater) ++ equal ++ sortFunctional(less)\n    }\n  }\n\n\n  def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {\n    val currele = acc(ele)\n    val (sorted, rest) = acc.splitAt(ele)\n    val bigger = sorted.takeWhile(p => p > currele)\n    val smaller = sorted.drop(bigger.size)\n    (bigger :+ rest.head) ++ smaller ++ rest.tail\n  }\n\n  def inserationsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n    if (start < end && (end - start) > 1) {\n      (start + 1 until end).foldLeft(v) {\n        (acc, ele) => insertioncore(acc,ele)\n      }\n    } else {\n      v\n    }\n\n\n  }\n\n\n  def selectionsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n\n    (start until end).foldLeft(v) {\n      (acc, ele) => {\n        val swapele = acc(ele)\n        val (value, pos) = (ele until end).foldLeft((swapele, ele)) {\n          (acc2, k) => {\n            val (value, pos) = acc2\n            val currcheck = acc(k)\n            if (swapele < currcheck)\n              (currcheck, k)\n            else\n              (value, pos)\n          }\n        }\n        val bx = acc(pos)\n        val o1 = acc.updated(pos, swapele)\n        val o2 = o1.updated(ele, value)\n        o2\n      }\n    }\n  }\n\n}")
    //val esc = codegen.emitSource(tmp(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[Vector[Int]])
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}


