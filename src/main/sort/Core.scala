

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

  case class MaybeSFunction[G, A[_], B[_], C[_], AB[_]](f: Either[StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]], DynHeader[G, A, B, C, AB] => Rep[Array[G]]]) {
    def apply(dyn: DynHeader[G, A, B, C, AB]): Rep[Array[G]] = f.fold(fa => fa(dyn), fb => fb(dyn))
  }

  object MaybeSFunction {
    def apply[G, A[_], B[_], C[_], AB[_]](f: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]]): MaybeSFunction[G, A, B, C, AB] = MaybeSFunction(Left(f))

    def apply[G, A[_], B[_], C[_], AB[_]](f: DynHeader[G, A, B, C, AB] => Rep[Array[G]]): MaybeSFunction[G, A, B, C, AB] = MaybeSFunction(Right(f))
  }

  case class MaybeFCompare[G](f: Either[StagedFunction[(Rep[G], Rep[G]), Rep[Int]], ((Rep[G], Rep[G])) => Rep[Int]]) {
    def apply(g: (Rep[G], Rep[G])): Rep[Int] = f.fold(fa => fa(g), fb => fb(g))
  }


  def compare[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeFCompare[G] = {
    if (stat.inline.compareinline) {
      MaybeFCompare[G](Right(stat.comp))
    }
    else {
      implicit val gmf = (stat.gtyp.mf)
      implicit val gtupexpose = exposeTuple[G, G]()
      val t: StagedFunction[(Rep[G], Rep[G]), Rep[Int]] = doGlobalLambda(stat.comp, Some("Compare"), Some("Compare"))
      MaybeFCompare[G](Left(t))
    }
  }

  def dispatcher[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeSFunction[G, A, B, C, AB] = {
    val sstring = stat.genSig()
    val exposarg: ExposeRep[DynHeader[G, A, B, C, AB]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Array[G]](stat.vtyp)
    val stageme: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val mix = MixSortHeader(stat, dyn)
      import mix._
      import lub1._
      import evab._
      val size = minus(end, start)
      val ret = if (inline.maxfunctions < 1) {
        _if(equiv(choose_algorithm(size), const(0)), {
          val f = inserationsort(stat)
          f(dyn)
        }, {
          val f = selectionsort(stat)
          f(dyn)
        }
        )
      } else {
        _if(less(size, const(16)), {
          {
            val ret = _if(equiv(choose_algorithm(size), const(0)), {
              val f = inserationsort(stat)
              f(dyn)
            }, {
              val f = selectionsort(stat)
              f(dyn)
            }
            )
            ret
          }
        }, {
          val ret = _if(equiv(choose_algorithm(size), const(0)), {
            //val f = quicksort(stat)
            val f = quicksort_imp(stat)
            f(dyn)
          }, {
            val f = mergesort(stat)
            f(dyn)
          }
          )
          ret
        }
        )
      }
      ret
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]] = doGlobalLambda(stageme, Some("Dispatch" + sstring), Some("Dispatch"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }


  def sort[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeSFunction[G, A, B, C, AB] = {
    val sstring = stat.genSig()
    val exposarg: ExposeRep[DynHeader[G, A, B, C, AB]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Array[G]](stat.vtyp)
    val stageme: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val mix = MixSortHeader(stat, dyn)
      import mix._
      import lub1._
      import evab._
      val size = minus(end, start)

      val fret: Rep[Array[G]] = if (!inline.inline) {
        val r: Rep[Array[G]] = _if(choose_inline(size), {
          val newline = stat.inline.copy(maxfunctions = stat.inline.maxfunctions - 1)
          val newmix: MixSortHeader[G, A, B, C, AB] = mix.copy(inline = newline)
          val copystat: StatHeader[G, A, B, C, AB] = newmix.getStatHeader()
          val rf = dispatcher(copystat)
          rf(dyn)
        }, {
          val rf = dispatcher(stat)
          rf(dyn)
        })
        r
      }
      else {
        val rf = dispatcher(stat)
        rf(dyn)
      }
      fret
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]] = doGlobalLambda(stageme, Some("Sort" + sstring), Some("Sort"))(exposarg, exposeret)
      MaybeSFunction(t)
    }


  }


  def selectionsort[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeSFunction[G, A, B, C, AB] = {
    val exposarg: ExposeRep[DynHeader[G, A, B, C, AB]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Array[G]](stat.vtyp)

    val stageme: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      import lub1._
      implicit val gmf = stat.gtyp.mf
      implicit val itupexpose = exposeTuple[G, Int]()
      import evab._
      val ret = rangefold(evab.unt(start, end), dyn.x, exposeret) {
        case (array, index) => {
          val swapele: Rep[G] = array(toRep(index))
          val (rvalue, rpos) = ((index) until end).foldLeft((swapele, toRep(index))) {
            case ((value, pos), index2) => {
              val b: Rep[G] = array(toRep(index2))
              val compf = compare(stat)
              val t: Rep[Boolean] = ordering_gt(compf(value,b), Const(0)) //Sorting Descending!
              myifThenElse[(Rep[G], Rep[Int])](t, (b, toRep(index2)), (value, pos))
            }
          }
          val bx = array(rpos)
          array.update(toRep(index), bx).update(rpos, swapele)
        }
      }
      ret.slice(toRep(start),toRep(end))
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]] = doGlobalLambda(stageme, Some("SelectionSort" + stat.genSig()), Some("SelectionSort"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }


  def inserationsort[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeSFunction[G, A, B, C, AB] = {
    val exposarg: ExposeRep[DynHeader[G, A, B, C, AB]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Array[G]](stat.vtyp)
    implicit val gmf = stat.gtyp.mf
    val stageme: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val mix = MixSortHeader(stat, dyn)
      import mix._
      import lub1._
      import evab._
      val size = minus(end, start)
      val ret: Rep[Array[G]] = evab._if(equiv(size, const(1)), dyn.x, {
        val one = const(1)
        val s1 = plus(start, one)
        rangefold(unt(s1, end), dyn.x, exposeret) {
          case (acc, ele) => inserationcore(acc, toRep(ele))(mix.gtyp.mf)
        }
      })
      vector_slice(ret,toRep(start),toRep(end))
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]] = doGlobalLambda(stageme, Some("InserationSort" + stat.genSig()), Some("InserationSort"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def mergesort[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeSFunction[G, A, B, C, AB] = {
    val exposarg: ExposeRep[DynHeader[G, A, B, C, AB]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Array[G]](stat.vtyp)

    val stageme: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      import evab._
      import lub1._
      val fsize = minus(end, start)
      val size_half = div(fsize, const(2))
      val half = plus(sh.start, size_half)
      val low_start = sh.start
      val low_end = half //0 .. 1, 0 .. 2
      val high_start = half
      val high_end = sh.end

      val lessmix = MixSortHeader.apply[G, A, AB, C, AB](x, low_start, low_end, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, sh.eva, sh.evab, sh.evc, sh.evab, lub3, lub4, lub3, lub4)
      val (statless, dynless) = lessmix.split()
      val lessf = sort(statless)
      val xl = lessf(dynless)
      val highmix = MixSortHeader.apply[G, AB, B, C, AB](x, high_start, high_end, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, sh.evab, sh.evb, sh.evc, sh.evab, lub2, lub2, lub4, lub4)
      val (stathigh, dynhigh) = highmix.split()
      val highf = sort(stathigh)
      val xh = highf(dynhigh)

      //val lowtmp = vector_slice(xl,toRep(low_start),toRep(half))
      //val hightmp = vector_slice(xl,toRep(half),toRep(high_end))
      merge(xl, xh)(sh.gtyp.mf)
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]] = doGlobalLambda(stageme, Some("MergeSort" + stat.genSig()), Some("MergeSort"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }


  def quicksort[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeSFunction[G, A, B, C, AB] = {
    val exposarg: ExposeRep[DynHeader[G, A, B, C, AB]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Array[G]](stat.vtyp)

    val stageme: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      import lub1._
      import evab._
      implicit val gmf = sh.gtyp.mf

      val fsize = minus(end, start)
      val half = div(fsize, const(2))
      val pivot = vector_apply(x, (toRep(half)))

      val compf = compare(stat)
      val less = filter[G](x, p => ordering_lt(compf(p,pivot),Const(0)))
      val equal = filter[G](x, p => ordering_equiv(compf(p,pivot),Const(0)))
      val greater = filter[G](x, p => ordering_gt(compf(p,pivot),Const(0)))

      val lesssize = size(less)
      val greatersize = size(greater)
      val zz: NoRep[Int] = 0
      val lessmix = MixSortHeader[G, NoRep, Rep, C, Rep](less, zz, lesssize, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, cNoRep, cRep, evc, cRep, NoRepRep, RepRep, NoRepRep, RepRep)
      val greatermix = MixSortHeader[G, NoRep, Rep, C, Rep](greater, zz, greatersize, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, cNoRep, cRep, evc, cRep, NoRepRep, RepRep, NoRepRep, RepRep)
      val (statless, dynless) = lessmix.split()
      val lessf = sort(statless)
      val xl = lessf(dynless)

      val (statgreater, dyngreater) = greatermix.split()
      val greaterf = sort(statgreater)
      val xg = greaterf(dyngreater)

      concat(concat(xl, equal), xg)

    }

    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]] = doGlobalLambda(stageme, Some("QuickSort" + stat.genSig()), Some("QuickSort"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def quicksort_imp[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): MaybeSFunction[G, A, B, C, AB] = {
    val exposarg: ExposeRep[DynHeader[G, A, B, C, AB]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Array[G]](stat.vtyp)

    val stageme: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      import lub1._
      import evab._
      implicit val gmf = sh.gtyp.mf

      val fsize = minus(end, start)
      val half = div(fsize, const(2))
      val pivot = vector_apply(x, (toRep(half)))

      val afterqsort = quicksortcore(x,toRep(start),toRep(end))

      /*val compf = compare(stat)
      val less = filter[G](x, p => ordering_lt(compf(p,pivot),Const(0)))
      val equal = filter[G](x, p => ordering_equiv(compf(p,pivot),Const(0)))
      val greater = filter[G](x, p => ordering_gt(compf(p,pivot),Const(0)))*/
      //val lessmix = MixSortHeader[G, NoRep, Rep, C, Rep](less, zz, lesssize, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, cNoRep, cRep, evc, cRep, NoRepRep, RepRep,
        //[G, A, AB, C, AB](x, low_start, low_end, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, sh.eva, sh.evab, sh.evc, sh.evab, lub3, lub4, lub3, lub4)
      val lessmix = MixSortHeader[G, A, AB, C, AB](afterqsort, start, half, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, sh.eva, sh.evab, sh.evc, sh.evab, lub3, lub4, lub3, lub4)
//val highmix = MixSortHeader.apply[G, AB, B, C, AB](x, high_start, high_end, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, sh.evab, sh.evb, sh.evc, sh.evab, lub2, lub2, lub4, lub4)
      val greatermix = MixSortHeader[G, AB, B, C, AB](afterqsort, half, sh.end, basesize, stat.comp, stat.inline, sh.gtyp, sh.vtyp, sh.evab, sh.evb, sh.evc, sh.evab, lub2, lub2, lub4, lub4)
      val (statless, dynless) = lessmix.split()
      val lessf = sort(statless)
      val xl = lessf(dynless)

      val (statgreater, dyngreater) = greatermix.split()
      val greaterf = sort(statgreater)
      val xg = greaterf(dyngreater)

      //concat(concat(xl, equal), xg)
      concat(xl, xg)
    }

    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynHeader[G, A, B, C, AB], Rep[Array[G]]] = doGlobalLambda(stageme, Some("QuickSort" + stat.genSig()), Some("QuickSort"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }


  def tmp[G, A[_], B[_], C[_], AB[_]](stat: StatHeader[G, A, B, C, AB]): (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = {
    val outer: (DynHeader[G, A, B, C, AB] => Rep[Array[G]]) = (dyn: DynHeader[G, A, B, C, AB]) => {
      val f = sort(stat)
      f(dyn)
    }
    outer
  }


  /*def graphvizexport() = {



    def lcomp(g: (Rep[Int],Rep[Int])): Rep[Int] = int_quick_compare(g._1,g._2)
    val ini: StatHeader[Int, Rep, Rep, Rep, Rep] = StatHeader[Int, Rep, Rep, Rep, Rep](Const(-1), Const(-1), Const(-1), lcomp, InlineInfo(false, 3, true))
    //val ini: StatSelectionHeader = StatSelectionHeader(None, None, None)
    val (code, cm) = emitGraph.emitDepGraphf(tmp(ini))(exposeDynHeader(ini), exposeRepFromRep[Array[Int]])
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    //stream.println(code)
    stream.flush()
    stream.close()
  }*/

  def codeexport() = {
    val ev: IRep[Rep] = cRep
    //def lcomp(g: (Rep[Int],Rep[Int])): Rep[Int] = int_quick_compare(g._1,g._2)
    //def lcomp(g: (Rep[MyComplex],Rep[MyComplex])): Rep[Int] = complex_compare(g._1,g._2)
    def lcomp(g: (Rep[Double],Rep[Double])): Rep[Int] = double_compare(g._1,g._2)
    val ini: StatHeader[Double, NoRep, NoRep, Rep, NoRep] = StatHeader[Double, NoRep, NoRep, Rep, NoRep](0, 500, Const(-1), lcomp, InlineInfo(false, 3, true))
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println(codestring)
    val esc = codegen.emitSource(tmp(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[Array[Double]])
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }


  val codestring = "import org.scalacheck._\nimport org.scalacheck.Properties\nimport org.scalacheck.Prop.forAll\nimport org.scalacheck.Gen._\nimport sort.MyComplex\n\nobject Bla extends org.scalacheck.Properties(\"Sort\") {\n\n  val genPosVector = containerOf[List, Int](Gen.posNum[Int])\n  val maxvalue = 2147483647\n  val buckets = 32\n\n  def maxd(cur: Int): Int = if (maxvalue / Math.pow(buckets, cur) > 1) maxd(cur + 1) else cur\n\n  val maxdiv = maxd(0)\n\n  //val maxdiv =   1000000000\n  property(\"startsWith\") = forAll(genPosVector) { l =>\n    true\n    /*val v = l.toArray\n    val c = new testClass\n    val s = c(v, 0, v.length, 16)\n    //val s2 = test(v,0,v.length)\n    val s3 = sortFunctional(v.toVector)\n    val s4 = msortfunctional(v.toVector)\n    val s5 = msd_radix_sort_head(v.toVector)\n    s3.corresponds(s) {\n      _ == _\n    } && s3.corresponds(s4) {\n      _ == _\n    } && s3.corresponds(s5) {\n      _ == _\n    }\n*/\n  }\n\n  def chooseBase(size: Int): Int = 0\n\n  def chooseSort(size: Int): Int = 0\n\n  def baseline(input: Vector[Int]): Vector[Int] = input.sortWith(_ < _)\n\n  def baseline_complex(input: Array[MyComplex]): Array[MyComplex] = {\n    def sortcomp(x: MyComplex, y: MyComplex): Boolean = x.cmp(y) < 0\n    input.sortWith((x, y) => sortcomp(x, y))\n  }\n\n  def baseline(input: Array[Int]): Array[Int] = input.sortWith(_ < _)\n\n  def merge(xs: Vector[Int], ys: Vector[Int]): Vector[Int] = {\n    if (xs.isEmpty) ys\n    else if (ys.isEmpty) xs\n    else {\n      (xs, ys) match {\n        case (x +: xs1, y +: ys1) =>\n          if (x > y)\n            x +: merge(xs1, ys)\n          else\n            y +: merge(xs, ys1)\n      }\n    }\n  }\n\n\n  def merge(xs: Array[Double], ys: Array[Double]): Array[Double] = ???\n  def merge(xs: Array[sort.MyComplex], ys: Array[sort.MyComplex]): Array[sort.MyComplex] = ???\n\n  def merge(xs: Array[Int], ys: Array[Int]): Array[Int] = {\n    if (xs.isEmpty) ys\n    else if (ys.isEmpty) xs\n    else {\n      val size = xs.size + ys.size\n\n      val retarray = new Array[Int](size)\n\n\n      var i = 0\n      var j = 0\n      var k = 0\n\n      while (i < xs.length && j < ys.length) {\n        if (xs(i) < ys(i)) {\n          retarray(k) = xs(i)\n          k = k + 1\n          i = i + i\n        }\n        else {\n          retarray(k) = ys(i)\n          k = k + 1\n          j = j + i\n        }\n      }\n\n      while (i < xs.length) {\n        retarray(k) = xs(i)\n        k = k + 1\n        i = i + i\n      }\n\n      while (j < ys.length) {\n        retarray(k) = ys(j)\n        k = k + 1\n        j = j + i\n      }\n      /*\n      (xs, ys) match {\n        case (x +: xs1, y +: ys1) =>\n          if (x > y)\n            x +: merge(xs1, ys)\n          else\n            y +: merge(xs, ys1)\n      }\n      */\n      retarray\n    }\n  }\n\n\n  def digitsplit(xs: Vector[Int], pos: Int): Vector[Vector[Int]] = {\n    val div: Int = Math.pow(buckets, maxdiv - 1 - pos).toInt\n    val tmpstore = new Array[Vector[Int]](buckets)\n    for (i <- 0 until tmpstore.size) tmpstore(i) = Vector.empty\n    val t = xs.foldLeft(tmpstore) {\n      (acc, ele) => {\n        val killright = (ele / div).toInt\n        val key = killright % buckets\n        tmpstore(key) = tmpstore(key) :+ ele\n        tmpstore\n      }\n    }\n    t.reverse.toVector\n\n  }\n\n  def msd_radix_sort(xs: Vector[Int], pos: Int): Vector[Int] = {\n    if (pos == maxdiv || xs.size < 2) xs\n    else {\n      val vlist = digitsplit(xs, pos)\n      val plus1 = pos + 1\n      vlist.flatMap(l => msd_radix_sort(l, plus1))\n    }\n  }\n\n\n  def msd_radix_sort_head(xs: Vector[Int]): Vector[Int] = msd_radix_sort(xs, 0)\n\n\n  def msortfunctional(xs: Vector[Int]): Vector[Int] = {\n    val n = xs.length / 2\n    if (n == 0) xs\n    else {\n      val (ys, zs) = xs splitAt n\n      merge(msortfunctional(ys), msortfunctional(zs))\n    }\n  }\n\n\n  def sortFunctional(xs: Vector[Int]): Vector[Int] = {\n    if (xs.length <= 1) xs\n    else {\n      val pivot = xs(xs.length / 2)\n      val less = xs.filter(p => pivot > p)\n      val equal = xs.filter(p => pivot == p)\n      val greater = xs.filter(p => pivot < p)\n      sortFunctional(greater) ++ equal ++ sortFunctional(less)\n    }\n  }\n\n  def ref_quicksort(xs: Array[Double], start: Int, end: Int) {\n    def swap(i: Int, j: Int) {\n      val t = xs(i);\n      xs(i) = xs(j);\n      xs(j) = t\n    }\n    def sort1(l: Int, r: Int) {\n      val pivot = xs((l + r) / 2)\n      var i = l;\n      var j = r\n      while (i <= j) {\n        while (xs(i) < pivot) i += 1\n        while (xs(j) > pivot) j -= 1\n        if (i <= j) {\n          swap(i, j)\n          i += 1\n          j -= 1\n        }\n      }\n      if (l < j) sort1(l, j)\n      if (j < r) sort1(i, r)\n    }\n    sort1(start, end)\n  }\n\n\n  def insertionSortA(a: Array[Int], start: Int, end: Int): Array[Int] = {\n    for (i <- start + 1 until end) {\n      // A[ i ] is added in the sorted sequence A[0, .. i-1]\n      // save A[i] to make a hole at index iHole\n      val item = a(i)\n      var iHole = i\n      // keep moving the hole to next smaller index until A[iHole - 1] is <= item\n      while (iHole > start && a(iHole - 1) > item) {\n        // move hole to next smaller index\n        a(iHole) = a(iHole - 1)\n        iHole = iHole - 1\n      }\n      // put item in the hole\n      a(iHole) = item\n    }\n    a\n  }\n\n  def insertioncore[T: Ordering](acc: Array[T], ele: Int): Array[T] = {\n    /*val orderingev = implicitly[Ordering[T]]\n    val currele = acc(ele)\n    val (sorted, rest) = acc.splitAt(ele)\n    val bigger = sorted.takeWhile(p => orderingev.gt(p,currele))\n    val smaller = sorted.drop(bigger.size)\n\n    (bigger :+ rest.head) ++ smaller ++ rest.tail*/\n    acc\n  }\n\n  /*def insertioncore(acc: Array[sort.MyComplex], ele: Int): Array[sort.MyComplex] = {\n    //def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {\n    val currele = acc(ele)\n    val (sorted, rest) = acc.splitAt(ele)\n    val bigger = sorted.takeWhile(p => p.cmp(currele) > 0)\n    val smaller = sorted.drop(bigger.size)\n    (bigger :+ rest.head) ++ smaller ++ rest.tail\n  }\n\n\n  def insertioncore(acc: Array[Int], ele: Int): Array[Int] = {\n    //def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {\n    val currele = acc(ele)\n    val (sorted, rest) = acc.splitAt(ele)\n    val bigger = sorted.takeWhile(p => p > currele)\n    val smaller = sorted.drop(bigger.size)\n    (bigger :+ rest.head) ++ smaller ++ rest.tail\n  }*/\n\n  def inserationsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n    if (start < end && (end - start) > 1) {\n      (start + 1 until end).foldLeft(v) {\n        (acc, ele) => insertioncore(acc.toArray, ele).toVector\n      }\n    } else {\n      v\n    }\n\n\n  }\n\n\n  def selectionsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n\n    (start until end).foldLeft(v) {\n      (acc, ele) => {\n        val swapele = acc(ele)\n        val (value, pos) = (ele until end).foldLeft((swapele, ele)) {\n          (acc2, k) => {\n            val (value, pos) = acc2\n            val currcheck = acc(k)\n            if (swapele < currcheck)\n              (currcheck, k)\n            else\n              (value, pos)\n          }\n        }\n        val bx = acc(pos)\n        val o1 = acc.updated(pos, swapele)\n        val o2 = o1.updated(ele, value)\n        o2\n      }\n    }\n  }\n}"
}


/*
/**
  * Created by rayda on 17-Oct-16.
  */
import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._

object Benchmark extends App{


  for (size <- 10 until 10000) {
    for (warmup <- 0 until 5) {
      val gen = containerOfN[List, Int](size, Gen.posNum[Int])
      val l = gen.sample.get
      val v = l.toVector
      val c = new testClass

      //val s2 = test(v,0,v.length)
      //val t1 = System.currentTimeMillis()
      //val s3 = Bla.sortFunctional(v)
      val t2 = System.nanoTime()
      val s4 = Bla.msortfunctional(v)
      //val s4 = Bla.baseline(v)
      val t3 = System.nanoTime()
      val s5 = c(v, 0, v.length, 16)
      //val s5 = Bla.baseline(v)
      val t4 = System.nanoTime()

      val time_poly = t4 - t3
      val time_leg = t3 - t2
      println(time_leg - time_poly)
    }


  }
}

 */
