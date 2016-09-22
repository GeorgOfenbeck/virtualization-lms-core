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


  trait RepSelector {
    val rrep: Boolean

    def repselect[A[_], T](a: A[T], ev: IRep[A]): Option[A[T]] =
      if (rrep) {
        if (ev.isRep()) Some(a) else None
      } else if (!ev.isRep()) None else Some(a)
  }

  trait DynSelector {
    val rrep: Boolean = true
  }

  trait StatSelector {
    val rrep: Boolean = false
  }

  abstract class Base[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int],
                                        eva: IRep[A], evb: IRep[B], evc: IRep[C])

  class SortHeader[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int],
                                     eva: IRep[A], evb: IRep[B], evc: IRep[C])
    extends Base(start, end, basesize, eva, evb, evc) with RepSelector {
    def start(): Option[A[Int]] = repselect(start, eva)

    def end(): Option[B[Int]] = repselect(end, evb)

    def basesize(): Option[C[Int]] = repselect(basesize, evc)
  }


  class DynHeader[A[_], B[_], C[_]](val x: Rep[Vector[Int]], start: A[Int], end: B[Int], basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends SortHeader(start, end, basesize, eva, evb, evc) with DynSelector

  class StatHeader[A[_], B[_], C[_]](start: A[Int], end: B[Int], basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends SortHeader(start, end, basesize, eva, evb, evc) with StatSelector

  class MixSortHeader[A[_], B[_], C[_]](val start: A[Int], val end: B[Int], val basesize: C[Int], val eva: IRep[A], val evb: IRep[B], val evc: IRep[C]) extends Base(start, end, basesize, eva, evb, evc)

  object MixSortHeader {
    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    def apply[RA[_], RB[_], RC[_]](hs: StatHeader[RA, RB, RC], hd: DynHeader[RA, RB, RC]): MixSortHeader[RA, RB, RC] = {
      val a: RA[Int] = choose(hs.start(), hd.start(), hs.eva)
      val b: RB[Int] = choose(hs.end(), hd.end(), hs.evb)
      val c: RC[Int] = choose(hs.basesize(), hd.basesize(), hs.evc)
      new MixSortHeader[RA, RB, RC](a, b, c, hs.eva, hs.evb, hs.evc)
    }
  }


  implicit def exposeDynHeader[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): ExposeRep[DynHeader[A, B, C]] =
    new ExposeRep[DynHeader[A, B, C]]() {

      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[Vector[Int]]) ++ stat.eva.fresh() ++ stat.evb.fresh() ++ stat.evc.fresh()
      val vec2t: Vector[Exp[_]] => DynHeader[A, B, C] = (in: Vector[Exp[_]]) => {
        def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: Option[T[A]], ev: IRep[T]): (Vector[Rep[_]], T[A]) = {
          val (vecafter, ele) = ev.fetch[A](in)
          val res: T[A] = ele.getOrElse(statele.get)
          (vecafter, res)
        }
        val x = in.head.asInstanceOf[Rep[Vector[Int]]]
        val (ostart, outstart) = help(in.tail, stat.start(), stat.eva)
        val (oend, outend) = help(ostart, stat.end(), stat.evb)
        val (obs, outbs) = help(oend, stat.basesize(), stat.evc)
        new DynHeader[A, B, C](x, outstart, outend, outbs,stat.eva,stat.evb, stat.evc )
      }

      val t2vec: DynHeader[A, B, C] => Vector[Exp[_]] = (in: DynHeader[A, B, C]) => {
        def help[T[_], A](ele: Option[T[A]], ev: IRep[T]): Vector[Exp[_]] = {
          ele.map(p => ev.getRep(p).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
        }
        Vector(in.x) ++ help(in.start(),in.eva) ++ help(in.end(),in.evb) ++ help(in.basesize(), in.evc)
      }

    }


  implicit def exposeTuple[A: TypeRep, B: TypeRep](): ExposeRep[(Rep[A], Rep[B])] = new ExposeRep[(Rep[A], Rep[B])] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[A], Arg[B])
    val vec2t: Vector[Exp[_]] => ((Exp[A], Exp[B])) = (in: Vector[Exp[_]]) => {
      val a = in.head.asInstanceOf[Exp[A]]
      val b = in.tail.head.asInstanceOf[Exp[B]]
      (a, b)
    }
    val t2vec: ((Exp[A], Exp[B])) => Vector[Exp[_]] = (in: ((Exp[A], Exp[B]))) => Vector(in._1, in._2)
  }


  /*
  def sort[A[_],B[_],C[_]](stat: StatHeader[A,B,C]): StagedFunction[DynHeader[A,B,C],Rep[Vector[Int]]] = {
    val stageme: (DynHeader[A,B,C] => Rep[Vector[Int]]) = (dyn: DynHeader[A,B,C]) => {
      val mix = SelectionHeader(stat, dyn)
      val size = mix.end - mix.start


      val qs = mf(exposeDynSelectionHeader(stat), quicksort(stat), "quicksort")
      qs(dyn)

      /*
      _if(size < mix.basesize, {
        _if(choose_base(size) == SInt(1), {
          val ms = mf(exposeDynSelectionHeader(stat), mergesort(stat))
          ms(dyn)
        }, {
          val ms = mf(exposeDynSelectionHeader(stat), mergesort(stat))
          ms(dyn)
        }
        )
      },{
        _if(choose_base(size) == SInt(0), {
          val is = mf(exposeDynSelectionHeader(stat), inserationsort(stat))
          is(dyn)
        }, {
          val ss = mf(exposeDynSelectionHeader(stat), selectionsort(stat))
          ss(dyn)
        }
        )
      })

      */

      /*},
        _if(choose_base(size) == SInt(1), {
          val is = mf(exposeDynSelectionHeader(stat), inserationsort(stat))
          is(dyn)
        }, {
          val ss = mf(exposeDynSelectionHeader(stat), selectionsort(stat))
          ss(dyn)
        })*/
      //)
    }
    outer
  }

  /*
  def sort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      val mix = SelectionHeader(stat, dyn)
      val size = mix.end - mix.start
      //_if(SBool(Right(false)), {
      //_if(size < mix.basesize, {
        _if(choose_sort(size) == SInt(0), {
          val ms = mf(exposeDynSelectionHeader(stat), mergesort(stat))
          ms(dyn)
          //val qs = mf(exposeDynSelectionHeader(stat), quicksort(stat))
          //qs(dyn)
        }, {
          val ms = mf(exposeDynSelectionHeader(stat), mergesort(stat))
          ms(dyn)
        }
        )
      /*},
        _if(choose_base(size) == SInt(1), {
          val is = mf(exposeDynSelectionHeader(stat), inserationsort(stat))
          is(dyn)
        }, {
          val ss = mf(exposeDynSelectionHeader(stat), selectionsort(stat))
          ss(dyn)
        })*/
      //)
    }
    outer
  }
  //inserationsort(mix)
      val is = mf(exposeDynSelectionHeader(stat), inserationsort(stat))
      is(dyn)
    }
      , {
        val ss = mf(exposeDynSelectionHeader(stat), selectionsort(stat))
        ss(dyn)
        //selectionsort(mix)
      }
   */

  def radixsort(sh: SelectionHeader): Rep[Vector[Int]] = {
    ???
  }


  def quicksort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      val sh = SelectionHeader(stat, dyn)
      val start = sh.start
      val end = sh.end
      val x = sh.x
      val half = (end - start) / 2
      val pivot = x(half)
      val less = filter[Int](x, p => pivot > p)
      val equal = filter[Int](x, p => pivot equiv p)
      val greater = filter[Int](x, p => pivot < p)



      val sh1 = SelectionHeader(less, SInt(0), SInt(size(less)), sh.basesize)
      val statsless = sh1.getStatSkel()
      val lessexpose = exposeDynSelectionHeader(statsless)
      val dynless = sh1.getDynSelectionHeader()
      val fless = mf(lessexpose, sort(statsless), "sort")
      val sless = fless(dynless)

      val sh2 = SelectionHeader(greater, SInt(0), SInt(size(greater)), sh.basesize)
      val statgreater = sh2.getStatSkel()
      val greaterexpose = exposeDynSelectionHeader(statgreater)
      val fgreater = mf(greaterexpose, sort(statgreater), "sort")
      val dyngreater = sh2.getDynSelectionHeader()
      val sgreater = fgreater(dyngreater)

      concat(concat(sless, equal), sgreater)
    }
    outer
  }

  def mergesort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      val sh = SelectionHeader(stat, dyn)
      val start = sh.start
      val end = sh.end
      val x = sh.x
      val half = (end - start) / 2
      val low_start = sh.start
      val low_end = half
      val high_start = half + 1
      val high_end = sh.end

      val sh1 = SelectionHeader(x, low_start, low_end, sh.basesize)
      val statsless = sh1.getStatSkel()
      val lessexpose = exposeDynSelectionHeader(statsless)
      val dynless = sh1.getDynSelectionHeader()
      val fless = mf(lessexpose, sort(statsless), "sort")
      val sless = fless(dynless)

      val sh2 = SelectionHeader(x, high_start, high_end, sh.basesize)
      val statgreater = sh2.getStatSkel()
      val greaterexpose = exposeDynSelectionHeader(statgreater)
      val fgreater = mf(greaterexpose, sort(statgreater), "sort")
      val dyngreater = sh2.getDynSelectionHeader()
      val sgreater = fgreater(dyngreater)
      merge(sless, sgreater)
    }
    outer
  }


  //def inserationsort(sh: SelectionHeader): Rep[Vector[Int]] = {
  def inserationsort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      val sh = SelectionHeader(stat, dyn)
      myifThenElse(size(sh.x) > Const(1), {
        sh.x
      }, {
        val s1 = sh.start + SInt(1)
        (sh.start until sh.end).foldLeft(sh.x) {
          case (acc, ele) => inserationcore(acc, ele)
        }
      })
    }
    val x = outer.asInstanceOf[Object]
    println(x.pickle.value)
    outer
  }


  def selectionsort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      val sh = SelectionHeader(stat, dyn)
      val start = sh.start
      val end = sh.end
      val x = sh.x
      implicit val itupexpose = exposeTuple[Int, Int]()

      (start until end).foldLeft(x) {
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
      }
    }
    outer
  }
*/

  def f(x: Rep[Int]): Rep[Int] = x

  def graphvizexport() = {
    //val ini: StatSelectionHeader = StatSelectionHeader(None, None, None)
    //val (code, cm) = emitGraph.emitDepGraphf(sort(ini))(exposeDynSelectionHeader(ini), exposeRepFromRep[Vector[Int]])
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    //stream.println(code)
    stream.flush()
    stream.close()
  }

  def codeexport() = {
    //val ini: StatSelectionHeader = StatSelectionHeader(None, None, None)
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println("import org.scalacheck._\nimport org.scalacheck.Properties\nimport org.scalacheck.Prop.forAll\nimport org.scalacheck.Gen._\n\nobject Bla extends org.scalacheck.Properties(\"Sort\") {\n\n  val genPosVector = containerOf[List,Int](Gen.posNum[Int])\n  val maxvalue = 2147483647\n  val buckets = 32\n\n  def maxd(cur: Int): Int = if (maxvalue / Math.pow(buckets,cur) > 1) maxd(cur + 1) else cur\n  val maxdiv = maxd(0)\n\n  //val maxdiv =   1000000000\n  property(\"startsWith\") = forAll(genPosVector) { l =>\n    val v = l.toVector\n    val c = new testClass\n    val s = c(v, 0, v.length, 16)\n    //val s2 = test(v,0,v.length)\n    val s3 = sortFunctional(v)\n    val s4 = msortfunctional(v)\n    val s5 = msd_radix_sort_head(v)\n    s3.corresponds(s) {\n      _ == _\n    } && s3.corresponds(s4) {\n      _ == _\n    }&& s3.corresponds(s5) {\n      _ == _\n    }\n\n  }\n\n  def merge(xs: Vector[Int], ys: Vector[Int]): Vector[Int] = {\n    if (xs.isEmpty) ys\n    else if (ys.isEmpty) xs\n    else {\n      (xs, ys) match {\n        case (x +: xs1, y +: ys1) =>\n          if (x > y)\n            x +: merge(xs1, ys)\n          else\n            y +: merge(xs, ys1)\n      }\n    }\n  }\n\n\n\n\n\n\n  def digitsplit(xs: Vector[Int], pos: Int): Vector[Vector[Int]] = {\n    val div:Int  = Math.pow(buckets,maxdiv-1-pos).toInt\n    val tmpstore = new Array[Vector[Int]](buckets)\n    for (i <- 0 until tmpstore.size) tmpstore(i) = Vector.empty\n    val t = xs.foldLeft(tmpstore){\n      (acc,ele) => {\n        val killright = (ele / div).toInt\n        val key = killright % buckets\n        tmpstore(key) = tmpstore(key) :+ ele\n        tmpstore\n      }\n    }\n    t.reverse.toVector\n\n  }\n\n  def msd_radix_sort(xs: Vector[Int], pos: Int): Vector[Int] = {\n    if (pos == maxdiv || xs.size < 2) xs\n    else {\n      val vlist = digitsplit(xs, pos)\n      val plus1 = pos + 1\n      vlist.flatMap(l => msd_radix_sort(l, plus1))\n    }\n  }\n\n\n  def msd_radix_sort_head(xs: Vector[Int]): Vector[Int] = msd_radix_sort(xs,0)\n\n\n  def msortfunctional(xs: Vector[Int]): Vector[Int] = {\n    val n = xs.length / 2\n    if (n == 0) xs\n    else {\n      val (ys, zs) = xs splitAt n\n      merge(msortfunctional(ys), msortfunctional(zs))\n    }\n  }\n\n\n  def sortFunctional(xs: Vector[Int]): Vector[Int] = {\n    if (xs.length <= 1) xs\n    else {\n      val pivot = xs(xs.length / 2)\n      val less = xs.filter(p => pivot > p)\n      val equal = xs.filter(p => pivot == p)\n      val greater = xs.filter(p => pivot < p)\n      sortFunctional(greater) ++ equal ++ sortFunctional(less)\n    }\n  }\n\n\n  def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {\n    val currele = acc(ele)\n    val (sorted, rest) = acc.splitAt(ele)\n    val bigger = sorted.takeWhile(p => p > currele)\n    val smaller = sorted.drop(bigger.size)\n    (bigger :+ rest.head) ++ smaller ++ rest.tail\n  }\n\n  def inserationsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n    if (start < end && (end - start) > 1) {\n      (start + 1 until end).foldLeft(v) {\n        (acc, ele) => insertioncore(acc,ele)\n      }\n    } else {\n      v\n    }\n\n\n  }\n\n\n  def selectionsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n\n    (start until end).foldLeft(v) {\n      (acc, ele) => {\n        val swapele = acc(ele)\n        val (value, pos) = (ele until end).foldLeft((swapele, ele)) {\n          (acc2, k) => {\n            val (value, pos) = acc2\n            val currcheck = acc(k)\n            if (swapele < currcheck)\n              (currcheck, k)\n            else\n              (value, pos)\n          }\n        }\n        val bx = acc(pos)\n        val o1 = acc.updated(pos, swapele)\n        val o2 = o1.updated(ele, value)\n        o2\n      }\n    }\n  }\n\n}")
    //val esc = codegen.emitSource(tmp(ini), "testClass", stream2)(exposeDynSelectionHeader(ini), exposeRepFromRep[Vector[Int]])
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}
