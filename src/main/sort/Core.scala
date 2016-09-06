package sort


import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


class ComplexVector

class Complex

class Core extends Skeleton {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }


  object SelectionHeader {
    def apply(s: StatSelectionHeader, d: DynSelectionHeader): SelectionHeader = {
      val na = (s.start, d.start) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      val nl = (s.end, d.end) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      val nbs = (s.basesize, d.basesize) match {
        case (None, Some(r)) => SInt(r)
        case (Some(i), None) => SInt(i)
        case _ => ???
      }
      SelectionHeader(d.x, na, nl, nbs)
    }
  }

  case class SelectionHeader(x: Rep[Vector[Int]], start: SInt, end: SInt, basesize: SInt) {
    def getDynSelectionHeader() = {
      val ostart: Option[Rep[Int]] = start.i.fold(fa => Some(fa), fb => None)
      val oend: Option[Rep[Int]] = end.i.fold(fa => Some(fa), fb => None)
      val obasesize: Option[Rep[Int]] = basesize.i.fold(fa => Some(fa), fb => None)
      DynSelectionHeader(x, ostart, oend, obasesize)
    }

    def getStatSkel() = {
      val ostart: Option[Int] = start.i.fold(fa => None, fb => Some(fb))
      val oend: Option[Int] = end.i.fold(fa => None, fb => Some(fb))
      val obasesize: Option[Int] = basesize.i.fold(fa => None, fb => Some(fb))
      StatSelectionHeader(ostart, oend, obasesize)
    }
  }

  case class DynSelectionHeader(x: Rep[Vector[Int]], start: Option[Rep[Int]], end: Option[Rep[Int]], basesize: Option[Rep[Int]])

  case class StatSelectionHeader(start: Option[Int], end: Option[Int], basesize: Option[Int])

  implicit def exposeDynSelectionHeader(stat: StatSelectionHeader): ExposeRep[DynSelectionHeader] =
    new ExposeRep[DynSelectionHeader]() {

      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
        val fs = if (stat.start.isEmpty) Vector(Arg[Int]) else Vector.empty
        val fe = if (stat.end.isEmpty) Vector(Arg[Int]) else Vector.empty
        val fbs = if (stat.basesize.isEmpty) Vector(Arg[Int]) else Vector.empty
        Vector(Arg[Vector[Int]]) ++ fs ++ fe ++ fbs
      }
      val vec2t: Vector[Exp[_]] => DynSelectionHeader = (in: Vector[Exp[_]]) => {
        val x = in.head.asInstanceOf[Rep[Vector[Int]]]

        val (ostart, outstart) = if (stat.start.isEmpty) (Some(in.tail.head.asInstanceOf[Rep[Int]]), in.tail.tail) else (None, in.tail)
        val (oend, outend) = if (stat.end.isEmpty) (Some(outstart.head.asInstanceOf[Rep[Int]]), outstart.tail) else (None, outstart)
        val (obs, outbs) = if (stat.basesize.isEmpty) (Some(outend.head.asInstanceOf[Rep[Int]]), outend.tail) else (None, outend)
        DynSelectionHeader(x, ostart, oend, obs)
      }

      val t2vec: DynSelectionHeader => Vector[Exp[_]] = (in: DynSelectionHeader) => {
        val vstart = in.start.map(p => Vector(p)).getOrElse(Vector.empty)
        val vend = in.end.map(p => Vector(p)).getOrElse(Vector.empty)
        val vbs = in.basesize.map(p => Vector(p)).getOrElse(Vector.empty)
        Vector(in.x) ++ vstart ++ vend ++ vbs
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


  def mf(expose: ExposeRep[DynSelectionHeader], innerf: => (DynSelectionHeader => Rep[Vector[Int]])): StagedFunction[DynSelectionHeader, Rep[Vector[Int]]] = {
    val f: (DynSelectionHeader => Rep[Vector[Int]]) = (wuf: DynSelectionHeader) => innerf(wuf)
    println(f)
    val t: StagedFunction[DynSelectionHeader, Rep[Vector[Int]]] = doGlobalLambda(f, true)(expose, exposeRepFromRep[Vector[Int]])
    t
  }


  def tmp(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      mf(exposeDynSelectionHeader(stat), sort(stat))(dyn)
    }
    outer
  }


  def sort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      val mix = SelectionHeader(stat, dyn)
      val size = mix.end - mix.start
      //_if(SBool(Right(false)), {
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
      val fless = mf(lessexpose, sort(statsless))
      val sless = fless(dynless)

      val sh2 = SelectionHeader(greater, SInt(0), SInt(size(greater)), sh.basesize)
      val statgreater = sh2.getStatSkel()
      val greaterexpose = exposeDynSelectionHeader(statgreater)
      val fgreater = mf(greaterexpose, sort(statgreater))
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
      val fless = mf(lessexpose, sort(statsless))
      val sless = fless(dynless)

      val sh2 = SelectionHeader(x, high_start, high_end, sh.basesize)
      val statgreater = sh2.getStatSkel()
      val greaterexpose = exposeDynSelectionHeader(statgreater)
      val fgreater = mf(greaterexpose, sort(statgreater))
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


  def f(x: Rep[Int]): Rep[Int] = x

  def graphvizexport() = {
    val ini: StatSelectionHeader = StatSelectionHeader(None, None, None)
    val (code, cm) = emitGraph.emitDepGraphf(sort(ini))(exposeDynSelectionHeader(ini), exposeRepFromRep[Vector[Int]])
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    stream.println(code)
    stream.flush()
    stream.close()
  }

  def codeexport() = {
    val ini: StatSelectionHeader = StatSelectionHeader(None, None, None)
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println("import org.scalacheck._\nimport org.scalacheck.Properties\nimport org.scalacheck.Prop.forAll\nimport org.scalacheck.Gen._\n\nobject Bla extends org.scalacheck.Properties(\"Sort\") {\n\n  val genPosVector = containerOf[List,Int](Gen.posNum[Int])\n  val maxvalue = 2147483647\n  val buckets = 32\n\n  def maxd(cur: Int): Int = if (maxvalue / Math.pow(buckets,cur) > 1) maxd(cur + 1) else cur\n  val maxdiv = maxd(0)\n\n  //val maxdiv =   1000000000\n  property(\"startsWith\") = forAll(genPosVector) { l =>\n    val v = l.toVector\n    val c = new testClass\n    val s = c(v, 0, v.length, 16)\n    //val s2 = test(v,0,v.length)\n    val s3 = sortFunctional(v)\n    val s4 = msortfunctional(v)\n    val s5 = msd_radix_sort_head(v)\n    s3.corresponds(s) {\n      _ == _\n    } && s3.corresponds(s4) {\n      _ == _\n    }&& s3.corresponds(s5) {\n      _ == _\n    }\n\n  }\n\n  def merge(xs: Vector[Int], ys: Vector[Int]): Vector[Int] = {\n    if (xs.isEmpty) ys\n    else if (ys.isEmpty) xs\n    else {\n      (xs, ys) match {\n        case (x +: xs1, y +: ys1) =>\n          if (x > y)\n            x +: merge(xs1, ys)\n          else\n            y +: merge(xs, ys1)\n      }\n    }\n  }\n\n\n\n\n\n\n  def digitsplit(xs: Vector[Int], pos: Int): Vector[Vector[Int]] = {\n    val div:Int  = Math.pow(buckets,maxdiv-1-pos).toInt\n    val tmpstore = new Array[Vector[Int]](buckets)\n    for (i <- 0 until tmpstore.size) tmpstore(i) = Vector.empty\n    val t = xs.foldLeft(tmpstore){\n      (acc,ele) => {\n        val killright = (ele / div).toInt\n        val key = killright % buckets\n        tmpstore(key) = tmpstore(key) :+ ele\n        tmpstore\n      }\n    }\n    t.reverse.toVector\n\n  }\n\n  def msd_radix_sort(xs: Vector[Int], pos: Int): Vector[Int] = {\n    if (pos == maxdiv || xs.size < 2) xs\n    else {\n      val vlist = digitsplit(xs, pos)\n      val plus1 = pos + 1\n      vlist.flatMap(l => msd_radix_sort(l, plus1))\n    }\n  }\n\n\n  def msd_radix_sort_head(xs: Vector[Int]): Vector[Int] = msd_radix_sort(xs,0)\n\n\n  def msortfunctional(xs: Vector[Int]): Vector[Int] = {\n    val n = xs.length / 2\n    if (n == 0) xs\n    else {\n      val (ys, zs) = xs splitAt n\n      merge(msortfunctional(ys), msortfunctional(zs))\n    }\n  }\n\n\n  def sortFunctional(xs: Vector[Int]): Vector[Int] = {\n    if (xs.length <= 1) xs\n    else {\n      val pivot = xs(xs.length / 2)\n      val less = xs.filter(p => pivot > p)\n      val equal = xs.filter(p => pivot == p)\n      val greater = xs.filter(p => pivot < p)\n      sortFunctional(greater) ++ equal ++ sortFunctional(less)\n    }\n  }\n\n\n  def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {\n    val currele = acc(ele)\n    val (sorted, rest) = acc.splitAt(ele)\n    val bigger = sorted.takeWhile(p => p > currele)\n    val smaller = sorted.drop(bigger.size)\n    (bigger :+ rest.head) ++ smaller ++ rest.tail\n  }\n\n  def inserationsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n    if (start < end && (end - start) > 1) {\n      (start + 1 until end).foldLeft(v) {\n        (acc, ele) => insertioncore(acc,ele)\n      }\n    } else {\n      v\n    }\n\n\n  }\n\n\n  def selectionsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n\n    (start until end).foldLeft(v) {\n      (acc, ele) => {\n        val swapele = acc(ele)\n        val (value, pos) = (ele until end).foldLeft((swapele, ele)) {\n          (acc2, k) => {\n            val (value, pos) = acc2\n            val currcheck = acc(k)\n            if (swapele < currcheck)\n              (currcheck, k)\n            else\n              (value, pos)\n          }\n        }\n        val bx = acc(pos)\n        val o1 = acc.updated(pos, swapele)\n        val o2 = o1.updated(ele, value)\n        o2\n      }\n    }\n  }\n\n}")
    val esc = codegen.emitSource(tmp(ini), "testClass", stream2)(exposeDynSelectionHeader(ini), exposeRepFromRep[Vector[Int]])
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}
