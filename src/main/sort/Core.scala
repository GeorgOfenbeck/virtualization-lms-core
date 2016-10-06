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


  def choose_sortx(size: SRep[Int]): SRep[Int] = {
    size.fold(fa => Left(choose_sort(fa)), fb => Right(0))
  }


  def sort[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): StagedFunction[DynHeader[A, B, C], Rep[Vector[Int]]] = {
    val sstring = stat.genSig()
    val exposarg: ExposeRep[DynHeader[A, B, C]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]
    val stageme: (DynHeader[A, B, C] => Rep[Vector[Int]]) = (dyn: DynHeader[A, B, C]) => {
      val mix = MixSortHeader(stat, dyn)
      import mix._
      val size = evb.minus(end, start)(evb, eva)
      val sev = getSRepEv(size)
      val cond = sev.less(size, mkSRep(10))
      val cev = getSRepEv(cond)
      val ret = cev._if(cond, {
        {
          val alg = choose_sortx(size)
          val alev = getSRepEv(alg)
          val cond = sev.equiv(size, mkSRep(0))
          val cev = getSRepEv(cond)
          val ret = cev._if(cond, {
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
          val alg = choose_sortx(size)
          val alev = getSRepEv(alg)
          val cond = sev.equiv(size, mkSRep(0))
          val cev = getSRepEv(cond)
          val ret = cev._if(cond, {
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

    val t: StagedFunction[DynHeader[A, B, C], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("Sort" + sstring),Some("Sort"))(exposarg, exposeret)
    t

  }

  def selectionsort[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): StagedFunction[DynHeader[A, B, C], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A, B, C]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]

    val stageme: (DynHeader[A, B, C] => Rep[Vector[Int]]) = (dyn: DynHeader[A, B, C]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      val x = sh.x
      implicit val itupexpose = exposeTuple[Int, Int]()

      val start = eva.toSRep(sh.start)
      val end = evb.toSRep(sh.end)
      val startev = getSRepEv(start)
      import startev._
      val r = (start until end)

      val rev = getSRepEv(r)
      rev.rangefold(r,dyn.x,exposeret) {
        case (acc, ele) => inserationcore(acc, ele.toRep(ele))
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
    val t: StagedFunction[DynHeader[A,B,C], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("SelectionSort" + stat.genSig()),Some("SelectionSort"))(exposarg,exposeret )
    t
  }



  def inserationsort[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): StagedFunction[DynHeader[A, B, C], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A, B, C]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]
    val stageme: (DynHeader[A, B, C] => Rep[Vector[Int]]) = (dyn: DynHeader[A, B, C]) => {
      val mix = MixSortHeader(stat, dyn)
      import mix._
      val size = evb.minus(end, start)(evb, eva)
      val sev = getSRepEv(size)
      val cond = sev.equiv(size, mkSRep(1))
      val cev = getSRepEv(cond)
      val ret: Rep[Vector[Int]] = cev._if(cond, dyn.x, {
        val one = mkSRep(1)
        val oneev = getSRepEv(one)
        val s1 = oneev.plus(mix.eva.toSRep(mix.start), one)
        val start = eva.toSRep(mix.start)
        val startev = getSRepEv(start)
        import startev._
        val r = (start until evb.toSRep(mix.end))
        val rev = getSRepEv(r)
        rev.rangefold(r,dyn.x,exposeret) {
          case (acc, ele) => inserationcore(acc, ele.toRep(ele))
        }
      })
      ret
    }
    val t: StagedFunction[DynHeader[A,B,C], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("InserationSort" + stat.genSig()),Some("InserationSort"))(exposarg,exposeret )
    t
  }

  def mergesort[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): StagedFunction[DynHeader[A, B, C], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A, B, C]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]

    val stageme: (DynHeader[A, B, C] => Rep[Vector[Int]]) = (dyn: DynHeader[A, B, C]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      val start = sh.start
      val end = sh.end
      val x = dyn.x
      val fsize = evb.minus(end, start)(evb, eva)
      val sev = getSRepEv(fsize)
      val half = sev.div(fsize,mkSRep(2))
      val hev = getSRepEv(half)

      val low_start = sh.start
      val low_end = half
      val high_start = hev.plus(half,mkSRep(1))
      val high_end = sh.end

      val lower = low_end.fold (fa => {
        val lessmix = MixSortHeader(x, low_start, fa, sh.basesize)(sh.eva, isRep,sh.evc)
        val (statless,dynless) = lessmix.split()
        val lessf = sort(statless)
        val xl = lessf(dynless)
        xl
      }
        ,
        fb => {
          val lessmix = MixSortHeader(x, low_start, fb, sh.basesize)(sh.eva, isNoRep,sh.evc)
          val (statless,dynless) = lessmix.split()
          val lessf = sort(statless)
          val xl = lessf(dynless)
          xl
        }
      )

      val higher = high_start.fold (fa => {
        val lessmix = MixSortHeader(x, fa, high_end, sh.basesize)(isRep, sh.evb,sh.evc)
        val (statless,dynless) = lessmix.split()
        val lessf = sort(statless)
        val xl = lessf(dynless)
        xl
      }
        ,
        fb => {
          val lessmix = MixSortHeader(x, fb, high_end, sh.basesize)(isNoRep, sh.evb,sh.evc)
          val (statless,dynless) = lessmix.split()
          val lessf = sort(statless)
          val xl = lessf(dynless)
          xl
        }
      )
      merge(lower, higher)
    }
    val t: StagedFunction[DynHeader[A,B,C], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("MergeSort" + stat.genSig()),Some("MergeSort"))(exposarg,exposeret )
    t
  }


  def quicksort[A[_], B[_], C[_]](stat: StatHeader[A, B, C]): StagedFunction[DynHeader[A, B, C], Rep[Vector[Int]]] = {
    val exposarg: ExposeRep[DynHeader[A, B, C]] = exposeDynHeader(stat)
    val exposeret = exposeRepFromRep[Vector[Int]]

    val stageme: (DynHeader[A, B, C] => Rep[Vector[Int]]) = (dyn: DynHeader[A, B, C]) => {
      val sh = MixSortHeader(stat, dyn)
      import sh._
      val start = sh.start
      val end = sh.end
      val x = dyn.x
      val fsize = evb.minus(end, start)(evb, eva)
      val sev = getSRepEv(fsize)
      val half = sev.div(fsize,mkSRep(2))
      val hev = getSRepEv(half)
      val pivot = x(hev.toRep(half))
      val less = filter[Int](x, p => pivot > p)
      val equal = filter[Int](x, p => pivot equiv p)
      val greater = filter[Int](x, p => pivot < p)



      //val sh1 = SelectionHeader(less, SInt(0), SInt(size(less)), sh.basesize)
      val lesssize = size(less)
      val greatersize = size(greater)
      val zz: NoRep[Int] = 0
      val lessmix = MixSortHeader(less,zz,lesssize,sh.basesize)(isNoRep,isRep,sh.evc)
      val greatermix = MixSortHeader(greater,zz,greatersize,sh.basesize)(isNoRep,isRep,sh.evc)

      val (statless,dynless) = lessmix.split()
      val lessf = sort(statless)
      val xl = lessf(dynless)

      val (statgreater,dyngreater) = greatermix.split()
      val greaterf = sort(statgreater)
      val xg = greaterf(dyngreater)

      concat(concat(xl, equal), xg)

      /*val statsless = StatHeader(mkSRep(0),Const(-1),sh.basesize)
      val lessexpose = exposeDynHeader(statsless)
      val dynless = sh1.getDynSelectionHeader()
      val fless = mf(lessexpose, sort(statsless), "sort")
      val sless = fless(dynless)

      val sh2 = SelectionHeader(greater, SInt(0), SInt(size(greater)), sh.basesize)
      val statgreater = sh2.getStatSkel()
      val greaterexpose = exposeDynSelectionHeader(statgreater)
      val fgreater = mf(greaterexpose, sort(statgreater), "sort")
      val dyngreater = sh2.getDynSelectionHeader()
      val sgreater = fgreater(dyngreater)

      concat(concat(sless, equal), sgreater)*/

    }
    val t: StagedFunction[DynHeader[A,B,C], Rep[Vector[Int]]] = doGlobalLambda(stageme, Some("QuickSort" + stat.genSig()),Some("QuickSort"))(exposarg,exposeret )
    t
  }

  /*
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



  */

  def tmp[A[_],B[_],C[_]](stat: StatHeader[A,B,C]): (DynHeader[A,B,C] => Rep[Vector[Int]]) = {
    val outer: (DynHeader[A,B,C] => Rep[Vector[Int]]) = (dyn: DynHeader[A,B,C]) => {
      val f = sort(stat)
      f(dyn)
    }
    outer
  }




  def graphvizexport() = {
    //val ini: StatSelectionHeader = StatSelectionHeader(None, None, None)
    //val (code, cm) = emitGraph.emitDepGraphf(sort(ini))(exposeDynSelectionHeader(ini), exposeRepFromRep[Vector[Int]])
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    //stream.println(code)
    stream.flush()
    stream.close()
  }

  def codeexport() = {
    val ev: IRep[Rep] = isRep
    val ini: StatHeader[Rep,Rep,Rep] = StatHeader.apply[Rep,Rep,Rep](Const(1),Const(1),Const(1))
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println("import org.scalacheck._\nimport org.scalacheck.Properties\nimport org.scalacheck.Prop.forAll\nimport org.scalacheck.Gen._\n\nobject Bla extends org.scalacheck.Properties(\"Sort\") {\n\n  val genPosVector = containerOf[List,Int](Gen.posNum[Int])\n  val maxvalue = 2147483647\n  val buckets = 32\n\n  def maxd(cur: Int): Int = if (maxvalue / Math.pow(buckets,cur) > 1) maxd(cur + 1) else cur\n  val maxdiv = maxd(0)\n\n  //val maxdiv =   1000000000\n  property(\"startsWith\") = forAll(genPosVector) { l =>\n    val v = l.toVector\n    val c = new testClass\n    val s = c(v, 0, v.length, 16)\n    //val s2 = test(v,0,v.length)\n    val s3 = sortFunctional(v)\n    val s4 = msortfunctional(v)\n    val s5 = msd_radix_sort_head(v)\n    s3.corresponds(s) {\n      _ == _\n    } && s3.corresponds(s4) {\n      _ == _\n    }&& s3.corresponds(s5) {\n      _ == _\n    }\n\n  }\n\n  def merge(xs: Vector[Int], ys: Vector[Int]): Vector[Int] = {\n    if (xs.isEmpty) ys\n    else if (ys.isEmpty) xs\n    else {\n      (xs, ys) match {\n        case (x +: xs1, y +: ys1) =>\n          if (x > y)\n            x +: merge(xs1, ys)\n          else\n            y +: merge(xs, ys1)\n      }\n    }\n  }\n\n\n\n\n\n\n  def digitsplit(xs: Vector[Int], pos: Int): Vector[Vector[Int]] = {\n    val div:Int  = Math.pow(buckets,maxdiv-1-pos).toInt\n    val tmpstore = new Array[Vector[Int]](buckets)\n    for (i <- 0 until tmpstore.size) tmpstore(i) = Vector.empty\n    val t = xs.foldLeft(tmpstore){\n      (acc,ele) => {\n        val killright = (ele / div).toInt\n        val key = killright % buckets\n        tmpstore(key) = tmpstore(key) :+ ele\n        tmpstore\n      }\n    }\n    t.reverse.toVector\n\n  }\n\n  def msd_radix_sort(xs: Vector[Int], pos: Int): Vector[Int] = {\n    if (pos == maxdiv || xs.size < 2) xs\n    else {\n      val vlist = digitsplit(xs, pos)\n      val plus1 = pos + 1\n      vlist.flatMap(l => msd_radix_sort(l, plus1))\n    }\n  }\n\n\n  def msd_radix_sort_head(xs: Vector[Int]): Vector[Int] = msd_radix_sort(xs,0)\n\n\n  def msortfunctional(xs: Vector[Int]): Vector[Int] = {\n    val n = xs.length / 2\n    if (n == 0) xs\n    else {\n      val (ys, zs) = xs splitAt n\n      merge(msortfunctional(ys), msortfunctional(zs))\n    }\n  }\n\n\n  def sortFunctional(xs: Vector[Int]): Vector[Int] = {\n    if (xs.length <= 1) xs\n    else {\n      val pivot = xs(xs.length / 2)\n      val less = xs.filter(p => pivot > p)\n      val equal = xs.filter(p => pivot == p)\n      val greater = xs.filter(p => pivot < p)\n      sortFunctional(greater) ++ equal ++ sortFunctional(less)\n    }\n  }\n\n\n  def insertioncore(acc: Vector[Int], ele: Int): Vector[Int] = {\n    val currele = acc(ele)\n    val (sorted, rest) = acc.splitAt(ele)\n    val bigger = sorted.takeWhile(p => p > currele)\n    val smaller = sorted.drop(bigger.size)\n    (bigger :+ rest.head) ++ smaller ++ rest.tail\n  }\n\n  def inserationsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n    if (start < end && (end - start) > 1) {\n      (start + 1 until end).foldLeft(v) {\n        (acc, ele) => insertioncore(acc,ele)\n      }\n    } else {\n      v\n    }\n\n\n  }\n\n\n  def selectionsort(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n\n    (start until end).foldLeft(v) {\n      (acc, ele) => {\n        val swapele = acc(ele)\n        val (value, pos) = (ele until end).foldLeft((swapele, ele)) {\n          (acc2, k) => {\n            val (value, pos) = acc2\n            val currcheck = acc(k)\n            if (swapele < currcheck)\n              (currcheck, k)\n            else\n              (value, pos)\n          }\n        }\n        val bx = acc(pos)\n        val o1 = acc.updated(pos, swapele)\n        val o2 = o1.updated(ele, value)\n        o2\n      }\n    }\n  }\n\n}")
    val esc = codegen.emitSource(tmp(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[Vector[Int]])
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}
