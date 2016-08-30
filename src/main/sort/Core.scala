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
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps  with ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }







  object SelectionHeader{
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
      SelectionHeader(d.x,na,nl)
    }
  }

  case class SelectionHeader(x: Rep[Vector[Int]], start: SInt, end: SInt) {
    def getDynSelectionHeader() = {
      val ostart: Option[Rep[Int]] = start.i.fold(fa => Some(fa), fb => None)
      val oend: Option[Rep[Int]] = end.i.fold(fa => Some(fa), fb => None)
      DynSelectionHeader(x,ostart,oend)
    }

    def getStatSkel() = {
      val ostart: Option[Int] = start.i.fold(fa => None, fb => Some(fb))
      val oend: Option[Int] = end.i.fold(fa => None, fb => Some(fb))
      StatSelectionHeader(ostart,oend)
    }
  }

  case class DynSelectionHeader(x: Rep[Vector[Int]], start: Option[Rep[Int]], end: Option[Rep[Int]])
  case class StatSelectionHeader(start: Option[Int], end: Option[Int])

  implicit def exposeDynSelectionHeader(stat: StatSelectionHeader): ExposeRep[DynSelectionHeader] =
    new ExposeRep[DynSelectionHeader]() {

        val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
          val fs = if(stat.start.isEmpty) Vector(Arg[Int]) else Vector.empty
          val fe = if(stat.end.isEmpty) Vector(Arg[Int]) else Vector.empty
          Vector(Arg[Vector[Int]]) ++ fs ++ fe
        }
        val vec2t: Vector[Exp[_]] => DynSelectionHeader = (in: Vector[Exp[_]]) => {
          val x = in.head.asInstanceOf[Rep[Vector[Int]]]

          val (ostart, outstart) = if (stat.start.isEmpty) (Some(in.tail.head.asInstanceOf[Rep[Int]]), in.tail.tail) else (None, in.tail)
          val (oend, outend) = if (stat.end.isEmpty) (Some(outstart.head.asInstanceOf[Rep[Int]]),outstart.tail) else (None,outstart)
          DynSelectionHeader(x,ostart,oend)
        }

      val t2vec: DynSelectionHeader => Vector[Exp[_]] = (in: DynSelectionHeader) => {
        val vstart = in.start.map(p => Vector(p)).getOrElse(Vector.empty)
        val vend = in.end.map(p => Vector(p)).getOrElse(Vector.empty)
        Vector(in.x) ++ vstart ++ vend
      }

  }


  implicit def exposeTuple[A: TypeRep,B: TypeRep](): ExposeRep[(Rep[A],Rep[B])] = new ExposeRep[(Rep[A],Rep[B])] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) =>  Vector(Arg[A],Arg[B])
    val vec2t: Vector[Exp[_]] => ((Exp[A],Exp[B])) = (in: Vector[Exp[_]]) => {
      val a = in.head.asInstanceOf[Exp[A]]
      val b = in.tail.head.asInstanceOf[Exp[B]]
      (a,b)
    }
    val t2vec: ((Exp[A],Exp[B])) => Vector[Exp[_]] = (in: ((Exp[A],Exp[B]))) =>  Vector(in._1,in._2)
  }


  def mf(expose: ExposeRep[DynSelectionHeader], innerf: => (DynSelectionHeader => Rep[Vector[Int]])): StagedFunction[DynSelectionHeader, Rep[Vector[Int]]] = {
    val f: (DynSelectionHeader => Rep[Vector[Int]]) = (wuf: DynSelectionHeader) => innerf(wuf)
    println(f)
    val t: StagedFunction[DynSelectionHeader, Rep[Vector[Int]]] = doGlobalLambda(f, true)(expose, exposeRepFromRep[Vector[Int]])
    t
  }


  def checksize(x: SInt) = {

  }


  def sort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      val mix = SelectionHeader(stat,dyn)
      val size = mix.start - mix.end
      val sizerep = size.toRep()
      val cond: Rep[Boolean] = ordering_gt(sizerep,Const(10))
      myifThenElse(cond, {
        quicksort(mix)
      }, {
        selectionsort(mix)
      } )
    }
    outer
  }

  def quicksort(sh: SelectionHeader): Rep[Vector[Int]] = {
    /*
    val statskel = sh.getStatSkel()
    val lessexpose = exposeDynSelectionHeader(statskel)
    val dyn = sh.getDynSelectionHeader()

    val fless = mf(lessexpose, sort(statskel))
    val sless = fless(dyn)
    sless */


    val start = sh.start
      val end = sh.end
      val x = sh.x
      val half = (end - start) / 2
      val pivot = x(half)


      val less = filter[Int](x, p => pivot > p)
      val equal = filter[Int](x, p => pivot equiv p)
      val greater = filter[Int](x, p => pivot < p)


/*
      val less = x
      val equal = x
      val greater = x
*/

    val sh1 = SelectionHeader(less, sh.start, half - 1)
    val statsless = sh1.getStatSkel()
    val lessexpose = exposeDynSelectionHeader(statsless)
    val dynless = sh1.getDynSelectionHeader()

    val fless = mf(lessexpose, sort(statsless))
    val sless = fless(dynless)



    val sh2 = SelectionHeader(greater, half + 1, sh.end)
    val statgreater = sh2.getStatSkel()
    val greaterexpose = exposeDynSelectionHeader(statgreater)
    val fgreater = mf(greaterexpose, sort(statgreater))
    val dyngreater = sh2.getDynSelectionHeader()
    val sgreater = fgreater(dyngreater)

    /*
      val sh1 = SelectionHeader(less, sh.start, half - 1)
      val lessexpose = exposeDynSelectionHeader(sh1.getStatSkel())
      val fless = mf(lessexpose, sort(sh1.getStatSkel()))
      val sless = fless(sh1.getDynSelectionHeader())
  */

    /*
     */

      concat(concat(sless, equal), sgreater)

      //concat(equal,equal)

      //sgreater
  }

  def selectionsort(sh: SelectionHeader): Rep[Vector[Int]] = {
    val start = sh.start
    val end = sh.end
    val x = sh.x
    implicit val itupexpose = exposeTuple[Int,Int]()

    (start until end).foldLeft(x){
      case (array, index) => {
        val swapele = array(index)
        val (value,pos) = ( (index ) until end).foldLeft( (swapele,index.toRep()) ){
          case ( (value,pos), index2) => {
            val b = array(index2)
            val t: Rep[Boolean] = value < b
            myifThenElse[(Rep[Int],Rep[Int])](t, (b,index2.toRep()) , (value,pos))
          }
        }
        val bx = array(pos)
        array.update(index,bx).update(pos,swapele)

      }
    }
  }


  def f(x: Rep[Int]): Rep[Int] = x

  def graphvizexport() = {
    val ini: StatSelectionHeader = StatSelectionHeader(None,None)
    val (code, cm) = emitGraph.emitDepGraphf(sort(ini))(exposeDynSelectionHeader(ini), exposeRepFromRep[Vector[Int]])
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    stream.println(code)
    stream.flush()
    stream.close()
  }

  def codeexport() = {
    val ini: StatSelectionHeader = StatSelectionHeader(None,None)
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println("import org.scalacheck._\nimport org.scalacheck.Properties\nimport org.scalacheck.Prop.forAll\n\nobject Bla extends org.scalacheck.Properties(\"Sort\"){\n\n  property(\"startsWith\") = forAll { (v: Vector[Int]) =>\n    val c = new testClass\n    val s = c(v, 0, v.length)\n    //val s2 = test(v,0,v.length)\n    val s3 = sortFunctional(v)\n\n    s3.corresponds(s){_ == _}\n\n  }\n\n  def sortFunctional(xs: Vector[Int]): Vector[Int] = {\n    if (xs.length <= 1) xs\n    else {\n      val pivot = xs(xs.length / 2)\n      val less = xs.filter(p => pivot > p)\n      val equal = xs.filter(p => pivot == p)\n      val greater = xs.filter(p => pivot < p)\n      sortFunctional(greater) ++  equal ++ sortFunctional(less)\n    }\n  }\n\n\n\n\n  def test(v: Vector[Int], start: Int, end: Int): Vector[Int] = {\n\n    (start until end).foldLeft(v) {\n      (acc,ele) => {\n        val swapele = acc(ele)\n        val (value, pos) = (ele until end).foldLeft((swapele,ele)){\n          (acc2,k) => {\n            val (value, pos) = acc2\n            val currcheck = acc(k)\n            if (swapele < currcheck)\n              (currcheck,k)\n            else\n              (value,pos)\n          }\n        }\n        val bx = acc(pos)\n        val o1 = acc.updated(pos,swapele)\n        val o2 = o1.updated(ele,value)\n        o2\n      }\n    }\n  }\n\n}\n//bla!")
    val esc = codegen.emitSource(sort(ini), "testClass", stream2)(exposeDynSelectionHeader(ini), exposeRepFromRep[Vector[Int]])
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}
