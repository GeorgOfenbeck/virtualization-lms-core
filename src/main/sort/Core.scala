package sort


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




  def sort(stat: StatSelectionHeader): (DynSelectionHeader => Rep[Vector[Int]]) = {
    
    val outer: (DynSelectionHeader => Rep[Vector[Int]]) = (dyn: DynSelectionHeader) => {
      
      val mix = SelectionHeader(stat,dyn)
      selectionsort(mix)
    }
    outer
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


  def selectionsort(sh: SelectionHeader): Rep[Vector[Int]] = {
    val start = sh.start
    val end = sh.end
    val x = sh.x
    (start until end).foldLeft(x){
      (array, index) => {
        val ab = x(index)
        val pos: SInt = ( (index + 1) until end).foldLeft(index){
          (iarray, index2) => {
            val b = x(index2)
            val t: Rep[Boolean] = ab < b
            SInt(myifThenElse[Rep[Int]](t, index2.toRep() ,  index.toRep()))
          }
        }
        val ax = x(index)
        val bx = x(pos)
        val up1 = x.update(index,bx)
        up1.update(pos,ax)
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
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\Test.scala"))
    val esc = codegen.emitSource(f, "testClass", stream2)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}
