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



  object SRange{

  }




  def selectionsort(x: Vector[Int], start1: SInt, end1: SInt): Rep[ComplexVector] = {
    val start = 0
    val end = x.size
    (start until end).foldLeft(x){
      (array, index) => {
        val ab = x(index)
        val pos: Int = (index+1 until end).foldLeft(index){
          (iarray, index2) => {
            val b = x(index2)
            if (ab < b) index2 else index
          }
        }
        val ax = x(index)
        val bx = x(pos)

        val up1 = x.updated(index,bx)
        up1.updated(pos,ax)
      }
    }
  }


  def f(x: Rep[Int]): Rep[Int] = x

  def graphvizexport() = {
    val (code, cm) = emitGraph.emitDepGraphf(f)
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    stream.println(code)
    stream.flush()
    stream.close()
  }

  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    val esc = codegen.emitSource(f, "testClass", stream2)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}
