package Filter2



import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class Core extends FilterHeader {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with sort.ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }

  def multiply(stat: StatFilterHeader): (DynFilterHeader => Rep[ImageH]) = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._

      mix.image_out
    }
    stageme
  }

  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\TestFilter.scala"))
    //stream2.println(codefrag)
    val inline = InlineInfo(false, 10, true, false, true, 0)
    val ini: StatFilterHeader = StatFilterHeader[Int,Int,Int,Int,Int,Int,Int,Int,Int]()
    //val ini = StatFilterHeader[Int, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep](Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), inline)
    //val ini: StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep] = StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](Const(-1), Const(-1), Const(-1),Const(-1))
    val esc = codegen.emitSource(multiply(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[ImageH])
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }


}