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

  def multiply(stat: StatFilterHeader): (DynFilterHeader => Rep[Image]) = {
    val exposarg: ExposeRep[DynFilterHeader] = ??? //exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[Image]
    val stageme: (DynFilterHeader => Rep[Image]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._

      ???
    }
    stageme
  }

}