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

  case class MaybeSFunction(f: Either[StagedFunction[DynFilterHeader, Rep[ImageH]], DynFilterHeader => Rep[ImageH]]) {
    def apply(dyn: DynFilterHeader): Rep[ImageH] = f.fold(fa => fa(dyn), fb => fb(dyn))
  }

  object MaybeSFunction {
    def apply(f: StagedFunction[DynFilterHeader, Rep[ImageH]]): MaybeSFunction = MaybeSFunction(Left(f))

    def apply(f: (DynFilterHeader => Rep[ImageH])): MaybeSFunction = MaybeSFunction(Right(f))
  }



  def multiply(stat: StatFilterHeader): MaybeSFunction = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._
      mix.image_in
/*
      val nmix = mix.cpy(image_in = image_out, image_out = image_in)
      val (nstat,ndyn) = nmix.split()
      val f = multiply(nstat)
      f(ndyn)*/
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynFilterHeader, Rep[ImageH]] = doGlobalLambda(stageme, Some("FilterMult" + stat.genSig()), Some("FilterMult" + stat.genSig()))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def entry(stat: StatFilterHeader): (DynFilterHeader => Rep[ImageH]) = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._
      val (nstat,ndyn) = mix.split()
      val f = multiply(nstat)
      val x = f(ndyn)
      x
    }
    stageme
  }

  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\TestFilter.scala"))
    stream2.println(codefrag)
    val inline = InlineInfo(false, 10, true, false, true, 0)
    val ini: StatFilterHeader = StatFilterHeader[Int,Int,Int,Int,Int,Int,Int,Int,Int](inlineInfo = inline)
    //val ini = StatFilterHeader[Int, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep](Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), inline)
    //val ini: StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep] = StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](Const(-1), Const(-1), Const(-1),Const(-1))
    val esc = codegen.emitSource(entry(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[ImageH])
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

  val codefrag = "\npackage Filter2\n\nclass ImageH(val ele: Int){\n  def get(x: Int, y: Int): Int = ele\n  def set(x: Int, y: Int, p: Int) = this\n}\n"


}