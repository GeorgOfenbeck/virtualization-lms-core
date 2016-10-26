package Karatsuba



import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class Core extends sort.Skeleton {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with sort.ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }





  def multiply(me: MyBigInt, that: MyBigInt): MyBigInt = {
    val t = _if()

    me
  }

  def multiplyByInt(x: Array[Int], y: Int, sign: Int): MyBigInt = ???

  def multiplyToLen(x: Array[Int], xlen: Int,y : Array[Int], ylen: Int): Array[Int] = ???

  def multiplyKaratsuba(x: MyBigInt, y: MyBigInt): MyBigInt = ???



  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println("")
    //val esc = codegen.emitSource(tmp(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[Vector[Int]])
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}

