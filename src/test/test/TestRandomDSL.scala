package test


import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


class MRandomClass extends BooleanOpsExp
with PurePrimitiveOpsExp
with IfThenElseExp
with ImplicitOpsExp
with InternalFunctionsExp

with GenRandomBooleanOps
with GenRandomPrimitiveOps
with GenRandomIFThenElse
with ScalaCompile {
 self =>
 override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenBooleanOps with ScalaGenIfThenElse{
  val IR: self.type = self
 }
 val emitGraph = new GraphVizExport {
  override val IR: self.type = self
 }
}

import org.scalacheck.Gen.{choose, numChar, alphaChar}
import org.scalatest._
import prop._

class TestRandomDSL extends PropSpec with PropertyChecks {


 val dsl = new MRandomClass
 println("hello dude")
 val desc = CodeDescriptor(100,10)
 val test1 = dsl.genCode(desc).sample.get
 val inisyms = test1.head.syms
 val resultsyms = test1.last.syms
 val rargs = dsl.genArgInstances(inisyms).sample.get

 val callstack = dsl.chainHeadf(test1)

 val callstack_staged = dsl.chainHeadsf(test1)

 val exposeargs = dsl.genExposeRep(inisyms)
 val exposeres = dsl.genExposeRep(resultsyms)

 val (code, cm) = dsl.emitGraph.emitDepGraphf(callstack_staged)(exposeargs,exposeres)
 val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check.dot"))
 stream.println(code)
 stream.flush()
 stream.close()
 //dsl.compile(callstack_staged)


 println("args!")
 println(rargs)
 println("args + result")
 println(callstack(rargs))



}
