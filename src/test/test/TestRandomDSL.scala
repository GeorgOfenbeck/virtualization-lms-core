package test


import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.scalalike._


class MRandomClass extends BooleanOpsExp
with PurePrimitiveOpsExp
with ImplicitOpsExp
with InternalFunctionsExp
with GenRandomBooleanOps
with GenRandomPrimitiveOps
with ScalaCompile {
 self =>
 override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenBooleanOps{
  val IR: self.type = self
 }
}

import org.scalacheck.Gen.{choose, numChar, alphaChar}
import org.scalatest._
import prop._

class TestRandomDSL extends PropSpec with PropertyChecks {


 val bla = new MRandomClass

 println("hello dude")

 val desc = CodeDescriptor(10,10)

 val test1 = bla.genCode(desc).sample.get
 val inisyms = test1.head.syms
 val rargs = bla.genArgInstances(inisyms).sample.get

 val callstack = bla.chainHeadf(test1)

 val callstack_staged = bla.chainHeadsf(test1)

 val expose = bla.genExposeRep(inisyms)
 bla.compile(callstack_staged)(expose,expose)


 println("args!")
 println(rargs)
 println("args + result")
 println(callstack(rargs))



}
