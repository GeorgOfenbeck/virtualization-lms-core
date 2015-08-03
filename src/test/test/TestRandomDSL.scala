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
//with GenRandomIFThenElse
//with GenRandomFunctions
with ScalaCompile {
  self =>
  override val codegen = new ScalaCodegen
    with EmitHeadInternalFunctionAsClass
    with ScalaGenBooleanOps
    with ScalaGenPrimitivOps
    with ScalaGenIfThenElse {
    val IR: self.type = self
  }
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
}

import org.scalacheck.Gen.{choose, numChar, alphaChar}
import org.scalatest._
import prop._

object TestRandomDSL extends org.scalacheck.Properties("MySpec") {

  import org.scalacheck.{Gen, Prop, Arbitrary}


  val desc = CodeDescriptor(100, 5, 5, 100, 20)


  def genNewDSL(): Gen[MRandomClass] = {
    for {
      dsl <- new MRandomClass
    }
      yield dsl
  }

  abstract class DSLwCode {
    val dsl: MRandomClass
    val code: Vector[dsl.FNest]
  }

  def genDSLwCode(desc: CodeDescriptor): Gen[DSLwCode] = org.scalacheck.Gen.lzy {
    for {
      dslr <- new MRandomClass
      coder <- dslr.genCode(desc)
    } yield new DSLwCode {
      override val dsl: dslr.type = dslr
      override val code: Vector[dslr.FNest] = coder
    }
  }

  implicit val shrinkCode: Shrink[DSLwCode] = Shrink({
    case dslwcode: DSLwCode => {
      import org.scalacheck.Shrink
      import org.scalacheck.Shrink.shrink
      println("shrinking")
      if (dslwcode.code.size > 2) {
        val res = new DSLwCode {
          override val dsl: dslwcode.dsl.type = dslwcode.dsl
          override val code: Vector[dslwcode.dsl.FNest] = dslwcode.code.dropRight(1)
        }
        println("returning smaller code")
        val x = Stream.concat(Stream(res), org.scalacheck.Shrink.shrink[DSLwCode](res))
        //println(x.size)
        x
      }
      else Stream.empty
    }
  })





  /*def genDSLCode(dsl: MRandomClass, desc: CodeDescriptor): Gen[Vector[dsl.FNest]] = {
   for {code <- dsl.genCode(desc)} yield code
  }*/
  /*
   implicit def getdslshrink(dsl: MRandomClass): Shrink[Vector[dsl.FNest]] = {
    println("trigger implicit")
    dsl.ShrinkMe.shrinkCode
   }*/

  property("my prop test ") =
    Prop.forAll(genDSLwCode(desc)) {
      dslwcode => {
        import dslwcode._
        /*val dsl = dslwcode.dsl
        val dslwcode.code = dslwcode.code*/
        val inisyms = dslwcode.code.head.syms
        val resultsyms = dslwcode.code.last.syms

        //Prop.forAll(dsl.genArgInstances(inisyms)) {
        Prop.forAll(dsl.hideit(inisyms)) {
          stealth =>
            val rargs = stealth.x
            /*Prop.forAll(genNewDSL()) { dsl: MRandomClass => {
             Prop.forAll (genDSLCode(dsl, desc)) {*/

            //val rargs = dsl.genArgInstances(inisyms).sample.get

            val callstack = dsl.chainHeadf(dslwcode.code)

            val callstack_staged = dsl.chainHeadsf(dslwcode.code)

            val exposeargs = dsl.genExposeRep(inisyms)
            val exposeres = dsl.genExposeRep(resultsyms)

            /*val (code, cm) = dsl.emitGraph.emitDepGraphf(callstack_staged)(exposeargs, exposeres)
            val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check.dot"))
            stream.println(code)
            stream.flush()
            stream.close()*/

            var worked = true


            try {
              val (compiled_staged, esc2) = dsl.compile(callstack_staged)(exposeargs, exposeres)
              println("-----")
              println(rargs)
              println(callstack(rargs))
              println(compiled_staged(rargs))
              println("-----")
            }
            catch {
              case ex : Throwable => {

                val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
                val esc = dsl.codegen.emitSource(callstack_staged, "testClass", stream2)(exposeargs, exposeres)
                stream2.flush()
                stream2.close()
                println("caught")
                println("msg: " + ex.getMessage)

                worked = false
              }
            }

            /*println("args!")
       println(rargs)
       println("args + result")*/
            //println(callstack(rargs))


            worked
        }
      }
    }
}
