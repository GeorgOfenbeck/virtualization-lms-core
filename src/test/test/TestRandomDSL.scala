package test


import java.io.{PrintWriter, StringWriter}

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._
import scala.reflect.Manifest
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util


trait MyRemap extends ScalaCodegen {
  val IR: GenRandomOps with InternalFunctionsExp

  override def remap[A](m: Manifest[A]): String = {
    m match {
      case b: FunctionMarker => "scala.Function1[_ <: Any, _ <: Any]"
      case _ => super.remap(m)
    }
  }
}

class MRandomClass extends BooleanOpsExp
with PurePrimitiveOpsExp
with IfThenElseExp
with ImplicitOpsExp
with InternalFunctionsExp

with GenRandomBooleanOps
with GenRandomPrimitiveOps
//with GenRandomIFThenElse
with GenRandomFunctions
with ScalaCompile {
  self =>


  override val codegen = new MyRemap
    with EmitHeadInternalFunctionAsClass
    with ScalaGenBooleanOps
    with ScalaGenPrimitivOps
    with ScalaGenIfThenElse {
    val IR: self.type = self
  }
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }

  var tuplercount = 1

  def tupler(x: Vector[cTP]) = {
    val argtuple = codegen.tupledeclarehelper(x.map(a => codegen.remap(a.tag.mf)), "")
    val withvalues = codegen.tupledeclarehelper(x.map(a => "(" + a.sym.toString + ").asInstanceOf[" + codegen.remap(a.tag.mf) + "]"), "")
    if (this.compiler eq null)
      setupCompiler()
    val staticData: List[(codegen.IR.Exp[_], Any)] = List()
    val className = "tupler$" + tuplercount
    tuplercount += 1
    val source = new StringWriter()
    val writer = new PrintWriter(source)
    val stringheader =
      "/*****************************************\n" +
        "  Emitting Generated Code                  \n" +
        "*******************************************/\n" +
        "class " + className + "" +
        " extends (Vector[Any] =>" + argtuple + ") {" +
        "\ndef apply( v: Vector[Any]): (" + argtuple + ") = {\n" +
        "val x: (" + argtuple + ") = " + withvalues +
        "\nx\n}" +
        "}" +
        "\n/*****************************************\n" +
        "  End of Generated Code                  \n" +
        "*******************************************/"
    writer.println(stringheader)
    if (dumpGeneratedCode) println(source)
    val compiler = this.compiler
    val run = new compiler.Run
    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //      compiler.genJVM.outputDir = fileSystem
    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")
    reporter.reset
    //output.reset
    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)
    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure): _*)
    val obj: Any => Any = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]): _*).asInstanceOf[Any => Any]
    obj
  }

  var detuplercount = 1

  def detupler(y: Vector[cTP]) = {
    val rettuple = codegen.tupledeclarehelper(y.map(a => codegen.remap(a.tag.mf)), "")
    val withindex = y.zipWithIndex
    val withvalues = withindex.map(ele => {
      val (e, idx) = ele
      codegen.tupleaccesshelper(idx, "helper", idx == withindex.size)
    }).mkString(",")
    if (this.compiler eq null)
      setupCompiler()
    val staticData: List[(codegen.IR.Exp[_], Any)] = List()
    val className = "detupler$" + tuplercount
    tuplercount += 1
    val source = new StringWriter()
    val writer = new PrintWriter(source)
    val stringheader =
      "/*****************************************\n" +
        "  Emitting Generated Code                  \n" +
        "*******************************************/\n" +
        "class " + className + "" +
        " extends ((" + rettuple + ") => Vector[Any]) {" +
        "\ndef apply(helper: " + rettuple + "): (Vector[Any]) = {\n" +
        "val x: (Vector[Any]) = Vector(" + withvalues + ")" +
        "\nx\n}" +
        "}" +
        "\n/*****************************************\n" +
        "  End of Generated Code                  \n" +
        "*******************************************/"
    writer.println(stringheader)
    if (dumpGeneratedCode) println(source)
    //if (true) println(source)
    val compiler = this.compiler
    val run = new compiler.Run
    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    //      compiler.genJVM.outputDir = fileSystem
    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")
    reporter.reset
    //output.reset
    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)
    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure): _*)
    val obj: Any => Any = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]): _*).asInstanceOf[Any => Any]
    obj
  }


}

import org.scalacheck.Gen.{choose, numChar, alphaChar}
import org.scalatest._
import prop._

object TestRandomDSL extends org.scalacheck.Properties("MySpec") {


  import org.scalacheck.{Gen, Prop, Arbitrary}


  val desc = CodeDescriptor(10, 5, 5, 3, 2)


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

  /*implicit val shrinkCode: Shrink[DSLwCode] = Shrink({
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
  })*/


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

          val callstack = dsl.chainHeadf(dslwcode.code)
          val callstack_staged = dsl.chainHeadsf(dslwcode.code)
          val exposeargs = dsl.genExposeRep(inisyms)
          val exposeres = dsl.genExposeRep(resultsyms)
          var worked = true
        try {
          val (compiled_staged, esc2) = dsl.compile(callstack_staged)(exposeargs, exposeres)
          //Prop.forAll(dsl.genArgInstances(inisyms)) {
          Prop.forAll(dsl.hideit(inisyms)) {
            stealth =>
              val rargs = stealth.x

              /*Prop.forAll(genNewDSL()) { dsl: MRandomClass => {
             Prop.forAll (genDSLCode(dsl, desc)) {*/

              //val rargs = dsl.genArgInstances(inisyms).sample.get


              /*val (code, cm) = dsl.emitGraph.emitDepGraphf(callstack_staged)(exposeargs, exposeres)
            val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check.dot"))
            stream.println(code)
            stream.flush()
            stream.close()*/


              //println("-----")
              //println(rargs)
              //println()


              val tupler = dsl.tupler(rargs)
              val tuple = tupler(rargs)
              //println(tuple)
              val rettuple = compiled_staged(tuple)
              //println(rettuple)
              val dtupler = dsl.detupler(resultsyms)
              val asvec = dtupler(rettuple)
              //println(asvec)
              //println("-----")

              val castit = asvec.asInstanceOf[Vector[Any]]

              val widx = castit.zipWithIndex

              if (false) {
                //should we compare with unstaged?
                val unstagedresult = callstack(rargs)
                val dropit = widx.dropWhile(ele => {
                  val (e, idx) = ele
                  e == unstagedresult(idx).sym
                })
                println("dropit: " + dropit)
                worked = dropit.isEmpty
              }

              worked

          }
        }
              catch {
                case ex: Throwable => {
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