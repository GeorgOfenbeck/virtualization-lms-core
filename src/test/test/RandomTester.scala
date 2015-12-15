package test

import java.io.{PrintWriter, StringWriter}

import org.scalacheck.Shrink

import scala.lms.targets.scalalike._
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util


trait RandomClass extends GenRandomOps with ScalaCompile {
  self =>
  val codegen: EmitHeadInternalFunctionAsClass { val IR: self.type }
  var tuplercount: Int = 0
  var detuplercount: Int = 0


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

  def detupler(y: Vector[cTP]) = {
    val rettuple = codegen.tupledeclarehelper(y.map(a => codegen.remap(a.tag.mf)), "")
    val withindex = y.zipWithIndex
    val withvalues = withindex.map(ele => {
      val (e, idx) = ele
      codegen.tupleaccesshelper(idx, "helper", idx == withindex.size-1)
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





abstract class RandomTester extends org.scalacheck.Properties("Random Testing"){

  import org.scalacheck.{Gen, Prop, Arbitrary}

  //the actual test object has to define what Random Class is returned
  def iniRandomC(): RandomClass
  lazy val desc: CodeDescriptor = CodeDescriptor(100, 2, 20, 5, 20, 20 , 1)



  implicit def getdslwshrink(dsl: DSLwCode): Shrink[DSLwCode] = {
    println("trigger implicit dslw")
    ???
  }

  implicit def getdslwshrink1(dsl: DSLwCode): Shrink[Vector[dsl.dsl.FNest]] = {
    println("trigger implicit dslw")
    ???
  }

  implicit def getdslwshrink2(dsl: DSLwCode): Shrink[RandomClass] = {
    println("trigger implicit dslw")
    ???
  }


   implicit def getdslshrink(dsl: RandomClass): Shrink[Vector[dsl.FNest]] = {
    println("trigger implicit")
     dsl.shrinkCodeX
   }

  implicit def shrinkIt(dsl: RandomClass): Shrink[RandomClass] = {
    println("trigger implicit with Shrink empty")
    ???
  }

  implicit def shrinkIt2(dsl: RandomClass): Shrink[DSLwCode] = {
    println("trigger implicit with Shrink empty dslwcode")
    ???
  }

  implicit val shrinkStuff: Shrink[DSLwCode] = Shrink(
    {
      case dslw: DSLwCode => {
        println("shrinking")
        ???

      }
    })


  abstract class DSLwCode {
    val dsl: RandomClass
    val code: Vector[dsl.FNest]
  }

  //had to introduce this indirection cause otherwise ScalaCheck will assume the class constant
  def genDSL(): Gen[RandomClass] = {
    def workaround(i: Int) = iniRandomC()
    for {
      nr <- Gen.posNum[Int]
    } yield workaround(nr)
  }


  def genDSLwCode(desc: CodeDescriptor): Gen[DSLwCode] = //org.scalacheck.Gen.lzy {
  {
    println("here")
    for {
      dslr <- genDSL()
      coder <- {
        dslr.genCode(desc)
      }
    } yield new DSLwCode {
      override val dsl: dslr.type = dslr
      override val code: Vector[dslr.FNest] = coder
    }
  }

  property("my prop test" ) =
    Prop.forAll(genDSLwCode(desc)) {
      dslwcode => {
        import dslwcode._
        import scala.lms.util._
        val inisyms = dslwcode.code.head.syms
        val resultsyms = dslwcode.code.last.syms
        val callstack = dsl.chainHeadf(dslwcode.code)
        val callstack_staged = dsl.chainHeadsf(dslwcode.code)
        val exposeargs = dsl.genExposeRep(inisyms)
        val exposeres = dsl.genExposeRep(resultsyms)
        var worked = true
        var cnt = 0
        try {
          println("starting compilation")
          val (compiled_staged, esc2) = dsl.compile(callstack_staged)(exposeargs, exposeres)
          val stealth = dsl.hideit(inisyms).sample.get

          {
            println("trying args")
            val rargs = stealth.x

            println("tuple the input")
            val tupler = dsl.tupler(rargs)
            val tuple = tupler(rargs)

            println("calling the code")
            val rettuple = compiled_staged(tuple)
            //println(rettuple)
            println("detuple the output")
            val dtupler = dsl.detupler(resultsyms)
            val asvec = dtupler(rettuple)

            println("got the result")
            val castit = asvec.asInstanceOf[Vector[Any]]
            val widx = castit.zipWithIndex

            if (true) {
              //should we compare with unstaged?
              val unstagedresult = callstack(rargs)
              val dropit = widx.dropWhile(ele => {
                val (e, idx) = ele
                e == unstagedresult(idx).sym || unstagedresult(idx).sym.toString().contains("function")
              })
              if (!dropit.isEmpty)
                println("dropit: " + dropit)
              worked = dropit.isEmpty
            }
            /*val file = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test" + cnt + ".scala")
            val stream2 = new java.io.PrintWriter(file)
            val esc = dsl.codegen.emitSource(callstack_staged, "testClass" + cnt, stream2)(exposeargs, exposeres)
            stream2.flush()
            stream2.close()
            file.flush()
            file.close()*/

            cnt = cnt + 1
          }

        }
        catch {
          case ex: Throwable => {
            println("in the catch!")
            val file = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Fail" + cnt + ".scala")
            val stream2 = new java.io.PrintWriter(file)
            val esc = dsl.codegen.emitSource(callstack_staged, "failClass" + cnt, stream2)(exposeargs, exposeres)
            stream2.flush()
            stream2.close()
            file.flush()
            file.close()
            println("caught")
            println("msg: " + ex.getMessage)
            val trace = ex.getStackTrace
            println(trace)
            worked = false
          }
        }

        worked
      }
  }
}