
package RandomTesting

import java.io.{PrintWriter, StringWriter}

import org.scalacheck.Shrink

import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util


trait RandomClass extends GenRandomOps with ScalaCompile {
  self =>
  val codegen: EmitHeadInternalFunctionAsClass { val IR: self.type }
  var tuplercount: Int = 0
  var detuplercount: Int = 0
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }



  def tupler(x: Vector[SoV[NoRep, _]]) = {
    val argtuple = codegen.tupledeclarehelper(x.map(a => codegen.remap(a.tag.mf)), "")

    def workaround(a: SoV[NoRep, _]): String = {
      val longstring = manifest[Long].toString()
      val floatstring = manifest[Float].toString()
      val doublestring = manifest[Double].toString()
      a.tag.mf.toString() match {
        case `longstring` => "(" + a.sym.toString + "L).asInstanceOf[" + codegen.remap (a.tag.mf) + "]"
        case `floatstring` => "(" + a.sym.toString + "f).asInstanceOf[" + codegen.remap (a.tag.mf) + "]"
        case `doublestring` => "(" + a.sym.toString + "D).asInstanceOf[" + codegen.remap (a.tag.mf) + "]"
        case _ => "(" + a.sym.toString + ").asInstanceOf[" + codegen.remap (a.tag.mf) + "]"
      }
    }

    val withvalues = codegen.tupledeclarehelper(x.map(a => workaround(a) ), "")
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
  def detupler(y: Vector[GenTypes[_]]) = {
    val rettuple = codegen.tupledeclarehelper(y.map(a => codegen.remap(a.mf)), "")
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

  var failvec:Option[Vector[Boolean]] = None
  var shrink = false


  //the actual test object has to define what Random Class is returned
  def iniRandomC(): RandomClass

  def getCodeDescription(randomClass: RandomClass): randomClass.CodeDescriptor

  def iniCCStatus(randomClass: RandomClass): randomClass.CCStatus = {
    randomClass.CCStatus(0,0,0,Map.empty, Map.empty)
  }

  abstract class DSLwCode {
    val dsl: RandomClass
    //val code: Vector[dsl.EvalGenIterStep[dsl.NoRep]]
    //val symbolic_code: Vector[dsl.EvalGenIterStep[dsl.Rep]]
    val dag: dsl.Dag
  }

  //had to introduce this indirection cause otherwise ScalaCheck will assume the class constant
  def genDSL(): Gen[RandomClass] = {
    def workaround(i: Int) = iniRandomC()
    for {
      nr <- Gen.posNum[Int]
    } yield workaround(nr)
  }


  def genDSLwCode(): Gen[DSLwCode] = //org.scalacheck.Gen.lzy {
  {
    println("here")
    for {
      dslr <- genDSL()
      (status,rdag) <- dslr.genCode(getCodeDescription(dslr),iniCCStatus(dslr))
      //(codev, codes) <- dslr.genCode(getCodeDescription(dslr),iniCCStatus(dslr))
    } yield new DSLwCode {
      override val dsl: dslr.type = dslr
      override val dag: dslr.Dag = rdag
      //override val code: Vector[dslr.FNest] = coder
      //override val code: Vector[dsl.EvalGenIterStep[dsl.NoRep]] = codev
      //override val symbolic_code: Vector[dsl.EvalGenIterStep[dsl.Rep]] = codes
    }
  }


  /*def fstream(curr: Int, ini: DSLwCode): Stream[DSLwCode] = {
    val s = ini.code.size
    val rest = s - curr
    val halfrest = rest / 2
    val newpos = curr + halfrest
    if (halfrest > 0) {
      val x = new DSLwCode {
        override val dsl: ini.dsl.type = ini.dsl
        override val symbolic_code: Vector[ini.dsl.EvalGenIterStep[ini.dsl.Exp]] = ini.symbolic_code.take(newpos)
        override val code: Vector[ini.dsl.EvalGenIterStep[ini.dsl.NoRep]] =  ini.code.take(newpos)
      }
      Stream.cons(x, fstream(newpos, ini))
    }
    else
      Stream.empty
  }
*/

  def fstream(curr: Int, ini: DSLwCode): Stream[DSLwCode] = {
    val s = ini.dag.opLookUp.opargsrets.size
    val rest = s - curr
    val halfrest = rest / 2
    val newpos = curr + halfrest
    if (halfrest > 0) {
      val x = removetill(ini,newpos)
      Stream.cons(x, fstream(newpos, ini))
    }
    else
      Stream.empty
  }

  def removetill ( s: DSLwCode, half: Int): DSLwCode = {
    val possible_removes = s.dag.OpswithoutDep()
    val ndag = s.dag.removeOp(possible_removes.head)
    val x = new DSLwCode {
      override val dsl: s.dsl.type = s.dsl
      override val dag =  ndag
    }
    if (x.dag.opLookUp.opargsrets.size > half)
      removetill(x,half)
    else
      x
  }




   implicit val shrinkCode: Shrink[DSLwCode] = Shrink({
    case s: DSLwCode => {
      println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!shrinking" + s.dag.opLookUp.opargsrets.size)
      shrink = true
      if (failvec.isDefined) {
        println("ENTER!")
        //in this case we actually can speed up the process since we can easily determine the first failing symbol

        val firstresultid = s.dag.opLookUp.ret2rets.keySet.min

        val failvecdefined = failvec.get
        val t = failvecdefined.takeWhile(p => p)
        val failid = t.size + firstresultid
        val firstfail = t :+ failvecdefined(t.size) //this should add the false boolean

        val debug = s.dag.opLookUp.ret2rets
        val failrets = debug(failid)
        val failop = s.dag.opLookUp.rets2op(failrets)
        val ndag = s.dag.removetillOp(failop)

        if (ndag.dag.size == s.dag.dag.size) {
          println("cannot shrink anymore")
          Stream.empty
        }
        else {
          val x = new DSLwCode {
            override val dsl: s.dsl.type = s.dsl
            override val dag = ndag
          }
          Stream(x)
        }
      }
      else {
        val inisyms = s.dag.dag(0).toVector.sortWith((a, b) => a.id < b.id).map(e => e.typ)
        val resultsyms = s.dag.dag.flatMap(l => l.toVector).sortWith((a, b) => a.id < b.id).map(e => e.typ)
        val (callstack, callstack_staged) = s.dsl.chainDag(s.dag)
        val exposeargs = s.dsl.genExposeRep(inisyms)
        val exposeres = s.dsl.genExposeRep(resultsyms)
        val possible_removes = s.dag.OpswithoutDep()
        if (possible_removes.isEmpty)
          Stream.empty
        else {
          val half = s.dag.opLookUp.opargsrets.size / 2
          val x = removetill(s, half)
          Stream.concat(Stream(x), fstream(half, s)) //Stream of size 3/4, 7/8 etc.
        }
      }
    }
  })


  property("my prop test" ) =
    Prop.forAll(genDSLwCode()) {
      dslwcode => {
        import dslwcode._
        import scala.lms.util._
        var worked = true
        failvec = None


        val inisyms = dslwcode.dag.dag(0).toVector.sortWith((a,b) => a.id < b.id).map(e => e.typ)
        val resultsyms = dslwcode.dag.dag.flatMap(l => l.toVector).sortWith((a,b) => a.id < b.id).map(e => e.typ)
        //val (callstack, callstack_staged) = dsl.chainHeadf(dslwcode.code)
        val (callstack, callstack_staged) = dsl.chainDag(dslwcode.dag)
        /*val inisyms = dslwcode.code.head.types
        val resultsyms = dslwcode.code.last.types
        val callstack = dsl.chainHeadf(dslwcode.code)
        val callstack_staged = dsl.chainHeadf(dslwcode.symbolic_code)*/
        val exposeargs = dsl.genExposeRep(inisyms)
        val exposeres = dsl.genExposeRep(resultsyms)


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
            //val castit = asvec.asInstanceOf[Vector[dsl.SoV[dsl.NoRep, _]]]
            val castit = asvec.asInstanceOf[Vector[dsl.NoRep[_]]]
            val widx = castit.zipWithIndex

            if (true) {
              //should we compare with unstaged?
              val unstagedresult = callstack(rargs)
              def NanCheck[T](x: T): Boolean = {
                x match {
                  case  t: Double => t.isNaN
                  case  t: Float => t.isNaN
                  case _ => false
                }
              }
              val dropit = widx.dropWhile(ele => {
                val (e, idx) = ele
                val t = unstagedresult(idx).sym
                if (e != unstagedresult(idx).sym)
                  {
                    println("Input")
                    rargs.map(e => println(e.sym))
                    println("Difference")
                    println(e)
                    println(t)
                  }

                e == t || t.toString().contains("function") || (NanCheck(t) && NanCheck(e))
              })
              if (!dropit.isEmpty || shrink) {

                val diff = widx.map(ele => {
                  val (e, idx) = ele
                  e == unstagedresult(idx).sym || unstagedresult(idx).sym.toString().contains("function")
                })
                failvec = Some(diff)

                println("dropit: " + dropit)
                val file = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Fail" + cnt + ".scala")
                val stream2 = new java.io.PrintWriter(file)
                val esc = dsl.codegen.emitSource(callstack_staged, "failClass" + cnt, stream2)(exposeargs, exposeres)
                stream2.flush()
                stream2.close()
                file.flush()
                file.close()
              }
              shrink = false
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
            shrink = true
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
            println(ex.printStackTrace())
            val trace = ex.getStackTrace
            println(trace)
            worked = false
          }
        }

        worked
      }
  }
}
