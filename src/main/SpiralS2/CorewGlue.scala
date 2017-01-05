package SpiralS2

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.InvocationTargetException

/**
  * Created by rayda on 03-Jan-17.
  */
class CorewGlue (variant: BreakDown.Tree, lookup: Map[List[Int], (Int,Boolean,Boolean)], testsize: Int,
                 WHT: Boolean = true,
                 static_size: Option[Int] = None,
                 interleaved: Boolean = false,
                 thread: Boolean = false,
                 base_default: Int = 0,
                 twid_inline: Boolean = true,
                 twid_default_precomp: Boolean = true,
                 validate: Boolean = true
                ) extends Core(variant,lookup, testsize, WHT, static_size, interleaved, thread, base_default, twid_inline, twid_default_precomp){

  def iniGTSkeleton(n: Option[Int], precomp: Boolean): Stat = {
    if (n.isEmpty) {
      val idIM: StatIMH = new StatIMH(0, 1, 0)
      //new Stat(if(static_size.isDefined) static_size.get else sph(), 1, new Stat_GT_IM(idIM, idIM), 0, None,Some(4))
      //new Stat(sph2(), if (static_size.isDefined) static_size.get else sph(), 1, new Stat_GT_IM(idIM, idIM), 0, None, None, precomp, exposeComplexVector)
      val expose = if (interleaved) exposeInterleavedComplexVector else exposeComplexVector
      new Stat(sph2(), if (static_size.isDefined) static_size.get else sph(), 1, new Stat_GT_IM(idIM, idIM), 0, None, None, precomp, expose)
    } else ???

  }


  def compile(): (Unit => Unit) = {
    import scala.tools.nsc._
    import scala.tools.nsc.util._
    import scala.tools.nsc.reporters._
    import scala.tools.nsc.io._

    import scala.tools.nsc.interpreter.AbstractFileClassLoader


    def setupCompiler(): Global = {
      /*
        output = new ByteArrayOutputStream()
        val writer = new PrintWriter(new OutputStreamWriter(output))
      */
      val settings = new Settings()
      val pathSeparator = System.getProperty("path.separator")

      settings.classpath.value = this.getClass.getClassLoader match {
        case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
        case _ => System.getProperty("java.class.path")
      }
      settings.bootclasspath.value = Predef.getClass.getClassLoader match {
        case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
        case _ => System.getProperty("sun.boot.class.path")
      }
      settings.encoding.value = "UTF-8"
      settings.outdir.value = "."
      settings.extdirs.value = ""
      //settings.verbose.value = true
      // -usejavacp needed on windows?

      reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out)) //writer
      new Global(settings, reporter)
    }

    val compiler: Global = setupCompiler()
    var compileCount = 0

    var dumpGeneratedCode = false

    val className = "staged" + compileCount
    compileCount += 1

    val source = new StringWriter()
    val writer = new PrintWriter(source)
    val stream2 = writer

    dumpCode(stream2)
    val ingt = iniGTSkeleton(None, false)
    val esc = codegen.emitSource((ini(ingt)), "testClass", stream2)(exposeDyn(ingt), ingt.expdata)
    stream2.println("\n}\n")
    val ingtpre = iniGTSkeleton(None, true)
    val esc2 = codegen.emitSource((ini(ingtpre)), "PreCompute", stream2)(exposeDyn(ingtpre), ingt.expdata)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}}\n")

    if (dumpGeneratedCode) println(source)


    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("vfs", None)

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
    val loader = new scala.tools.nsc.util.AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass("SpiralS2")
    /*loader.loadClass("SpiralS2.Twiddle")
    loader.loadClass("SpiralS2.PreCompute")*/
    //val cons = cls.getCon//cls.getConstructor()

    val main = cls.getMethod("main", classOf[Array[String]])

    //val obj: Unit => Unit = cons.newInstance().asInstanceOf[Unit => Unit]
    val f: Unit => Unit = (u: Unit) => {
      try {
        main.invoke(null, Array.empty[String])
      }
      catch {
        case e: InvocationTargetException => {
          try {
            throw e.getCause
          }
          catch{
            case e: Exception => {
              println(e)
            }
          }
        }
        case _ => ???

      }
    }

    f
  }


  def codeexport () = {
    val stream2 = new java.io.PrintWriter (new java.io.FileOutputStream ("F:\\Phd\\git\\code\\SpiralSTarget\\src\\main\\Test.scala") )
    dumpCode (stream2)
    val ingt = iniGTSkeleton (None, false)
    val esc = codegen.emitSource ((ini (ingt) ), "testClass", stream2) (exposeDyn (ingt), ingt.expdata)
    stream2.println ("\n}\n")
    val ingtpre = iniGTSkeleton (None, true)
    val esc2 = codegen.emitSource ((ini (ingtpre) ), "PreCompute", stream2) (exposeDyn (ingtpre), ingt.expdata)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println ("\n}}\n")
    stream2.flush ()
    stream2.close ()
  }

  def dumpCode (stream2: java.io.PrintWriter) = {

    val data = if (!interleaved) "val input = (0 until size).foldLeft(new ComplexVector(new Array[Complex](size))) {\n          (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)\n        }\n        val out = new ComplexVector(new Array[Complex](size))" else
      "val input = (0 until size).foldLeft(new InterleavedVector(new Array[Double](2*size))) {\n          (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)\n        }\n        val out = new InterleavedVector(new Array[Double](2*size))"

    val call = if (interleaved) {
      if (static_size.isDefined ) {
        "apply(input.save, out.save,List.empty)\n val res =new InterleavedVector(resx) //, 0, instride, 0, instride, Vector.empty)"
      }
      else {
        "apply(input.save, out.save,List.empty, size) \n val res = new InterleavedVector(resx)//, 0, instride, 0, instride, Vector.empty)"
      }
    }  else

      if (static_size.isDefined ) {
      "apply(input, out,List.empty) \n val res = resx;//, 0, instride, 0, instride, Vector.empty)"
    }
    else {
      "apply(input, out,List.empty, size) \n val res = resx;//, 0, instride, 0, instride, Vector.empty)"
    }
    stream2.println ("\n\nimport scala.annotation.tailrec\nimport scala.util.Random\nimport org.scalameter._\n\nimport java.util.concurrent._\nimport scala.util.DynamicVariable\n\n\nobject SpiralS2 extends App {\nobject common {\n\n  val forkJoinPool = new ForkJoinPool\n\n  abstract class TaskScheduler {\n    def schedule[T](body: => T): ForkJoinTask[T]\n    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {\n      val right = task {\n        taskB\n      }\n      val left = taskA\n      (left, right.join())\n    }\n  }\n\n  class DefaultTaskScheduler extends TaskScheduler {\n    def schedule[T](body: => T): ForkJoinTask[T] = {\n      val t = new RecursiveTask[T] {\n        def compute = body\n      }\n      Thread.currentThread match {\n        case wt: ForkJoinWorkerThread =>\n          t.fork()\n        case _ =>\n          forkJoinPool.execute(t)\n      }\n      t\n    }\n  }\n\n  val scheduler =\n    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)\n\n  def task[T](body: => T): ForkJoinTask[T] = {\n    scheduler.value.schedule(body)\n  }\n\n  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {\n    scheduler.value.parallel(taskA, taskB)\n  }\n\n  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {\n    val ta = task { taskA }\n    val tb = task { taskB }\n    val tc = task { taskC }\n    val td = taskD\n    (ta.join(), tb.join(), tc.join(), td)\n  }\n}\nobject Settings {\n  val decompchoice: Map[List[Int],(Int,Boolean,Boolean)] = " + lookup.toString + "\n val validate = " + validate + " \n  //true\n  val WHT = " + WHT.toString + "\n  var sanitycheck: Int = 0\n  val n: Option[Int] = " + static_size + "\n\n  val standardConfig = config(\n    Key.exec.minWarmupRuns -> 100,\n    Key.exec.maxWarmupRuns -> 1000,\n    Key.exec.benchRuns -> 1000,\n    Key.verbose -> false\n  ) withWarmer (new Warmer.Default)\n}\n\n\nobject Twiddle {\n  var precompbuffer: Vector[Complex] = Vector.empty\n  var dprecompbuffer: Vector[Double] = Vector.empty\n  var precomp: Array[Complex] = null\n  var dprecomp: Array[Double] = null\n  var twindex = 0\n\n  def store(n: Int, d: Int, k: Int, i: Int): Complex = {\n    val t = Twiddle.apply(n,d,k,i)\n    precompbuffer = precompbuffer :+ t\n    t\n  }\n\n  def load(): Complex = {\n    val t = precomp(twindex)\n    twindex  = twindex + 1;\n    t\n  }\n  def dstore(n: Int, d: Int, k: Int, i: Int,re: Boolean): Double = {\n    val t: Double = if (re) Twiddle.apply(n,d,k,i).re else Twiddle.apply(n,d,k,i).im\n    dprecompbuffer = dprecompbuffer :+ t\n    t\n  }\n\n  def dload(): Double = {\n    val t = dprecomp(twindex)\n    twindex  = twindex + 1;\n    t\n  }\ndef parloop(size: Int, numTasks: Int, ini: ComplexVector, out: ComplexVector, body: ((ComplexVector, Int)) => ComplexVector): ComplexVector = {\n    val r = (0 to size by (size / (Math.min(numTasks, size))))\n    val ranges1 = (r zip r.tail)\n    val last: (Int, Int) = (ranges1.last._1, size)\n    val ranges = ranges1.dropRight(1) :+ last\n\n    def blub(r: Range): Unit ={\n      r.map(\n        p => {\n          val t = (ini,p)\n          body(t)\n        }\n      )\n    }\n\n    val tasks = ranges.map({ case (from, to) => common.task(blub(from until to)) })\n    tasks foreach {\n      _.join\n    }\n    out\n  }\n var TMap = Map.empty[(Int, Int, Int), ComplexVector]\n\n  object MathUtilities {\n\n    def dLin(N: Int, a: Double, b: Double): List[Double] = {\n      val t_array = new Array[Double](N)\n      for (i <- 0 until N)\n        t_array(i) = a * i + b\n      t_array.toList\n    }\n\n    def diagTensor(a: List[Double], b: List[Double]): List[Double] = {\n      val t_array = new Array[Double](a.size * b.size)\n      for (i <- 0 until a.size)\n        for (j <- 0 until b.size)\n          t_array(i * b.size + j) = a(i) * b(j)\n      t_array.toList\n    }\n  }\n\n  def apply(x: ComplexVector, n: Int, d: Int, k: Int): ComplexVector = {\n    val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))\n    val t = E(n)\n    val root_list_re = diag map (ele => t.re(ele.toInt * k))\n    val root_list_im = diag map (ele => t.im(ele.toInt * k))\n\n    for (i <- 0 until root_list_re.size) {\n      val u = Complex(root_list_re(i), root_list_im(i))\n      //val idx = vrep(yi)\n      val tx = x.apply(i)\n      x.update(i, tx * u)\n    }\n    x\n  }\n\n  def apply(n: Int, d: Int, k: Int, i: Int): Complex = {\n\n    if (!TMap.contains((n, d, k))) {\n      val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))\n      val t = E(n)\n      val root_list_re = diag map (ele => t.re(ele.toInt * k))\n      val root_list_im = diag map (ele => t.im(ele.toInt * k))\n\n      val cv = new ComplexVector(new Array[Complex](root_list_re.size))\n      for (i <- 0 until root_list_re.size) {\n        val u = Complex(root_list_re(i), root_list_im(i))\n        cv.update(i, u)\n      }\n      TMap = TMap + ((n, d, k) -> cv)\n    }\n    val cv = TMap.get((n, d, k))\n    cv.get(i)\n  }\n\n\n  def DFT(n: Int): Vector[ComplexVector] = {\n    val m = new Array[ComplexVector](n)\n    val k = 1\n    val t_e = E(n)\n    for (x <- 0 until n)\n      m(x) = new ComplexVector(new Array[Complex](n))\n    for (x <- 0 until n)\n      for (y <- 0 until n) {\n\n        m(x).update(y, new Complex(t_e.re(x * y * k), t_e.im(x * y * k)))\n      }\n    m.toVector\n  }\n\n\n  def WHT(n: Int, x: Int, y: Int): Complex = {\n    //1* 1*\n    //1* -1*\n\n\n    val t = if (n == 1) new Complex(1, 0) /*{\n      if (rx == 1 && ry == 1) new Complex(-1, 0) else\n    }*/\n    else {\n      val nx = x % (n / 2)\n      val ny = y % (n / 2)\n      if (x >= n / 2 && y >= n / 2)\n        Complex(-1, 0) * WHT(n / 2, nx, ny)\n      else\n        WHT(n / 2, nx, ny)\n    }\n    t\n\n  }\n\n  //this is the version that returns a single complex\n  def DFT(n: Int, x: Int, y: Int): Complex = {\n    val k = 1\n    val t_e = E(n)\n    new Complex(t_e.re(x * y * k), t_e.im(x * y * k))\n  }\n\n\n}\n\n\nobject E {\n  var EMap = Map.empty[Int, E]\n\n  def apply(n: Int): E = {\n    val eo = EMap.get(n)\n    eo.getOrElse({\n      val ne = new E(n)\n      EMap = EMap + (n -> ne)\n      ne\n    })\n  }\n}\n\nclass E(val n: Int) {\n  def Gcd[A](x: A, y: A)(implicit integral: Integral[A]): A = {\n    val t = scala.math.BigInt(integral.toLong(x))\n    val res = t.gcd(scala.math.BigInt(integral.toLong(y)))\n    x match {\n      case _: Int => res.toInt.asInstanceOf[A]\n      case _: Long => res.toLong.asInstanceOf[A]\n      case _: Short => res.toShort.asInstanceOf[A]\n    }\n  }\n\n  def NormalizeRational[A](x: A, y: A)(implicit integral: Integral[A]): (A, A) = {\n    val gcd = Gcd(x, y)\n    (integral.quot(x, gcd), integral.quot(y, gcd))\n  }\n\n  def normalize_2pi_shift(xin: Double, yin: Double): (Double, Double) = {\n    var (x, y) = NormalizeRational(Math.round(xin), Math.round(yin))\n    if ((x / y) < 0) {\n      val t: Long = Math.ceil(x.toDouble / y.toDouble / (-2.0)).toLong\n      x = x + 2 * t * y\n    } else {\n      val t = (Math.floor((x.toDouble - 2 * y.toDouble) / y.toDouble / 2.0) + 1).toLong;\n      x = x - 2 * y * t;\n    }\n    val (xp, yp) = NormalizeRational(x, y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_pi_over2_shift(xin: Double, yin: Double): (Double, Double) = {\n    val (x, y) = (Math.round(xin), Math.round(yin))\n    val (xp, yp) = NormalizeRational(2 * x - y, 2 * y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_pi_over2_reflection(xin: Double, yin: Double): (Double, Double) = {\n    val (x, y) = (Math.round(xin), Math.round(yin))\n    val (xp, yp) = NormalizeRational(y - 2 * x, 2 * y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_trig(sign: Int, trig: String, x: Double, y: Double): (Int, String, Double, Double, Double) = {\n    // normalization in 2Pi, achieving: 0 <= xn / yn <= 2\n    val (xn, yn) = normalize_2pi_shift(x, y)\n    if (xn > yn) {\n      trig match {\n        case \"sin\" => normalize_trig(sign * (-1), \"sin\", xn - yn, yn)\n        case \"cos\" => normalize_trig(sign * (-1), \"cos\", xn - yn, yn)\n      }\n    } else if (xn == yn) {\n      trig match {\n        case \"sin\" => (sign, \"sin\", xn, yn, sign * (+0.0))\n        case \"cos\" => (sign, \"cos\", xn, yn, sign * (-1.0))\n      }\n    } else {\n      if (xn > yn / 2) {\n        // normalization in Pi, achieving 0 <= xn / yn <= 1/2\n        val (xp, yp) = normalize_pi_over2_shift(xn, yn)\n        trig match {\n          case \"sin\" => normalize_trig(sign * (+1), \"cos\", xp, yp)\n          case \"cos\" => normalize_trig(sign * (-1), \"sin\", xp, yp)\n        }\n      } else if (xn == yn / 2) {\n        trig match {\n          case \"sin\" => (sign, \"sin\", xn, yn, sign * (+1.0))\n          case \"cos\" => (sign, \"cos\", xn, yn, sign * (+0.0))\n        }\n      } else {\n        // now reflect in Pi / 2, and make sure that 0 <= xn / yn <= 1/4\n        if (xn > yn / 4) {\n          val (xp, yp) = normalize_pi_over2_reflection(xn, yn)\n          trig match {\n            case \"sin\" => (sign, \"cos\", xp, yp, Double.MaxValue)\n            case \"cos\" => (sign, \"sin\", xp, yp, Double.MaxValue)\n          }\n        } else if (xn == yn / 4) {\n          (sign, \"cos\", 1.0, 4.0, Double.MaxValue)\n        } else {\n          if (xn == 0.0) {\n            trig match {\n              case \"sin\" => (sign, \"sin\", xn, yn, sign * (+0.0))\n              case \"cos\" => (sign, \"cos\", xn, yn, sign * (+1.0))\n            }\n          } else {\n            trig match {\n              case \"sin\" => (sign, \"sin\", xn, yn, Double.MaxValue)\n              case \"cos\" => (sign, \"cos\", xn, yn, Double.MaxValue)\n            }\n          }\n        }\n      }\n    }\n  }\n\n  private def valueSinOrCos(f: String, x: Double, y: Double): Double = {\n    val (sign, trig, xn, yn, value) = normalize_trig(1, f, x, y)\n    if (!value.equals(scala.Double.MaxValue)) {\n      value\n\n    } else {\n      trig match {\n        case \"sin\" => (xn, yn) match {\n          case (1.0, 6.0) => sign * 0.5\n          case _ => sign * Math.sin(xn * Math.PI / yn)\n        }\n        case \"cos\" => sign * Math.cos(xn * Math.PI / yn)\n      }\n    }\n  }\n\n  def SinPi(x: Double, y: Double): Double = valueSinOrCos(\"sin\", x, y)\n\n  def CosPi(x: Double, y: Double): Double = valueSinOrCos(\"cos\", x, y)\n\n  private def yieldk(n: Int) = {\n    //TODO - find short form for return value\n    def tmp() = {\n      for (k <- 0 until n\n           // this if checks if x^t becomes 1 before n==t, this is e.g. the\n           // case for 2nd root of unity of 4 where it becomes 1 at x^2\n           if (for (t <- 2 until n - 1\n                    if (Math.cos(2 * math.Pi * k * t / n) == 1)\n           ) yield 1).isEmpty\n      )\n        yield k\n    }\n\n    tmp.last\n  }\n\n  lazy val store = yieldk(n)\n\n  def re(p: Int): Double = {\n    val x = CosPi(2.0 * p * store, n)\n    x\n  }\n\n  def im(p: Int): Double = SinPi(2.0 * p * store, n) * -1.0\n}\n\n\ncase class Complex(val re: Double, val im: Double) {\n  def +(rhs: Complex): Complex = {\n    if (Settings.validate) Settings.sanitycheck = Settings.sanitycheck + 1\n    Complex(re + rhs.re, im + rhs.im)\n  }\n\n  def -(rhs: Complex): Complex = {\n    if (Settings.validate) Settings.sanitycheck = Settings.sanitycheck + 1\n    Complex(re - rhs.re, im - rhs.im)\n  }\n\n  def *(rhs: Complex): Complex = Complex(re * rhs.re - im * rhs.im, re * rhs.im + im * rhs.re)\n\n}\n\n class InterleavedVector(val save: Array[Double]) extends AnyVal {\n    //class ComplexVector(n: Int) extends AnyVal{\n    //val save = new Array[Complex](n)\n\n    def apply(i: Int): Complex = Complex(save(2*i),save(2*i+1))\n\n    def update(i: Int, y: Complex): InterleavedVector = {\n      save(2*i) = y.re\n      save(2*i+1) = y.im\n      this\n    }\n\n    def print() = {\n      save.map(p => println(p))\n    }\n\n  }\nclass ComplexVector(val save: Array[Complex]) extends AnyVal {\n  //class ComplexVector(n: Int) extends AnyVal{\n  //val save = new Array[Complex](n)\n\n  def apply(i: Int): Complex = save(i)\n\n  def update(i: Int, y: Complex): ComplexVector = {\n    save(i) = y\n    this\n  }\n\n  def print() = {\n    save.map(p => println(p))\n  }\n\n}\n\nobject VectorMult {\n  def apply(base: Int, strides: Vector[Int], loopvars: Vector[Int]): Int = {\n    //val t = loopvars.reverse.zip(strides)\n    var x = base\n    val length = loopvars.length\n    for (i <- 0 until strides.length)\n      x = x + (strides(i) * loopvars(length - 1 - i))\n\n    //val r = base + t.foldLeft(0)({ (acc, ele) => acc + (ele._1 * ele._2) })\n    //assert(r == x)\n    //r\n    x\n  }\n}\n\ndef time(): Double = {\n  val t = new testClass\n  val pre = new PreCompute\n\n  /*for (i <- 0 until 4) {\n    for (j <- 0 until 4)\n      println(Twiddle.WHT(4, i, j))\n    println(\" ------------\")\n  }*/\n  val r = new Random()\n\n  val iterateover = if (Settings.n.isDefined) Vector(Settings.n.get) else (" + testsize + " until " + (testsize + 1) + " )\n  val timings = for (twopower <- iterateover) yield{\n    val size = Math.pow(2, twopower).toInt\n\n    var fail = false\n    val fftmatrix = for (i <- Vector(r.nextInt(size)) /*0 until size*/ ) yield {\n      //columns\n      val one = Complex(1, 0)\n      val zero = Complex(0, 0)\n\n       " + data


    + "\n  Twiddle.twindex = 0\n      Twiddle.precompbuffer = Vector.empty\n   Twiddle.dprecompbuffer = Vector.empty\n    //VARIOUS PRE CALLS HERE\n  val resx = pre." + call + "     // VARIOUS PRE CALLS HERE END\n      Twiddle.precomp = Twiddle.precompbuffer.toArray\n   Twiddle.dprecomp = Twiddle.dprecompbuffer.toArray\n    val seqtime = Settings.standardConfig measure {\n\n\n        val instride = Vector(1, 1)\n        Settings.sanitycheck = 0\n        Twiddle.twindex = 0\n        //VARIOUS CALLS HERE\n       val resx = t." + call + "     // VARIOUS CALLS HERE END\n\n        if (Settings.validate) {\n\n          for (c <- 0 until size) {\n            val c1 = res(c)\n            val c2 = if (Settings.WHT) Twiddle.WHT(size, c, i) else Twiddle.DFT(size, c, i)\n\n            val thres = 1E-3\n            if (Math.abs(c1.re - c2.re) > thres) {\n              println(c1.re)\n              println(c2.re)\n              fail = true\n            }\n            if (Math.abs(c1.im - c2.im) > thres) {\n              println(c1.im)\n              println(c2.im)\n              fail = true\n            }\n            assert(!fail)\n          }\n        }\n      }\n      println(s\"time: $seqtime ms\")\nseqtime\n      \n    }\n    //println(fftmatrix)\n    //val validate = Twiddle.DFT(size)\n    //println(validate)\n\n    /*var fail = false\n    val thres = 1E-3\n    for (i <- 0 until size)\n      for (j <- 0 until size) {\n        val c1 = fftmatrix(i)(j)\n        val c2 = validate(i)(j)\n        if (Math.abs(c1.re - c2.re) > thres) {\n          println(c1.re)\n          println(c2.re)\n          fail = true\n        }\n        if (Math.abs(c1.im - c2.im) > thres) {\n          println(c1.im)\n          println(c2.im)\n          fail = true\n        }\n      }*/\n\n   if (!fail)\n        println(size + \" WORKS!!!!\")\n\n      fftmatrix\n    }\n    println(\"timings\")\n    println(timings)\n    timings.head.head\n  }\n\n\n  time()/*val one = Complex(0, 0)\n  val two = Complex(0, 0)\n  val three = Complex(1, 0)\n  val four = Complex(0, 0)\n\n\n  val in = new ComplexVector(4)\n  val x1 = in.update(0, one)\n  val x2 = x1.update(1, two)\n  val x3 = x2.update(2, three)\n  val x4 = x3.update(3, four)\n\n  val out = new ComplexVector(4)\n  val res = t.apply(x4, out, 4, 0, Vector.empty, 0, Vector.empty, Vector.empty)\n  res.print()*/\n\n")


  }
}
