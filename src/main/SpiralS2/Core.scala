package SpiralS2

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


class Core extends Header {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenOrderingOps {
    val IR: self.type = self
  }

  val WHT = true
  //false
  val inplace = false // true

  def resolveH(h: IMHBase, i: AInt, v: AInt): AInt = h.base + (h.s0 * i) + (h.s1 * v)

  def resolveTwid(n: AInt, d: AInt, k: AInt, i: AInt): DataEle = ???

  def chooseRadix(n: AInt): AInt = n / toOE(2)

  def sum[A](till: AInt, ini: Data, body: iData => Data): Data = {
    sumFoldx(till.ev.toRep(till.a), false, ini.getdata(), body)
  }

  case class iData(d: SComplexVector, i: AInt)

  implicit val exposeiData = new ExposeRep[iData]() {

    val freshExps = (u: Unit) => Vector(Arg[ComplexVector], Arg[Int])
    val vec2t: Vector[Exp[_]] => iData = (in: Vector[Exp[_]]) => {
      val t = in(1).asInstanceOf[Rep[Int]]
      val s = SComplexVector(in(0).asInstanceOf[Rep[ComplexVector]]) //exposeSingle.vec2t(in.tail)
      iData(s, R2AInt(t))
    }
    val t2vec: iData => Vector[Exp[_]] = (in: iData) => {
      val t: Vector[Exp[_]] = Vector(in.d.d)
      val s = Vector(in.i.ev.toRep(in.i.a))
      t ++ s
    }
  }

  def F2(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      sum(mix.lb, mix.x, { idata => {
        val t01 = dyn.x.apply(resolveH(mix.im.gather(), toOE(0), idata.i))
        val t02 = dyn.x.apply(resolveH(mix.im.gather(), toOE(1), idata.i))
        val (t1, t2): (DataEle, DataEle) = mix.im match {
          case im_git: GT_IM => (t01, t02)
          case im_gti: GTI_IM => mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
            fb => ((resolveTwid(fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(0), mix.v)) * t01),
              (resolveTwid(fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(1), mix.v)) * t02)))
        }
        mix.y.update(resolveH(mix.im.scatter(), toOE(0), idata.i), (t1 + t2)).update(resolveH(mix.im.scatter(), toOE(1), idata.i), (t1 - t2))
      }
      })
    }
    val t: StagedFunction[Dyn, Data] = doGlobalLambda(stageme, Some("F2" + stat.toSig()), Some("F2" + stat.toSig()))(expose, exposeData)
    MaybeSFunction(t)
  }

  //we always "uprank" r
  def fuseIM(r: IMHBase, s: IMHBase, lv: AInt): IMH = {
    val ss0 = r.s0
    val fbase = r.base + (ss0 * s.base)
    val ns0 = r.s0 * s.s0
    val ns1 = toOE(0) + (r.s0 * s.s1)
    val ns2 = r.s1
    val baseres = fbase + ns2 * lv
    IMH(baseres, ns0, ns1)
  }

  def DFT_CT(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      val m = chooseRadix(mix.n)
      val k = mix.n / m
      sum(mix.lb, mix.x, { idata => {
        val stage1mix: Mix = {
          val s1_gather: IMH = {
            val base = toOE(0)
            val (s0, s1) = if (WHT) (k, toOE(1)) else (toOE(1), m)
            val inner = IMH(base, s0, s1)
            fuseIM(mix.im.gather(), inner, mix.v)
          }
          val s1_scatter: IMH = if (inplace) {
            fuseIM(mix.im.scatter(), IMH(toOE(0), m, toOE(1)), mix.v)
          } else IMH(toOE(0), toOE(1), m)

          val nim = GT_IM(s1_gather, s1_scatter)
          val stage1_target = if (inplace) mix.y else mix.y.create(mix.n)
          mix.copy(x = mix.x, y = stage1_target, n = m, lb = k, im = nim, v = idata.i)
        }
        val dataafterS1 = DFT(stage1mix.getStat())(stage1mix.getDyn())
        val stage2mix: Mix = {
          val s2_gather: IMH = IMH(toOE(0), m, toOE(1))
          val s2_scatter: IMH = fuseIM(mix.im.scatter(), s2_gather, mix.v)
          val nim = if (inplace) GTI_IM(s2_scatter, s2_gather) else GT_IM(s2_gather, s2_scatter)
          mix.copy(x = dataafterS1, y = mix.y, n = m, lb = k, im = nim, v = idata.i)
        }
        DFT(stage2mix.getStat())(stage2mix.getDyn())

      }
      })
    }
    val t: StagedFunction[Dyn, Data] = doGlobalLambda(stageme, Some("DFT_CT" + stat.toSig()), Some("DFT_CT" + stat.toSig()))(expose, exposeData)
    MaybeSFunction(t)
  }

  def DFT(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      val bool: Exp[Boolean] = mix.n.ev.toRep(mix.n.a) <= Const(2)
      myifThenElse(bool, {
        F2(stat)(dyn)
      }, {
        DFT_CT(stat)(dyn)
      })
    }
    val t: StagedFunction[Dyn, Data] = doGlobalLambda(stageme, Some("DFT" + stat.toSig()), Some("DFT" + stat.toSig()))(expose, exposeData)
    MaybeSFunction(t)
  }

  def ini(stat: Stat): (Dyn => Data) = {
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      DFT(stat)(dyn)
    }
    stageme
  }

  def iniGTSkeleton(n: Option[Int]): Stat = {
    if (n.isEmpty) {
      val idIM: StatIMH = new StatIMH(0, 1, 0)
      new Stat(sph(), 1, new Stat_GT_IM(idIM, idIM), 1, None)
    } else ???

  }

  def codeexport() = {
    lazy val ingt = iniGTSkeleton(None)
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    stream2.println("package SpiralS2\n\nimport scala.annotation.tailrec\n\nobject Settings {\n  val validate = true\n  val WHT = true\n}\n\n\nobject Twiddle {\n  var TMap = Map.empty[(Int, Int, Int), ComplexVector]\n\n  object MathUtilities {\n\n    def dLin(N: Int, a: Double, b: Double): List[Double] = {\n      val t_array = new Array[Double](N)\n      for (i <- 0 until N)\n        t_array(i) = a * i + b\n      t_array.toList\n    }\n\n    def diagTensor(a: List[Double], b: List[Double]): List[Double] = {\n      val t_array = new Array[Double](a.size * b.size)\n      for (i <- 0 until a.size)\n        for (j <- 0 until b.size)\n          t_array(i * b.size + j) = a(i) * b(j)\n      t_array.toList\n    }\n  }\n\n  def apply(x: ComplexVector, n: Int, d: Int, k: Int): ComplexVector = {\n    val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))\n    val t = E(n)\n    val root_list_re = diag map (ele => t.re(ele.toInt * k))\n    val root_list_im = diag map (ele => t.im(ele.toInt * k))\n\n    for (i <- 0 until root_list_re.size) {\n      val u = Complex(root_list_re(i), root_list_im(i))\n      //val idx = vrep(yi)\n      val tx = x.apply(i)\n      x.update(i, tx * u)\n    }\n    x\n  }\n\n  def apply(n: Int, d: Int, k: Int, i: Int): Complex = {\n\n    if (!TMap.contains((n, d, k))) {\n      val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))\n      val t = E(n)\n      val root_list_re = diag map (ele => t.re(ele.toInt * k))\n      val root_list_im = diag map (ele => t.im(ele.toInt * k))\n\n      val cv = new ComplexVector(new Array[Complex](root_list_re.size))\n      for (i <- 0 until root_list_re.size) {\n        val u = Complex(root_list_re(i), root_list_im(i))\n        cv.update(i, u)\n      }\n      TMap = TMap + ((n, d, k) -> cv)\n    }\n    val cv = TMap.get((n, d, k))\n    cv.get(i)\n  }\n\n\n  def DFT(n: Int): Vector[ComplexVector] = {\n    val m = new Array[ComplexVector](n)\n    val k = 1\n    val t_e = E(n)\n    for (x <- 0 until n)\n      m(x) = new ComplexVector(new Array[Complex](n))\n    for (x <- 0 until n)\n      for (y <- 0 until n) {\n\n        m(x).update(y, new Complex(t_e.re(x * y * k), t_e.im(x * y * k)))\n      }\n    m.toVector\n  }\n\n\n\n  def WHT(n: Int, x: Int, y: Int): Complex = {\n    //1* 1*\n    //1* -1*\n\n\n\n\n    val t = if (n == 1) new Complex(1, 0) /*{\n      if (rx == 1 && ry == 1) new Complex(-1, 0) else\n    }*/ else {\n      val nx = x % (n/2)\n      val ny = y % (n/2)\n      if (x >= n/2 && y >= n/2)\n        Complex(-1, 0) * WHT(n / 2, nx, ny)\n      else\n        WHT(n / 2, nx, ny)\n    }\n    t\n\n  }\n\n  //this is the version that returns a single complex\n  def DFT(n: Int, x: Int, y: Int): Complex = {\n    val k = 1\n    val t_e = E(n)\n    new Complex(t_e.re(x * y * k), t_e.im(x * y * k))\n  }\n\n\n}\n\n\nobject E {\n  var EMap = Map.empty[Int, E]\n\n  def apply(n: Int): E = {\n    val eo = EMap.get(n)\n    eo.getOrElse({\n      val ne = new E(n)\n      EMap = EMap + (n -> ne)\n      ne\n    })\n  }\n}\n\nclass E(val n: Int) {\n  def Gcd[A](x: A, y: A)(implicit integral: Integral[A]): A = {\n    val t = scala.math.BigInt(integral.toLong(x))\n    val res = t.gcd(scala.math.BigInt(integral.toLong(y)))\n    x match {\n      case _: Int => res.toInt.asInstanceOf[A]\n      case _: Long => res.toLong.asInstanceOf[A]\n      case _: Short => res.toShort.asInstanceOf[A]\n    }\n  }\n\n  def NormalizeRational[A](x: A, y: A)(implicit integral: Integral[A]): (A, A) = {\n    val gcd = Gcd(x, y)\n    (integral.quot(x, gcd), integral.quot(y, gcd))\n  }\n\n  def normalize_2pi_shift(xin: Double, yin: Double): (Double, Double) = {\n    var (x, y) = NormalizeRational(Math.round(xin), Math.round(yin))\n    if ((x / y) < 0) {\n      val t: Long = Math.ceil(x.toDouble / y.toDouble / (-2.0)).toLong\n      x = x + 2 * t * y\n    } else {\n      val t = (Math.floor((x.toDouble - 2 * y.toDouble) / y.toDouble / 2.0) + 1).toLong;\n      x = x - 2 * y * t;\n    }\n    val (xp, yp) = NormalizeRational(x, y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_pi_over2_shift(xin: Double, yin: Double): (Double, Double) = {\n    val (x, y) = (Math.round(xin), Math.round(yin))\n    val (xp, yp) = NormalizeRational(2 * x - y, 2 * y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_pi_over2_reflection(xin: Double, yin: Double): (Double, Double) = {\n    val (x, y) = (Math.round(xin), Math.round(yin))\n    val (xp, yp) = NormalizeRational(y - 2 * x, 2 * y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_trig(sign: Int, trig: String, x: Double, y: Double): (Int, String, Double, Double, Double) = {\n    // normalization in 2Pi, achieving: 0 <= xn / yn <= 2\n    val (xn, yn) = normalize_2pi_shift(x, y)\n    if (xn > yn) {\n      trig match {\n        case \"sin\" => normalize_trig(sign * (-1), \"sin\", xn - yn, yn)\n        case \"cos\" => normalize_trig(sign * (-1), \"cos\", xn - yn, yn)\n      }\n    } else if (xn == yn) {\n      trig match {\n        case \"sin\" => (sign, \"sin\", xn, yn, sign * (+0.0))\n        case \"cos\" => (sign, \"cos\", xn, yn, sign * (-1.0))\n      }\n    } else {\n      if (xn > yn / 2) {\n        // normalization in Pi, achieving 0 <= xn / yn <= 1/2\n        val (xp, yp) = normalize_pi_over2_shift(xn, yn)\n        trig match {\n          case \"sin\" => normalize_trig(sign * (+1), \"cos\", xp, yp)\n          case \"cos\" => normalize_trig(sign * (-1), \"sin\", xp, yp)\n        }\n      } else if (xn == yn / 2) {\n        trig match {\n          case \"sin\" => (sign, \"sin\", xn, yn, sign * (+1.0))\n          case \"cos\" => (sign, \"cos\", xn, yn, sign * (+0.0))\n        }\n      } else {\n        // now reflect in Pi / 2, and make sure that 0 <= xn / yn <= 1/4\n        if (xn > yn / 4) {\n          val (xp, yp) = normalize_pi_over2_reflection(xn, yn)\n          trig match {\n            case \"sin\" => (sign, \"cos\", xp, yp, Double.MaxValue)\n            case \"cos\" => (sign, \"sin\", xp, yp, Double.MaxValue)\n          }\n        } else if (xn == yn / 4) {\n          (sign, \"cos\", 1.0, 4.0, Double.MaxValue)\n        } else {\n          if (xn == 0.0) {\n            trig match {\n              case \"sin\" => (sign, \"sin\", xn, yn, sign * (+0.0))\n              case \"cos\" => (sign, \"cos\", xn, yn, sign * (+1.0))\n            }\n          } else {\n            trig match {\n              case \"sin\" => (sign, \"sin\", xn, yn, Double.MaxValue)\n              case \"cos\" => (sign, \"cos\", xn, yn, Double.MaxValue)\n            }\n          }\n        }\n      }\n    }\n  }\n\n  private def valueSinOrCos(f: String, x: Double, y: Double): Double = {\n    val (sign, trig, xn, yn, value) = normalize_trig(1, f, x, y)\n    if (!value.equals(scala.Double.MaxValue)) {\n      value\n\n    } else {\n      trig match {\n        case \"sin\" => (xn, yn) match {\n          case (1.0, 6.0) => sign * 0.5\n          case _ => sign * Math.sin(xn * Math.PI / yn)\n        }\n        case \"cos\" => sign * Math.cos(xn * Math.PI / yn)\n      }\n    }\n  }\n\n  def SinPi(x: Double, y: Double): Double = valueSinOrCos(\"sin\", x, y)\n\n  def CosPi(x: Double, y: Double): Double = valueSinOrCos(\"cos\", x, y)\n\n  private def yieldk(n: Int) = {\n    //TODO - find short form for return value\n    def tmp() = {\n      for (k <- 0 until n\n           // this if checks if x^t becomes 1 before n==t, this is e.g. the\n           // case for 2nd root of unity of 4 where it becomes 1 at x^2\n           if (for (t <- 2 until n - 1\n                    if (Math.cos(2 * math.Pi * k * t / n) == 1)\n           ) yield 1).isEmpty\n      )\n        yield k\n    }\n    tmp.last\n  }\n\n  lazy val store = yieldk(n)\n\n  def re(p: Int): Double = {\n    val x = CosPi(2.0 * p * store, n)\n    x\n  }\n\n  def im(p: Int): Double = SinPi(2.0 * p * store, n) * -1.0\n}\n\n\ncase class Complex(val re: Double, val im: Double) {\n  def +(rhs: Complex): Complex = Complex(re + rhs.re, im + rhs.im)\n\n  def -(rhs: Complex): Complex = Complex(re - rhs.re, im - rhs.im)\n\n  def *(rhs: Complex): Complex = Complex(re * rhs.re - im * rhs.im, re * rhs.im + im * rhs.re)\n\n}\n\nclass ComplexVector(val save: Array[Complex]) extends AnyVal {\n  //class ComplexVector(n: Int) extends AnyVal{\n  //val save = new Array[Complex](n)\n\n  def apply(i: Int): Complex = save(i)\n\n  def update(i: Int, y: Complex): ComplexVector = {\n    save(i) = y\n    this\n  }\n\n  def print() = {\n    save.map(p => println(p))\n  }\n\n}\n\nobject VectorMult {\n  def apply(base: Int, strides: Vector[Int], loopvars: Vector[Int]): Int = {\n    //val t = loopvars.reverse.zip(strides)\n    var x = base\n    val length = loopvars.length\n    for (i <- 0 until strides.length)\n      x = x + (strides(i) * loopvars(length - 1 - i))\n\n    //val r = base + t.foldLeft(0)({ (acc, ele) => acc + (ele._1 * ele._2) })\n    //assert(r == x)\n    //r\n    x\n  }\n}\n\nobject Testit extends App {\n  val t = new testClass\n\n  for (i <- 0 until 4){\n    for (j <- 0 until 4)\n      println(Twiddle.WHT(4,i,j))\n    println(\" ------------\")\n  }\n  for (twopower <- 2 until 15) {\n    val size = Math.pow(2, twopower).toInt\n\n    var fail = false\n    val fftmatrix = for (i <- 0 until size) yield {\n      //columns\n      val one = Complex(1, 0)\n      val zero = Complex(0, 0)\n\n      val input = (0 until size).foldLeft(new ComplexVector(new Array[Complex](size))) {\n        (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)\n      }\n      val out = new ComplexVector(new Array[Complex](size))\n      val instride = Vector(1, 1)\n      val res = t.apply(input, out, size) //, 0, instride, 0, instride, Vector.empty)\n\n      if (Settings.validate) {\n\n        for (c <- 0 until res.save.size) {\n          val c1 = res(c)\n          val c2 = if (Settings.WHT) Twiddle.WHT(size, c, i) else Twiddle.DFT(size, c, i)\n\n          val thres = 1E-3\n          if (Math.abs(c1.re - c2.re) > thres) {\n            println(c1.re)\n            println(c2.re)\n            fail = true\n          }\n          if (Math.abs(c1.im - c2.im) > thres) {\n            println(c1.im)\n            println(c2.im)\n            fail = true\n          }\n          assert(!fail)\n        }\n      }\n\n\n      res\n    }\n    //println(fftmatrix)\n    //val validate = Twiddle.DFT(size)\n    //println(validate)\n\n    /*var fail = false\n    val thres = 1E-3\n    for (i <- 0 until size)\n      for (j <- 0 until size) {\n        val c1 = fftmatrix(i)(j)\n        val c2 = validate(i)(j)\n        if (Math.abs(c1.re - c2.re) > thres) {\n          println(c1.re)\n          println(c2.re)\n          fail = true\n        }\n        if (Math.abs(c1.im - c2.im) > thres) {\n          println(c1.im)\n          println(c2.im)\n          fail = true\n        }\n      }*/\n\n    if (!fail)\n      println(size + \" WORKS!!!!\")\n\n  }\n\n\n\n  /*val one = Complex(0, 0)\n  val two = Complex(0, 0)\n  val three = Complex(1, 0)\n  val four = Complex(0, 0)\n\n\n  val in = new ComplexVector(4)\n  val x1 = in.update(0, one)\n  val x2 = x1.update(1, two)\n  val x3 = x2.update(2, three)\n  val x4 = x3.update(3, four)\n\n  val out = new ComplexVector(4)\n  val res = t.apply(x4, out, 4, 0, Vector.empty, 0, Vector.empty, Vector.empty)\n  res.print()*/\n\n}")
    val esc = codegen.emitSource((ini(ingt)), "testClass", stream2)(exposeDyn(ingt), exposeData)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }
}
