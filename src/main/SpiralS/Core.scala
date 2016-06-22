package SpiralS

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class ComplexVector

class Complex

class Core extends Skeleton {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenIfThenElse {
    val IR: self.type = self
  }

  //def iniGTSkeleton(n: Option[Int]): StatGTSkeleton = if (n.isEmpty) StatGTSkeleton(None, Some(1), StatIMH(None), StatIMH(None), Some(ParInfo(6,64))) else StatGTSkeleton(n, Some(1), StatIMH(None), StatIMH(None),Some(ParInfo(6,64)))
  def iniGTSkeleton(n: Option[Int]): StatGTSkeleton = if (n.isEmpty) StatGTSkeleton(None, Some(1), StatIMH(None), StatIMH(None), None, None) else StatGTSkeleton(n, Some(1), StatIMH(None), StatIMH(None), None,None)

  def F2(stat: StatGTSkeleton): StagedFunction[DynGTSkeleton, Single] = {
    val expose = exposeDynGTSkeleton(stat)
    val f: (DynGTSkeleton => Single) = (z: DynGTSkeleton) => {
      val nv0 = ivecappend(z.v, Const(0))
      val nv1 = ivecappend(z.v, Const(1))
      val mix = GTSkeletonFull(stat, z)
      val target: Single = mix.y
      val scatterim = mix.s
      val gatherim = mix.g

      val gindex1: Rep[Int] = ivecmult(gatherim.base.toRep(), gatherim.strides, nv0)
      val gindex2: Rep[Int] = ivecmult(gatherim.base.toRep(), gatherim.strides, nv1)

      val sindex1: Rep[Int] = ivecmult(scatterim.base.toRep(), scatterim.strides, nv0)
      val sindex2: Rep[Int] = ivecmult(scatterim.base.toRep(), scatterim.strides, nv1)

      val t01 = vecapply(z.x.y, gindex1)
      val t02 = vecapply(z.x.y, gindex2)

      val (t1,t2) = mix.twiddleScaling match{
        case Some(twiddleScaling) => {
          val m1 = twiddle_apply_index(twiddleScaling.n.toRep(),twiddleScaling.d.toRep(),twiddleScaling.k.toRep(),gindex1)
          val m2 = twiddle_apply_index(twiddleScaling.n.toRep(),twiddleScaling.d.toRep(),twiddleScaling.k.toRep(),gindex2)
          (times(m1,t01),times(m2,t02))
        }
        case None => (t01,t02)
      }

      val cres1 = plus(t1, t2)
      val cres2 = minus(t1, t2)


      val res1 = vecupdate(target.y, sindex1, cres1)
      val res2 = vecupdate(res1, sindex2, cres2)
      Single(res2)

    }
    doGlobalLambda(f, true)(expose, exposeSingle)
  }
  object MathUtilities {

  }




  def static_chooseRadix(n: Int) = n / 2

  def chooseRadix(n: SInt) = n.i.fold(fa => SInt(choose_radix(fa)), fb => SInt(static_chooseRadix(fb)))

  def fuseIM(r: IMH, s: IMH): IMH = {
    val ss0 = ivecfirstorzero(r.strides) //ivecapply(r.strides, Const(0))
    val fbase = r.base + SInt(ss0) * s.base
    val fstrides = iveczipmagic(r.strides, s.strides)
    IMH(fbase, fstrides)
  }

  def zGT(expose: ExposeRep[DynGTSkeleton], innerf: => (DynGTSkeleton => Single)): StagedFunction[DynGTSkeleton, Single] = {
    val f: (DynGTSkeleton => Single) = (wuf: DynGTSkeleton) => innerf(wuf)
    val t: StagedFunction[DynGTSkeleton, Single] = doGlobalLambda(f, true)(expose, exposeSingle)
    t
  }


  val WHT = false


  def DFT_CT(mix: GTSkeletonFull): Single =
  {
    val m = chooseRadix(mix.n)
    val k = mix.n / m

    //val res = sumFold(mix.loopbound.toRep(), Single(veccreate(mix.n.toRep())), {
    val parcond = mix.loopbound.i.fold(fa => true, fb => fb > 1)
    val res = sumFold(mix.loopbound.toRep(), parcond && mix.parInfo.isDefined, mix.y, {
      isingle => {
        val newparcond = if (parcond && mix.parInfo.isDefined) None else mix.parInfo
        val i = isingle.i
        val acc = isingle.s
        val tmp = Single(veccreate(mix.n.toRep()))
        val stage1 = {
          val loopvars = ivecappend(mix.v, i)
          val s1_gather = {
            val base = SInt(0)
            val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
            val t2 = if (!WHT) {
                val t1 = ivecappend(t0, k.toRep())
                ivecappend(t1, Const(1))
              } else
              {
                val t1 = ivecappend(t0, Const(1))
                ivecappend(t1, m.toRep())
              }
            val inner = IMH(base, t2)
            val outer_upranked = IMH(mix.g.base, ivecuprank(mix.g.strides))
            fuseIM(outer_upranked, inner) //gather therefore swapped
            //inner
          }
          val s1_scatter = {
            val base = SInt(0)
            val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
            val t1 = ivecappend(t0, Const(1))
            val t3 = ivecappend(t1, m.toRep())
            val inner = IMH(base, t3)
            inner
          }
          mix.copy(x = mix.x, y = tmp, n = m, loopbound = k, g = s1_gather, s = s1_scatter, loopvars,newparcond)
        }
        val stage1stat = stage1.getStatSkel()
        val stage1expose = exposeDynGTSkeleton(stage1stat)
        val stage1dyn = stage1.getDynSkel()
        val f1: StagedFunction[DynGTSkeleton, Single] = zGT(stage1expose, DFT(stage1stat))
        val t1 = f1(stage1dyn)


        /*val after_twiddle = if (!WHT) {
          val tv = twiddle_apply(t1.y, mix.n.toRep(), mix.n.toRep(), m.toRep(), Const(1))
          Single(tv)
        } else Single(t1.y)*/

        val after_twiddle = Single(t1.y)
        //val twiddle_indexed = twiddle_apply_indexed()









        val stage2 = {
          val loopvars = ivecappend(mix.v, i)

          val before_fuse_gather = {
            val base = SInt(0) //: Either[Rep[Int], Option[Int]] = Right(Some(0))
            val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
            val t1 = ivecappend(t0, m.toRep())
            val t3 = ivecappend(t1, Const(1))
            IMH(base, t3)
          }

          val s1_gather = before_fuse_gather //GTI!!
          val outer_upranked = IMH(mix.s.base, ivecuprank(mix.s.strides))
          val s1_scatter = fuseIM(outer_upranked, before_fuse_gather)

          //assert(mix.twiddleScaling.isEmpty)
          val tw = TwiddleScaling(SInt(mix.n.toRep()), SInt(m.toRep()), SInt(Const(1))) //this is assuming that we always fuse into stage one and therefore can always override stage 2 Twiddle
          mix.copy(x = after_twiddle, y = mix.y, n = k, loopbound = m, g = s1_gather, s = s1_scatter, loopvars, newparcond, twiddleScaling = Some(tw))
          //mix.copy(x = t1, y = mix.y, n = k, loopbound = m, g = mix.g, s = mix.s, loopvars)
        }
        val stage2stat = stage2.getStatSkel()
        val stage2expose = exposeDynGTSkeleton(stage2stat)
        val stage2dyn = stage2.getDynSkel()
        val f2: StagedFunction[DynGTSkeleton, Single] = zGT(stage2expose, DFT(stage2stat))
        val t2 = f2(stage2dyn)
        t2
        //t1
      }
    })
    res
  }

  def DFT_Rader(mix: GTSkeletonFull): Single = mix.x

  def DFT(stat: StatGTSkeleton): (DynGTSkeleton => Single) = {
    val outer: (DynGTSkeleton => Single) = (dyn: DynGTSkeleton) => {
      val mix = GTSkeletonFull(stat, dyn)
      val sn: Rep[Int] = mix.n.toRep()
      val cond = isbasecase(sn)
      myifThenElse(cond, {
        val f2f = F2(stat)
        sumFold(mix.loopbound.toRep(),false, Single(veccreate(mix.n.toRep())), {
          isingle => f2f(mix.copy(v = ivecappend(mix.v, isingle.i)).getDynSkel())
        })
      },
        myifThenElse(isPrime(sn),
          DFT_Rader(mix),
          DFT_CT(mix))
      )
    }
    outer
  }


  def graphvizexport() = {
    lazy val ingt = iniGTSkeleton(None)
    val (code, cm) = emitGraph.emitDepGraphf(DFT(ingt))(exposeDynGTSkeleton(ingt), exposeSingle)
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
    stream.println(code)
    stream.flush()
    stream.close()
  }

  def codeexport() = {
    lazy val ingt = iniGTSkeleton(None)
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))

    stream2.println("package SpiralS\n\nobject Twiddle {\n  var TMap = Map.empty[(Int,Int,Int), ComplexVector]\n  object MathUtilities {\n\n    def dLin(N: Int, a: Double, b: Double): List[Double] = {\n      val t_array = new Array[Double](N)\n      for (i <- 0 until N)\n        t_array(i) = a * i + b\n      t_array.toList\n    }\n\n    def diagTensor(a: List[Double], b: List[Double]): List[Double] = {\n      val t_array = new Array[Double](a.size * b.size)\n      for (i <- 0 until a.size)\n        for (j <- 0 until b.size)\n          t_array(i * b.size + j) = a(i) * b(j)\n      t_array.toList\n    }\n  }\n\n  def apply(x: ComplexVector, n: Int, d: Int, k: Int): ComplexVector = {\n    val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))\n    val t = E(n)\n    val root_list_re = diag map (ele => t.re(ele.toInt * k))\n    val root_list_im = diag map (ele => t.im(ele.toInt * k))\n\n    for (i <- 0 until root_list_re.size) {\n      val u = Complex(root_list_re(i), root_list_im(i))\n      //val idx = vrep(yi)\n      val tx = x.apply(i)\n      x.update(i, tx * u)\n    }\n    x\n  }\n\n  def apply(n: Int, d: Int, k: Int, i: Int): Complex = {\n\n    if (!TMap.contains((n,d,k))){\n      val diag = MathUtilities.diagTensor(MathUtilities.dLin(n / d, 1, 0), MathUtilities.dLin(d, 1, 0))\n      val t = E(n)\n      val root_list_re = diag map (ele => t.re(ele.toInt * k))\n      val root_list_im = diag map (ele => t.im(ele.toInt * k))\n\n      val cv = new ComplexVector(root_list_re.size)\n      for (i <- 0 until root_list_re.size) {\n        val u = Complex(root_list_re(i), root_list_im(i))\n        cv.update(i,u)\n      }\n      TMap = TMap + ( (n,d,k) -> cv)\n    }\n    val cv = TMap.get((n,d,k))\n    cv.get(i)\n  }\n\n\n  def DFT(n: Int): Vector[ComplexVector] = {\n    val m = new Array[ComplexVector](n)\n    val k = 1\n    val t_e = E(n)\n    for (x <- 0 until n)\n      m(x) = new ComplexVector(n)\n    for (x <- 0 until n)\n      for (y <- 0 until n) {\n\n        m(x).update(y, new Complex(t_e.re(x * y * k), t_e.im(x * y * k)))\n      }\n    m.toVector\n  }\n\n  //this is the version that returns a single complex\n  def DFT(n: Int, x: Int, y: Int): Complex = {\n    val k = 1\n    val t_e = E(n)\n    new Complex(t_e.re(x * y * k), t_e.im(x * y * k))\n  }\n\n\n}\n\n\n\n  object E {\n    var EMap = Map.empty[Int, E]\n    def apply(n: Int): E = {\n      val eo = EMap.get(n)\n      eo.getOrElse( {\n        val ne = new E(n)\n        EMap = EMap + (n -> ne)\n        ne\n      })\n    }\n  }\n\nclass E(val n: Int) {\n  def Gcd[A](x: A, y: A)(implicit integral: Integral[A]): A = {\n    val t = scala.math.BigInt(integral.toLong(x))\n    val res = t.gcd(scala.math.BigInt(integral.toLong(y)))\n    x match {\n      case _: Int => res.toInt.asInstanceOf[A]\n      case _: Long => res.toLong.asInstanceOf[A]\n      case _: Short => res.toShort.asInstanceOf[A]\n    }\n  }\n\n  def NormalizeRational[A](x: A, y: A)(implicit integral: Integral[A]): (A, A) = {\n    val gcd = Gcd(x, y)\n    (integral.quot(x, gcd), integral.quot(y, gcd))\n  }\n\n  def normalize_2pi_shift(xin: Double, yin: Double): (Double, Double) = {\n    var (x, y) = NormalizeRational(Math.round(xin), Math.round(yin))\n    if ((x / y) < 0) {\n      val t: Long = Math.ceil(x.toDouble / y.toDouble / (-2.0)).toLong\n      x = x + 2 * t * y\n    } else {\n      val t = (Math.floor((x.toDouble - 2 * y.toDouble) / y.toDouble / 2.0) + 1).toLong;\n      x = x - 2 * y * t;\n    }\n    val (xp, yp) = NormalizeRational(x, y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_pi_over2_shift(xin: Double, yin: Double): (Double, Double) = {\n    val (x, y) = (Math.round(xin), Math.round(yin))\n    val (xp, yp) = NormalizeRational(2 * x - y, 2 * y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_pi_over2_reflection(xin: Double, yin: Double): (Double, Double) = {\n    val (x, y) = (Math.round(xin), Math.round(yin))\n    val (xp, yp) = NormalizeRational(y - 2 * x, 2 * y)\n    (xp.toDouble, yp.toDouble)\n  }\n\n  def normalize_trig(sign: Int, trig: String, x: Double, y: Double): (Int, String, Double, Double, Double) = {\n    // normalization in 2Pi, achieving: 0 <= xn / yn <= 2\n    val (xn, yn) = normalize_2pi_shift(x, y)\n    if (xn > yn) {\n      trig match {\n        case \"sin\" => normalize_trig(sign * (-1), \"sin\", xn - yn, yn)\n        case \"cos\" => normalize_trig(sign * (-1), \"cos\", xn - yn, yn)\n      }\n    } else if (xn == yn) {\n      trig match {\n        case \"sin\" => (sign, \"sin\", xn, yn, sign * (+0.0))\n        case \"cos\" => (sign, \"cos\", xn, yn, sign * (-1.0))\n      }\n    } else {\n      if (xn > yn / 2) {\n        // normalization in Pi, achieving 0 <= xn / yn <= 1/2\n        val (xp, yp) = normalize_pi_over2_shift(xn, yn)\n        trig match {\n          case \"sin\" => normalize_trig(sign * (+1), \"cos\", xp, yp)\n          case \"cos\" => normalize_trig(sign * (-1), \"sin\", xp, yp)\n        }\n      } else if (xn == yn / 2) {\n        trig match {\n          case \"sin\" => (sign, \"sin\", xn, yn, sign * (+1.0))\n          case \"cos\" => (sign, \"cos\", xn, yn, sign * (+0.0))\n        }\n      } else {\n        // now reflect in Pi / 2, and make sure that 0 <= xn / yn <= 1/4\n        if (xn > yn / 4) {\n          val (xp, yp) = normalize_pi_over2_reflection(xn, yn)\n          trig match {\n            case \"sin\" => (sign, \"cos\", xp, yp, Double.MaxValue)\n            case \"cos\" => (sign, \"sin\", xp, yp, Double.MaxValue)\n          }\n        } else if (xn == yn / 4) {\n          (sign, \"cos\", 1.0, 4.0, Double.MaxValue)\n        } else {\n          if (xn == 0.0) {\n            trig match {\n              case \"sin\" => (sign, \"sin\", xn, yn, sign * (+0.0))\n              case \"cos\" => (sign, \"cos\", xn, yn, sign * (+1.0))\n            }\n          } else {\n            trig match {\n              case \"sin\" => (sign, \"sin\", xn, yn, Double.MaxValue)\n              case \"cos\" => (sign, \"cos\", xn, yn, Double.MaxValue)\n            }\n          }\n        }\n      }\n    }\n  }\n\n  private def valueSinOrCos(f: String, x: Double, y: Double): Double = {\n    val (sign, trig, xn, yn, value) = normalize_trig(1, f, x, y)\n    if (!value.equals(scala.Double.MaxValue)) {\n      value\n\n    } else {\n      trig match {\n        case \"sin\" => (xn, yn) match {\n          case (1.0, 6.0) => sign * 0.5\n          case _ => sign * Math.sin(xn * Math.PI / yn)\n        }\n        case \"cos\" => sign * Math.cos(xn * Math.PI / yn)\n      }\n    }\n  }\n\n  def SinPi(x: Double, y: Double): Double = valueSinOrCos(\"sin\", x, y)\n\n  def CosPi(x: Double, y: Double): Double = valueSinOrCos(\"cos\", x, y)\n\n  private def yieldk(n: Int) = {\n    //TODO - find short form for return value\n    def tmp() = {\n      for (k <- 0 until n\n           // this if checks if x^t becomes 1 before n==t, this is e.g. the\n           // case for 2nd root of unity of 4 where it becomes 1 at x^2\n           if (for (t <- 2 until n - 1\n                    if (Math.cos(2 * math.Pi * k * t / n) == 1)\n           ) yield 1).isEmpty\n      )\n        yield k\n    }\n    tmp.last\n  }\n\n  lazy val store = yieldk(n)\n\n  def re(p: Int): Double = {\n    val x = CosPi(2.0 * p * store, n)\n    x\n  }\n\n  def im(p: Int): Double = SinPi(2.0 * p * store, n) * -1.0\n}\n\n\ncase class Complex(val re: Double, val im: Double) {\n  def +(rhs: Complex): Complex = Complex(re + rhs.re, im + rhs.im)\n\n  def -(rhs: Complex): Complex = Complex(re - rhs.re, im - rhs.im)\n\n  def *(rhs: Complex): Complex = Complex(re * rhs.re - im * rhs.im, re * rhs.im + im * rhs.re)\n\n}\n\nclass ComplexVector(n: Int) {\n  val save = new Array[Complex](n)\n\n  def apply(i: Int): Complex = save(i)\n\n  def update(i: Int, y: Complex): ComplexVector = {\n    save(i) = y\n    this\n  }\n\n  def print() = {\n    save.map(p => println(p))\n  }\n\n}\n\nobject VectorMult {\n  def apply(base: Int, strides: Vector[Int], loopvars: Vector[Int]): Int = {\n    val t = loopvars.reverse.zip(strides)\n    val r = base + t.foldLeft(0)({ (acc, ele) => acc + (ele._1 * ele._2) })\n    r\n  }\n}\n\nobject Testit extends App {\n  val t = new testClass\n\n  for (twopower <- 1 until 15) {\n    val size = Math.pow(2,twopower).toInt\n\n    var fail = false\n    val fftmatrix = for (i <- 0 until size) yield {\n      //columns\n      val one = Complex(1, 0)\n      val zero = Complex(0, 0)\n\n      val input = (0 until size).foldLeft(new ComplexVector(size)) {\n        (acc, ele) => if (ele == i) acc.update(ele, one) else acc.update(ele, zero)\n      }\n      val out = new ComplexVector(size)\n      val instride = Vector(1, 1)\n      val res = t.apply(input, out, size, 0, instride, 0, instride, Vector.empty)\n\n\n      for (c <- 0 until res.save.size) {\n        val c1 = res(c)\n        val c2 = Twiddle.DFT(size,c,i)\n\n        val thres = 1E-3\n        if (Math.abs(c1.re - c2.re) > thres) {\n          println(c1.re)\n          println(c2.re)\n          fail = true\n        }\n        if (Math.abs(c1.im - c2.im) > thres) {\n          println(c1.im)\n          println(c2.im)\n          fail = true\n        }\n        assert(!fail)\n      }\n      res\n    }\n    //println(fftmatrix)\n    //val validate = Twiddle.DFT(size)\n    //println(validate)\n\n    /*var fail = false\n    val thres = 1E-3\n    for (i <- 0 until size)\n      for (j <- 0 until size) {\n        val c1 = fftmatrix(i)(j)\n        val c2 = validate(i)(j)\n        if (Math.abs(c1.re - c2.re) > thres) {\n          println(c1.re)\n          println(c2.re)\n          fail = true\n        }\n        if (Math.abs(c1.im - c2.im) > thres) {\n          println(c1.im)\n          println(c2.im)\n          fail = true\n        }\n      }*/\n\n    if (!fail)\n      println(size + \" WORKS!!!!\")\n\n  }\n\n\n\n  /*val one = Complex(0, 0)\n  val two = Complex(0, 0)\n  val three = Complex(1, 0)\n  val four = Complex(0, 0)\n\n\n  val in = new ComplexVector(4)\n  val x1 = in.update(0, one)\n  val x2 = x1.update(1, two)\n  val x3 = x2.update(2, three)\n  val x4 = x3.update(3, four)\n\n  val out = new ComplexVector(4)\n  val res = t.apply(x4, out, 4, 0, Vector.empty, 0, Vector.empty, Vector.empty)\n  res.print()*/\n\n}")
    val esc = codegen.emitSource((DFT(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

}
