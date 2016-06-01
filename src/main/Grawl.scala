package apps


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


object Grawl extends App {


  trait DFT_Exp extends BaseExp with FunctionsExp with IfThenElsePureExp with PurePrimitiveOpsExp with ImplicitOpsExp with ScalaCompile {

    case class ISingle(s: Single, i: Rep[Int])

    case class Single(y: Rep[ComplexVector])

    case class Radix(n: Exp[Int]) extends Def[Int]

    def choose_radix(n: Exp[Int]): Exp[Int] = Radix(n)

    case class BaseCase(n: Exp[Int]) extends Def[Boolean]

    def isbasecase(n: Exp[Int]): Exp[Boolean] = BaseCase(n)


    case class IVecCreate(s: Exp[Int]) extends Def[Vector[Int]]

    def iveccreate(i: Exp[Int]): Exp[Vector[Int]] = IVecCreate(i)

    case class IVecApply(vec: Exp[Vector[Int]], i: Exp[Int]) extends Def[Int]

    def ivecapply(vec: Exp[Vector[Int]], i: Exp[Int]): Exp[Int] = IVecApply(vec, i)

    case class IVecUpdate(vec: Exp[Vector[Int]], i: Exp[Int], y: Exp[Int]) extends Def[Vector[Int]]

    def ivecupdate(vec: Exp[Vector[Int]], i: Exp[Int], y: Exp[Int]): Exp[Vector[Int]] = IVecUpdate(vec, i, y)

    case class IVecAppend(vec: Exp[Vector[Int]], y: Exp[Int]) extends Def[Vector[Int]]

    def ivecappend(vec: Exp[Vector[Int]], y: Exp[Int]): Exp[Vector[Int]] = IVecAppend(vec, y)

    case class IVecMult(base: Exp[Int], vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]) extends Def[Int]

    def ivecmult(base: Exp[Int], vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]): Exp[Int] = IVecMult(base, vec1, vec2)

    case class IVecZipMagic(vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]) extends Def[Vector[Int]]

    def iveczipmagic(vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]): Exp[Vector[Int]] = IVecZipMagic(vec1, vec2)

    case class VecCreate(s: Exp[Int]) extends Def[ComplexVector]

    def veccreate(i: Exp[Int]): Exp[ComplexVector] = VecCreate(i)

    case class VecApply(vec: Exp[ComplexVector], i: Exp[Int]) extends Def[Complex]

    def vecapply(vec: Exp[ComplexVector], i: Exp[Int]): Exp[Complex] = VecApply(vec, i)

    case class VecUpdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]) extends Def[ComplexVector]

    def vecupdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]): Exp[ComplexVector] = VecUpdate(vec, i, y)

    case class Plus(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

    def plus(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Plus(lhs, rhs)

    case class Minus(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

    def minus(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Minus(lhs, rhs)

    case class Times(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

    def times(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = Times(lhs, rhs)

    //case class SumLoop[T: TypeRep](till: Exp[Int], body: Exp[ComplexVector]) extends Def[ComplexVector]

    //def sumLoop[T: TypeRep](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) = IfThenElse(cond, thenp, elsep)

    case class SumFold(till: Exp[Int], ini: Exp[ComplexVector], loopvar: Exp[Int], loopacc: Exp[ComplexVector], body: Exp[_ => _]) extends Def[ComplexVector]

    def sumFold[A](till: Rep[Int], ini: Single, body: ISingle => Single)(implicit tupleexpose: ExposeRep[ISingle], singleexpose: ExposeRep[Single]): Single = {
      val lambda = doInternalLambda(body, false, false)(tupleexpose, singleexpose)
      val newsyms = singleexpose.freshExps()
      val looptuple = tupleexpose.freshExps()
      val loopvar = looptuple.head.asInstanceOf[Exp[Int]]
      val loopacc = looptuple.tail.head.asInstanceOf[Exp[ComplexVector]]
      val sumloopnode = SumFold(till, ini.y, loopvar, loopacc, lambda.exp)
      val sumnodeexp = toAtom(sumloopnode)

      val returnNodes = if (newsyms.size > 1) {
        newsyms.zipWithIndex.map(fsym => {
          //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
          val otp = exp2tp(fsym._1)
          val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
          val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
          val newx = toAtom(cc)(tag, null)
          newx
        })
      } else {
        newsyms.zipWithIndex.map(fsym => {
          val tag: TypeRep[Any] = exp2tp(fsym._1).tag.asInstanceOf[TypeRep[Any]]
          val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, false, true)
          val newx = toAtom(cc)(tag, null)
          newx
        })
      }
      singleexpose.vec2t(returnNodes)
    }
  }


  val t = Vector(1,2,3)
  val t1 = t.updated(1,3)

  trait ScalaGenBaseCase extends ScalaCodegen with EmitHeadInternalFunctionAsClass {
    val IR: DFT_Exp

    import IR._

    var delay: Vector[(TP[_], Vector[String], (Block, Vector[String]) => Vector[String])] = Vector.empty
    var delaynow: Boolean = false

    override def emitNode(tp: TP[_], acc: Vector[String],
                          block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
      val ma = tp.rhs match {

        case BaseCase(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " == 2 //check for base case"))
        //case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](" + quote(n) + ") //buffer creation"))
        case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new ComplexVector(" + quote(n) + ") //buffer creation"))
        case VecApply(vec: Exp[ComplexVector], i: Exp[Int]) => Vector(emitValDef(tp, ""  + quote(vec) + "("+ quote(i) + ")" ))
        case VecUpdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]) => Vector(emitValDef(tp, ""  + quote(vec) + ".update("+ quote(i) + ","+ quote(y) + ")" ))
        case IVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "Vector.empty[Int] //creating vector with " + quote(n)))
        case IVecAppend(v: Exp[Vector[Int]], y: Exp[Int]) => Vector(emitValDef(tp, quote(v) + " :+ " + quote(y)))
        case IVecApply(vec, i) => Vector(emitValDef(tp, quote(vec) + "(" + quote(i) + ")"))
        case IVecZipMagic(r, s) => Vector(emitValDef(tp, "Vector(" + quote(r) + ".head * " + quote(s) + ".head) ++ " + quote(r) + ".zipAll(" + quote(s) + ",0,0).map(p => p._1 + " + quote(r) + ".head * p._2)"))
        case IVecMult(b,s, l) => Vector(emitValDef(tp, " VectorMult(" + quote(b) + "," + quote(s) + "," + quote(l) + ")"))

        case Plus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
        case Minus(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
        case Times(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
        //case Divide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))


        case Radix(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " / 2 //stupid radix choice placeholder"))

        case SumFold(till: Exp[Int], ini: Exp[ComplexVector], loopvar: Exp[Int], acc: Exp[ComplexVector], body) => {
          val bodylambda = exp2tp(body)
          val rets: Vector[String] = bodylambda.rhs match {
            case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
              //val l1 = "val " + quote(tp) + " = ("+ quote(loopvar) + " <- 0 until "+ quote(till) + ").foldLeft(Vector.empty) {\n "
              val helper = if (tx.size > 1) {
                tx.zipWithIndex.map(a => {
                  val (tp, index) = a
                  val typ = remap(tp.tag.mf)
                  "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
                }).mkString("\n")
              } else {
                //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
                "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
              }
              val argtuple = tupledeclarehelper(tx.map(a => remap(a.tag)), "")
              val l1 = "val " + quote(tp) + " = (0 until " + quote(till) + ").foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
              val l10 = l1 + "\n" + helper + "\n"
              val l2 = block_callback(ty, Vector(l10))
              val trestuple: Vector[String] = ty.res.map(r => quote(r))
              val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
              val l4 = l3 + "\n})\n"
              l4
            })
            case _ => {
              assert(false, "got an SumLoop statment which does not contain a lambda")
              Vector.empty
            }
          }
          rets
        }

        case _ => {
          println(tp)
          super.emitNode(tp, acc, block_callback)
        }
      }
      ma
    }
  }

  class MyDSLProgram extends DFT_Exp {
    self =>

    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }

    override val codegen = new ScalaCodegen
      with EmitHeadInternalFunctionAsClass
      with ScalaGenPrimitivOps
      with ScalaGenBaseCase
      with ScalaGenIfThenElse {
      val IR: self.type = self
    }

    object SInt {
      def apply(nos: Int): SInt = SInt(Right(nos))

      def apply(nos: Rep[Int]): SInt = SInt(Left(nos))
    }

    case class SInt(i: Either[Rep[Int], Int]) {
      def +(that: SInt): SInt = {
        val t: Either[Rep[Int], Int] = i.fold(fa => {
          val r: Rep[Int] = that.i.fold(ifa => fa + ifa, ifb => fa + unit(ifb))
          Left(r)
        }, fb => {
          val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) + ifa), ifb => Right(fb + ifb))
          r
        })
        SInt(t)
      }

      def -(that: SInt): SInt = {
        val t: Either[Rep[Int], Int] = i.fold(fa => {
          val r: Rep[Int] = that.i.fold(ifa => fa - ifa, ifb => fa - unit(ifb))
          Left(r)
        }, fb => {
          val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) - ifa), ifb => Right(fb - ifb))
          r
        })
        SInt(t)
      }

      def *(that: SInt) = {
        val t: Either[Rep[Int], Int] = i.fold(fa => {
          val r: Rep[Int] = that.i.fold(ifa => fa * ifa, ifb => fa * unit(ifb))
          Left(r)
        }, fb => {
          val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) * ifa), ifb => Right(fb * ifb))
          r
        })
        SInt(t)
      }

      def /(that: SInt) = {
        val t: Either[Rep[Int], Int] = i.fold(fa => {
          val r: Rep[Int] = that.i.fold(ifa => fa / ifa, ifb => fa / unit(ifb))
          Left(r)
        }, fb => {
          val r: Either[Rep[Int], Int] = that.i.fold(ifa => Left(unit(fb) / ifa), ifb => Right(fb / ifb))
          r
        })
        SInt(t)
      }

      def toRep(): Rep[Int] = i.fold(fa => fa, fb => unit(fb))
    }

    case class Complex(val _im: Rep[Double], val _re: Rep[Double]) {
      def plus(x: Complex, y: Complex) = Complex(x._re + y._re, x._im + y._im)

      def minus(x: Complex, y: Complex) = Complex(x._re - y._re, x._im - y._im)

      def times(x: Complex, y: Complex) = {
        val m1 = x._re * y._re
        val m2 = x._im * y._im
        val m3 = x._re * y._im
        val m4 = x._im * y._re
        Complex(m1 - m2, m3 + m4)
      }
    }

    object IMH {
      def apply(s: StatIMH, d: DynIMH): IMH = {
        val nb = (s.base, d.base) match {
          case (None, Some(r)) => SInt(r)
          case (Some(i), None) => SInt(i)
          case _ => ???
        }
        IMH(nb, d.strides)
      }
    }

    //case class IMH(base: SInt, strides: Vector[SInt])
    case class IMH(base: SInt, strides: Rep[Vector[Int]]) {

      def getDynIMH() = base.i.fold(fa => DynIMH(Some(fa), strides), fb => DynIMH(None, strides))

      def getStatIMH() = base.i.fold(fa => StatIMH(None), fb => StatIMH(Some(fb)))
    }

    case class DynIMH(base: Option[Rep[Int]], strides: Rep[Vector[Int]]) {
      def t2vec(): Vector[Exp[_]] = if (base.isEmpty) Vector(strides) else Vector(base.get, strides)
    }

    case class StatIMH(base: Option[Int]) {
      def freshExps(): Vector[Exp[_]] = if (base.isEmpty) Vector(Arg[Int]) ++ Vector(Arg[Vector[Int]]) else Vector(Arg[Vector[Int]])

      def vec2t(v: Vector[Exp[_]]): (DynIMH, Vector[Exp[_]]) = {
        if (base.isEmpty) {
          val b = v.head.asInstanceOf[Exp[Int]]
          val s = v.tail.head.asInstanceOf[Exp[Vector[Int]]]
          (DynIMH(Some(b), s), v.tail.tail)
        } else {
          val s = v.head.asInstanceOf[Exp[Vector[Int]]]
          (DynIMH(None, s), v.tail.tail)
        }
      }
    }


    //case class IMH(base: SInt, strides: Vector[SInt])

    //case class GTSkeletonFull(x: Single, y: Single, n: SInt, g: IMH, s: IMH, v: Vector[Rep[Int]])
    case class DynGTSkeleton(x: Single, y: Single, n: Option[Rep[Int]], loopbound: Option[Rep[Int]], g: DynIMH, s: DynIMH, v: Rep[Vector[Int]])

    case class StatGTSkeleton(n: Option[Int], loopbound: Option[Int], g: StatIMH, s: StatIMH)

    object GTSkeletonFull {
      def apply(s: StatGTSkeleton, d: DynGTSkeleton): GTSkeletonFull = {
        val na = (s.n, d.n) match {
          case (None, Some(r)) => SInt(r)
          case (Some(i), None) => SInt(i)
          case _ => ???
        }
        val nl = (s.loopbound, d.loopbound) match {
          case (None, Some(r)) => SInt(r)
          case (Some(i), None) => SInt(i)
          case _ => ???
        }
        GTSkeletonFull(d.x, d.y, na, nl, IMH(s.g, d.g), IMH(s.s, d.s), d.v)

      }
    }

    case class GTSkeletonFull(x: Single, y: Single, n: SInt, loopbound: SInt, g: IMH, s: IMH, v: Rep[Vector[Int]]) {


      def getDynSkel() = {
        val on: Option[Rep[Int]] = n.i.fold(fa => Some(fa), fb => None)
        val ol: Option[Rep[Int]] = loopbound.i.fold(fa => Some(fa), fb => None)
        DynGTSkeleton(x, y, on, ol, g.getDynIMH(), s.getDynIMH(), v)
      }

      def getStatSkel() = {
        val on: Option[Int] = n.i.fold(fa => None, fb => Some(fb))
        val ol: Option[Int] = loopbound.i.fold(fa => None, fb => Some(fb))
        StatGTSkeleton(on, ol, g.getStatIMH(), s.getStatIMH())
      }
    }

    implicit def exposeDynGTSkeleton(stat: StatGTSkeleton): ExposeRep[DynGTSkeleton] = {
      new ExposeRep[DynGTSkeleton]() {
        val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
          val fn = if (stat.n.isEmpty) Vector(Arg[Int]) else Vector.empty
          val fl = if (stat.loopbound.isEmpty) Vector(Arg[Int]) else Vector.empty

          exposeSingle.freshExps() ++ exposeSingle.freshExps() ++
            fn ++ fl ++ stat.g.freshExps() ++ stat.s.freshExps() ++ Vector(Arg[Vector[Int]])
        }
        val vec2t: Vector[Exp[_]] => DynGTSkeleton = (in: Vector[Exp[_]]) => {
          assert(in.size >= 2)
          val x = exposeSingle.vec2t(in)
          val singlesize = exposeSingle.t2vec(x).size
          val outx = in.drop(singlesize)
          val y = exposeSingle.vec2t(outx)
          val outy = outx.drop(singlesize)
          val (n, outn) = if (stat.n.isEmpty) (Some(outy.head.asInstanceOf[Rep[Int]]), outy.tail) else (None, outy)
          val (l, outl) = if (stat.loopbound.isEmpty) (Some(outn.head.asInstanceOf[Rep[Int]]), outn.tail) else (None, outn)
          val (g, outg) = stat.g.vec2t(outl)
          val (s, outs) = stat.s.vec2t(outg)
          val v = outs.head.asInstanceOf[Exp[Vector[Int]]]
          DynGTSkeleton(x, y, n, l, g, s, v)
        }
        val t2vec: DynGTSkeleton => Vector[Exp[_]] = (in: DynGTSkeleton) => {
          val vn = in.n.map(p => Vector(p)).getOrElse(Vector.empty)
          val vl = in.loopbound.map(p => Vector(p)).getOrElse(Vector.empty)
          Vector(in.x.y, in.y.y) ++ vn ++ vl ++ in.g.t2vec() ++ in.s.t2vec() ++ Vector(in.v)
        }
      }
    }

    implicit val exposeTIntSingle = new ExposeRep[ISingle]() {
      val freshExps = (u: Unit) => Vector(Arg[ComplexVector], Arg[Int])
      val vec2t: Vector[Exp[_]] => ISingle = (in: Vector[Exp[_]]) => {
        val t = in(1).asInstanceOf[Rep[Int]]
        val s = Single(in(0).asInstanceOf[Rep[ComplexVector]]) //exposeSingle.vec2t(in.tail)
        ISingle(s, t)
      }
      val t2vec: ISingle => Vector[Exp[_]] = (in: ISingle) => {
        val t: Vector[Exp[_]] = Vector(in.i)
        val s = Vector(in.s.y)
        t ++ s
      }
    }


    implicit val exposeSingle = new ExposeRep[Single]() {
      val freshExps = (u: Unit) => Vector(Arg[ComplexVector])
      val vec2t: Vector[Exp[_]] => Single = (in: Vector[Exp[_]]) => Single(in.head.asInstanceOf[Rep[ComplexVector]])
      val t2vec: Single => Vector[Exp[_]] = (in: Single) => Vector(in.y)
    }


    def iniGTSkeleton(n: Option[Int]): StatGTSkeleton = {
      val im = StatIMH(None)
      val ingt = if (n.isEmpty) StatGTSkeleton(None, Some(1), im, im) else StatGTSkeleton(n, Some(1), im, im)
      ingt
    }



    def F2x(z: GTSkeletonFull): Single = {
      val target: Single = z.y
      val scatterim = z.s
      val gatherim = z.g

      val gindex1: Rep[Int] = ivecmult(gatherim.base.toRep(),gatherim.strides,z.v)
      val gindex2: Rep[Int] = gindex1 + Const(1)

      val sindex1: Rep[Int] = ivecmult(scatterim.base.toRep(),scatterim.strides,z.v)
      val sindex2: Rep[Int] = sindex1 + Const(1)

      val t1 = vecapply(z.x.y, gindex1)
      val t2 = vecapply(z.x.y, gindex2)

      val cres1 = plus(t1, t2)
      val cres2 = minus(t1, t2)


      val res1 = vecupdate(target.y, sindex1, cres1)
      val res2 = vecupdate(res1, sindex1, cres2)
      Single(res2)
      z.x
    }

    def F2(stat: StatGTSkeleton): StagedFunction[DynGTSkeleton, Single] = {
      val expose = exposeDynGTSkeleton(stat)
      val f: (DynGTSkeleton => Single) = (z: DynGTSkeleton) => {
        val mix = GTSkeletonFull(stat,z)
        val target: Single = mix.y
        val scatterim = mix.s
        val gatherim = mix.g

        val gindex1: Rep[Int] = ivecmult(gatherim.base.toRep(),gatherim.strides,z.v)
        val gindex2: Rep[Int] = gindex1 + Const(1)

        val sindex1: Rep[Int] = ivecmult(scatterim.base.toRep(),scatterim.strides,z.v)
        val sindex2: Rep[Int] = sindex1 + Const(1)

        val t1 = vecapply(z.x.y, gindex1)
        val t2 = vecapply(z.x.y, gindex2)

        val cres1 = plus(t1, t2)
        val cres2 = minus(t1, t2)


        val res1 = vecupdate(target.y, sindex1, cres1)
        val res2 = vecupdate(res1, sindex1, cres2)
        Single(res2)

      }
      doGlobalLambda(f,true)(expose, exposeSingle)
    }

    def static_chooseRadix(n: Int) = n / 2

    def chooseRadix(n: SInt) = n.i.fold(fa => SInt(choose_radix(fa)), fb => SInt(static_chooseRadix(fb)))

    def fuseIM(r: IMH, s: IMH): IMH = {
      val ss0 = ivecapply(r.strides, Const(0))
      val fbase = r.base + SInt(ss0) * s.base
      val fstrides = iveczipmagic(r.strides, s.strides)
      IMH(fbase, fstrides)
    }


    def DFTstart(stat: StatGTSkeleton): (DynGTSkeleton => Single) = {
      val outer: (DynGTSkeleton => Single) = (dyn: DynGTSkeleton) => {
        val stage1expose = exposeDynGTSkeleton(stat)
        val f1: StagedFunction[DynGTSkeleton, Single] = zGT(stage1expose, DFT(stat))
        val t1 = f1(dyn)
        t1
      }
      outer
    }

    def zGT(expose: ExposeRep[DynGTSkeleton], innerf: => (DynGTSkeleton => Single)): StagedFunction[DynGTSkeleton, Single] = {
      val f: (DynGTSkeleton => Single) = (wuf: DynGTSkeleton) => innerf(wuf)
      val t: StagedFunction[DynGTSkeleton, Single] = doGlobalLambda(f, true)(expose, exposeSingle)
      t
    }




    def DFT(stat: StatGTSkeleton): (DynGTSkeleton => Single) = {
      val outer: (DynGTSkeleton => Single) = (dyn: DynGTSkeleton) => {

        val mix = GTSkeletonFull(stat, dyn)

        val sn: Rep[Int] = mix.n.toRep()
        val cond = isbasecase(sn)
        myifThenElse(cond, {
          val f2f = F2(stat)
          val t= f2f(dyn)
          t
        }, {
          val m = chooseRadix(mix.n)
          val k = mix.n / m

          val res = sumFold(mix.loopbound.toRep(), Single(veccreate(mix.n.toRep())), {
            isingle => {
              val i = isingle.i
              val acc = isingle.s
              val stage1 = {
                val loopvars = ivecappend(mix.v, i)
                val s1_gather = {
                  val base = SInt(0)
                  val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
                  val t1 = ivecappend(t0, k.toRep())
                  val t2 = ivecappend(t1, Const(1))
                  val inner = IMH(base, t2)
                  //val inner = IMH(base, strides)
                  fuseIM(mix.g, inner) //gather therefore swapped
                }
                val s1_scatter = {
                  val base = SInt(0)
                  //val strides = Vector(SInt(1), m)
                  val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
                  val t1 = ivecappend(t0, Const(1))
                  val t2 = ivecappend(t1, m.toRep())
                  val inner = IMH(base, t2)
                  //inner
                  fuseIM(mix.s, inner)
                }
                mix.copy(x = mix.x, y = acc, n = m, loopbound = k, g = s1_gather, s = s1_scatter, loopvars)
                //mix.copy(x = mix.x, y = acc, n = m, loopbound = k, g = mix.g, s = mix.s, loopvars)
                //mix.copy(x = mix.x, y = acc, n = m, loopbound = k, g = mix.g, s = mix.s, loopvars)
              }

              val stage1stat = stage1.getStatSkel()
              val stage1expose = exposeDynGTSkeleton(stage1stat)
              val stage1dyn = stage1.getDynSkel()
              val f1: StagedFunction[DynGTSkeleton, Single] = zGT(stage1expose, DFT(stage1stat))
              val t1 = f1(stage1dyn)


              val stage2 = {
                val loopvars = ivecappend(mix.v, i)

                val before_fuse_gather = {
                  val base = SInt(0) //: Either[Rep[Int], Option[Int]] = Right(Some(0))
                  val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
                  val t1 = ivecappend(t0, m.toRep())
                  val t2 = ivecappend(t1, Const(1))
                  IMH(base, t2)
                }
                val s1_gather = fuseIM(mix.g, before_fuse_gather) //gather therefore swapped                
                val s1_scatter = fuseIM(mix.s, before_fuse_gather)

                mix.copy(x = t1, y = mix.y, n = k, loopbound = m, g = s1_gather, s = s1_scatter, loopvars)
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
        })
      }
      outer
    }


    def testexpose() = {
      val ini = iniGTSkeleton(None)
      val exp = exposeDynGTSkeleton(ini)
      val fresh = exp.freshExps()
      println(fresh)
      //val inidestruct = exp.t2vec(ini)
      val reconstruct = exp.vec2t(fresh)
      println(reconstruct)
      val deconstruct = exp.t2vec(reconstruct)
      println(deconstruct)
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

      stream2.println("package apps\n\ncase class Complex(val re: Double, val im: Double) {\n  def +(rhs: Complex): Complex = Complex(re + rhs.re, im + rhs.im)\n  def -(rhs: Complex): Complex = Complex(re - rhs.re, im - rhs.im)\n}\n\nclass ComplexVector(n: Int) {\n  val save = new Array[Complex](n)\n  def apply(i: Int): Complex = save(i)\n\n  def update(i: Int, y: Complex): ComplexVector = {\n    save(i) = y\n    this\n  }\n}\n\nobject VectorMult {\n  def apply(base: Int, strides: Vector[Int], loopvars: Vector[Int]): Int = base + loopvars.zip(strides).foldLeft(0)({(acc,ele) => acc + (ele._1 * ele._2)})\n}\n\nobject Testit extends App{\n  val t = new testClass\n  val one = Complex(1,0)\n  val zero = Complex(0,0)\n\n  val in = new ComplexVector(2)\n  val x1 = in.update(0,one)\n  val x2 = x1.update(1,zero)\n\n  val out = new ComplexVector(2)\n  t.callme(in,out,2,1,Vector.empty,1,Vector.empty,Vector.empty)\n\n}")
      val esc = codegen.emitSource((DFT(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
      //val esc = codegen.emitSource((DFTstart(ingt)), "testClass", stream2)(exposeDynGTSkeleton(ingt), exposeSingle)
      stream2.println("\n}\n")
      stream2.flush()
      stream2.close()
    }
  }


  val dsl = new MyDSLProgram
  //dsl.testexpose()
  //dsl.graphvizexport()
  dsl.codeexport()


}

