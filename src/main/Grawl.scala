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

    def minus(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = minus(lhs, rhs)

    case class Times(lhs: Exp[Complex], rhs: Exp[Complex]) extends Def[Complex]

    def times(lhs: Exp[Complex], rhs: Exp[Complex]): Exp[Complex] = times(lhs, rhs)

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


  trait ScalaGenBaseCase extends ScalaCodegen with EmitHeadInternalFunctionAsClass {
    val IR: DFT_Exp

    import IR._


    override def emitNode(tp: TP[_], acc: Vector[String],
                          block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
      val ma = tp.rhs match {
        case BaseCase(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " == 2 //check for base case"))
        //case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](" + quote(n) + ") //buffer creation"))
        case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new ComplexVector(" + quote(n) + ") //buffer creation"))
        case IVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "Vector.empty[Int]"))
        case IVecAppend(v: Exp[Vector[Int]], y: Exp[Int]) => Vector(emitValDef(tp, quote(v) + " :+ " + quote(y)))
        case IVecApply(vec, i) => Vector(emitValDef(tp, quote(vec) + "(" + quote(i) + ")"))
        case IVecZipMagic(r, s) => Vector(emitValDef(tp, "Vector(" + quote(r) + ".head * " + quote(s) + ".head) ++ " + quote(r) + ".zipAll(" + quote(s) + ",0,0).map(p => p._1 + " + quote(r) + ".head * p._2)"))


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

        case _ => super.emitNode(tp, acc, block_callback)
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
          val s = v.tail.asInstanceOf[Exp[Vector[Int]]]
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

    case class StatGTSkeleton(n: Option[Int], loopbound: Option[Int], g: StatIMH, s: StatIMH, v: Rep[Vector[Int]])

    case class GTSkeletonFull(x: Single, y: Single, n: SInt, loopbound: SInt, g: IMH, s: IMH, v: Rep[Vector[Int]]) {
      def getDynIMH() = {
        val on: Option[Rep[Int]] = n.i.fold(fa => Some(fa), fb => None)
        val ol: Option[Rep[Int]] = loopbound.i.fold(fa => Some(fa), fb => None)
        DynGTSkeleton(x, y, on, ol, g.getDynIMH(), s.getDynIMH(), v)
      }

      def getStatIMH() = {
        val on: Option[Int] = n.i.fold(fa => None, fb => Some(fb))
        val ol: Option[Int] = loopbound.i.fold(fa => None, fb => Some(fb))
        StatGTSkeleton(on, ol, g.getStatIMH(), s.getStatIMH(), v)
      }
    }

    implicit def exposeDynGTSkeleton(stat: StatGTSkeleton): ExposeRep[DynGTSkeleton] = {
      new ExposeRep[DynGTSkeleton]() {
        val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
          val fn = if (stat.n.isEmpty) Vector(Arg[Vector[Int]]) else Vector.empty
          val fl = if (stat.loopbound.isEmpty) Vector(Arg[Vector[Int]]) else Vector.empty
          exposeSingle.freshExps() ++ exposeSingle.freshExps() ++
            fn ++ fl ++ stat.g.freshExps() ++ stat.s.freshExps()
        }
        val vec2t: Vector[Exp[_]] => DynGTSkeleton = (in: Vector[Exp[_]]) => {
          assert(in.size >= 2)
          val x = exposeSingle.vec2t(in)
          val singlesize = exposeSingle.t2vec(x).size
          val outx = in.drop(singlesize)
          val y = exposeSingle.vec2t(outx)
          val outy = in.drop(singlesize)
          val (n, outn) = if (stat.n.isEmpty) (Some(outy.head.asInstanceOf[Rep[Int]]), outy.tail) else (None, outy)
          val (l, outl) = if (stat.loopbound.isEmpty) (Some(outn.head.asInstanceOf[Rep[Int]]), outn.tail) else (None, outn)
          val (g, outg) = stat.g.vec2t(outl)
          val (s, outs) = stat.s.vec2t(outg)
          val v = outs.asInstanceOf[Exp[Vector[Int]]]
          DynGTSkeleton(x, y, n, l, g, s, v)
        }
        val t2vec: DynGTSkeleton => Vector[Exp[_]] = (in: DynGTSkeleton) => {
          val vn = in.n.map(p => Vector(p)).getOrElse(Vector.empty)
          val vl = in.loopbound.map(p => Vector(p)).getOrElse(Vector.empty)
          Vector(in.x.y, in.y.y) ++ vn ++ vl ++ in.g.t2vec() ++ in.s.t2vec() ++ Vector(in.v)
        }
      }
    }


    implicit def exposeGTSkeletonFull(x: => GTSkeletonFull): ExposeRep[GTSkeletonFull] = {
      new ExposeRep[GTSkeletonFull]() {

        val freshExps = (u: Unit) => {
          def fa(r: Rep[Int]): Vector[Rep[_]] = Vector(Arg[Int])
          def fb(r: Int): Vector[Rep[_]] = Vector.empty
          val nfold = x.n.i.fold(fa, fb)
          val t = Vector(Arg[ComplexVector], Arg[ComplexVector]) ++
            Vector(Arg[Vector[Int]]) ++
            nfold ++
            x.loopbound.i.fold(fa, fb) ++
            x.g.base.i.fold(fa, fb) ++
            Vector(Arg[Vector[Int]]) ++
            //x.g.strides.flatMap(p => p.i.fold(fa, fb)) ++
            x.s.base.i.fold(fa, fb) ++
            Vector(Arg[Vector[Int]])
          //x.s.strides.flatMap(p => p.i.fold(fa, fb))
          //x.v.flatMap(r => fa(r))

          t
        }

        val vec2t: Vector[Exp[_]] => GTSkeletonFull = (in: Vector[Exp[_]]) => {
          assert(in.size >= 2)
          def createimh(inv: Vector[Exp[_]], s: IMH): (Vector[Exp[_]], IMH) = {
            val (nb, ab): (Either[Rep[Int], Int], Vector[Exp[_]]) =
              s.base.i.fold(fa => {
                (Left(inv(0).asInstanceOf[Exp[Int]]), inv.tail)
              }, { fb => (Right(fb), inv) })

            (ab.tail, IMH(SInt(nb), ab.head.asInstanceOf[Rep[Vector[Int]]]))
            /*val (ns, as): (Vector[Either[Exp[_], Int]], Vector[Exp[_]]) = s.strides.foldLeft(
              (Vector.empty[Either[Exp[_], Int]], ab)) {
              (acc, ele) => {
                val (nb, t0): (Either[Exp[_], Int], Vector[Exp[_]]) = ele.i.fold(fa => {
                  (Left(ab.head), ab.tail)
                }, { fb => (Right(fb), ab) })
                (acc._1 :+ nb, t0)
              }

            }
            val nsvec = ns.map(p => SInt(p.asInstanceOf[Either[Rep[Int], Int]]))
            (as, IMH(SInt(nb), nsvec))*/

          }

          val nx = Single(in(0).asInstanceOf[Exp[ComplexVector]])
          val ny = Single(in(1).asInstanceOf[Exp[ComplexVector]])
          val nv = in(2).asInstanceOf[Exp[Vector[Int]]]
          val (nn, an): (Either[Rep[Int], Int], Vector[Exp[_]]) =
            x.n.i.fold(fa => {
              (Left(in(3).asInstanceOf[Exp[Int]]), in.tail.tail.tail.tail)
            }, { fb => (Right(fb), in.tail.tail.tail) })

          val (nl, al): (Either[Rep[Int], Int], Vector[Exp[_]]) =
            x.loopbound.i.fold(fa => {
              (Left(an.head.asInstanceOf[Exp[Int]]), an.tail)
            }, { fb => (Right(fb), an) })

          val (ag, ng) = createimh(al, x.g)
          val (as, ns) = createimh(ag, x.s)
          GTSkeletonFull(nx, ny, SInt(nn), SInt(nl), ng, ns, nv)
        }

        val t2vec: GTSkeletonFull => Vector[Exp[_]] = (in: GTSkeletonFull) => {
          def fa(r: Rep[Int]): Vector[Rep[Int]] = Vector(r)
          def fb(r: Int): Vector[Rep[Int]] = Vector.empty
          Vector(in.x.y, in.y.y) ++
            Vector(in.v) ++
            in.n.i.fold(fa, fb) ++
            in.loopbound.i.fold(fa, fb) ++
            in.g.base.i.fold(fa, fb) ++
            Vector(in.g.strides) ++
            //in.g.strides.flatMap(p => p.i.fold(fa, fb)) ++
            in.s.base.i.fold(fa, fb) ++
            Vector(in.s.strides)
          //in.s.strides.flatMap(p => p.i.fold(fa, fb))

        }
      }
    }


    type NoRep[T] = T

    trait SD[R[_]] {
      def unit[T: TypeRep](x: R[T]): Rep[T]

      def fresh[T: TypeRep]: Vector[Rep[_]]

      def get[T](x: R[T]): Vector[Rep[_]]

      def sv(): Boolean
    }

    implicit object SDRep extends SD[Rep] {
      def unit[T: TypeRep](x: Rep[T]): Rep[T] = x

      def fresh[T: TypeRep]: Vector[Rep[_]] = Vector(Arg[T])

      def get[T](x: Rep[T]): Vector[Rep[_]] = Vector(x)

      def sv() = true
    }

    implicit object SDNoRep extends SD[NoRep] {
      def unit[T: TypeRep](x: NoRep[T]): Rep[T] = Const(x)

      def fresh[T: TypeRep]: Vector[Rep[_]] = Vector.empty

      def get[T](x: NoRep[T]): Vector[Rep[_]] = Vector.empty

      def sv() = false
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


    def iniGTSkeleton(n: SInt): GTSkeletonFull = {
      val im = IMH(SInt(Const(0)), Arg[Vector[Int]]) //h0,1
      //val im = IMH(SInt(0), Vector(SInt(1))) //h0,1
      val inputvec = Single(Arg[ComplexVector])
      val outputvec = Single(Arg[ComplexVector])
      val ingt = if (n.i.isLeft)
        GTSkeletonFull(inputvec, outputvec, SInt(Arg[Int]), SInt(1), im, im, Arg[Vector[Int]])
      else
        GTSkeletonFull(inputvec, outputvec, n, SInt(1), im, im, Arg[Vector[Int]])
      ingt
    }


    def F2(z: GTSkeletonFull): Single = {
      /*val target: Single = z.y
      val scatterim = z.s
      val gatherim = z.g

      val gindex1: Rep[Int] = ???
      val gindex2: Rep[Int] = ???

      val sindex1: Rep[Int] = ???
      val sindex2: Rep[Int] = ???

      val t1 = vecapply(z.x.y, gindex1)
      val t2 = vecapply(z.x.y, gindex2)

      val cres1 = plus(t1, t2)
      val cres2 = minus(t1, t2)


      val res1 = vecupdate(target.y, sindex1, cres1)
      val res2 = vecupdate(res1, sindex1, cres2)
      Single(res2)*/
      z.x
    }


    def static_chooseRadix(n: Int) = n / 2

    def chooseRadix(n: SInt) = n.i.fold(fa => SInt(choose_radix(fa)), fb => SInt(static_chooseRadix(fb)))

    def fuseIM(r: IMH, s: IMH): IMH = {
      val ss0 = ivecapply(r.strides, Const(0))
      val fbase = r.base + SInt(ss0) * s.base
      val fstrides = iveczipmagic(r.strides, s.strides)
      IMH(fbase, fstrides)
    }

    // z is level above GT describtion
    def zGT(z: GTSkeletonFull, innerf: => (GTSkeletonFull => Single)): StagedFunction[GTSkeletonFull, Single] = {
      val f: (GTSkeletonFull => Single) = (wuf: GTSkeletonFull) => innerf(wuf)
      //val t: StagedFunction[GTSkeletonFull, Single] = doLambda(f, false)(exposeGTSkeletonFull(z), exposeSingle)
      val t: StagedFunction[GTSkeletonFull, Single] = doGlobalLambda(f, true)(exposeGTSkeletonFull(z), exposeSingle)
      t
    }


    def DFT(): (GTSkeletonFull => Single) = {
      val outer: (GTSkeletonFull => Single) = (z: GTSkeletonFull) => {
        val sn: Rep[Int] = z.n.toRep()
        val cond = isbasecase(sn)
        myifThenElse(cond, {
          F2(z)
        }, {
          val m = chooseRadix(z.n)
          val k = z.n / m
          /*val f1: StagedFunction[GTSkeletonFull, Single] = zGT(z, DFT())
          val t1 = f1(z)
          t1*/

          val res = sumFold(z.loopbound.toRep(), Single(veccreate(z.n.toRep())), {
            isingle => {
              val i = isingle.i
              val acc = isingle.s
              val stage1 = {
                val loopvars = ivecappend(z.v, i)
                val s1_gather = {
                  val base = SInt(0)
                  val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
                  val t1 = ivecappend(t0, k.toRep())
                  val t2 = ivecappend(t1, Const(1))
                  //val t2 = ivecappend(t1,Const(279))
                  //val strides = Vector(k, SInt(1))
                  val inner = IMH(base, t2)
                  //val inner = IMH(base, strides)
                  fuseIM(z.g, inner) //gather therefore swapped
                }
                val s1_scatter = {
                  val base = SInt(0)
                  //val strides = Vector(SInt(1), m)
                  val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
                  val t1 = ivecappend(t0, Const(1))
                  val t2 = ivecappend(t1, m.toRep())
                  val inner = IMH(base, t2)
                  //inner
                  fuseIM(z.s, inner)
                }
                z.copy(x = z.x, y = acc, n = m, loopbound = k, g = s1_gather, s = s1_scatter, loopvars)
              }
              //val f1: StagedFunction[GTSkeletonFull, Single] = zGT(stage1, DFT())
              //val t1 = f1(stage1)
              val f1: StagedFunction[GTSkeletonFull, Single] = zGT(z, DFT())
              val t1 = f1(z)
              //t1

              val stage2 = {
                //val loopvars = z.v ++ Vector(i)
                val loopvars = ivecappend(z.v, i)
                val before_fuse_gather = {
                  val base = SInt(0) //: Either[Rep[Int], Option[Int]] = Right(Some(0))
                  val t0 = iveccreate(m.toRep()) //dirty workaround to fix it inside the function
                  val t1 = ivecappend(t0, m.toRep())
                  val t2 = ivecappend(t1, Const(1))
                  //val strides = Vector(m, SInt(1)) //: Vector[Either[Rep[Int], Option[Int]]] = Vector(meither, Right(Some(1)))
                  IMH(base, t2)
                }
                val s1_gather = fuseIM(z.g, before_fuse_gather) //gather therefore swapped
                //val s1_gather = before_fuse_gather
                val s1_scatter = fuseIM(z.s, before_fuse_gather)
                //val s1_scatter = before_fuse_gather
                //val s1_gather = z.g
                //val s1_scatter = z.s
                //GTSkeletonFull(t1,z.y,z.n, s1_gather, s1_scatter, loopvars)
                z.copy(x = t1, y = z.y, n = k, loopbound = m, g = s1_gather, s = s1_scatter, loopvars)
              }
              val f2: StagedFunction[GTSkeletonFull, Single] = zGT(stage2, DFT())
              val t2 = f2(stage2)
              t2
            }
          })
          res
        })
      }
      outer
    }


    def testexpose() = {
      val ini = iniGTSkeleton(SInt(Arg[Int]))
      val exp = exposeGTSkeletonFull(ini)

      val fresh = exp.freshExps()
      val inidestruct = exp.t2vec(ini)
      val reconstruct = exp.vec2t(fresh)
      val deconstruct = exp.t2vec(reconstruct)
      println(deconstruct)
    }


    def graphvizexport() = {
      lazy val ingt = iniGTSkeleton(SInt(Left(Arg[Int])))
      val (code, cm) = emitGraph.emitDepGraphf(DFT())(exposeGTSkeletonFull(ingt), exposeSingle)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

    def codeexport() = {
      lazy val ingt = iniGTSkeleton(SInt(Left(Arg[Int])))
      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
      stream2.println("package apps")
      stream2.println("class ComplexVector(n: Int)")
      val esc = codegen.emitSource(DFT(), "testClass", stream2)(exposeGTSkeletonFull(ingt), exposeSingle)
      stream2.flush()
      stream2.close()
    }
  }


  val dsl = new MyDSLProgram
  dsl.testexpose()
  //dsl.graphvizexport()
  dsl.codeexport()


}

