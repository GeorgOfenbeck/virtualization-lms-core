package apps


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


object DFTRecurseNoType extends App {


  class ComplexVector

  class Complex

  trait DFT_Exp extends BaseExp with FunctionsExp with IfThenElsePureExp with PurePrimitiveOpsExp with ImplicitOpsExp with ScalaCompile {

    case class Single(y: Rep[ComplexVector])

    case class Radix(n: Exp[Int]) extends Def[Int]

    def choose_radix(n: Exp[Int]): Exp[Int] = Radix(n)

    case class BaseCase(n: Exp[Int]) extends Def[Boolean]

    def isbasecase(n: Exp[Int]): Exp[Boolean] = BaseCase(n)

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

    case class SumLoop(till: Exp[Int], body: Exp[_ => _]) extends Def[ComplexVector => ComplexVector]

    def sumLoop[A](till: Rep[Int], body: Rep[Int] => Single)(implicit intexpose: ExposeRep[Rep[Int]], singleexpose: ExposeRep[Single]): Single = {
      val lambda = doInternalLambda(body,false,false)

      val newsyms = singleexpose.freshExps()
      val block = Block(newsyms)
      val sumloopnode = SumLoop(till,lambda.exp)
      val sumnodeexp = toAtom(sumloopnode)

      val returnNodes = if (newsyms.size > 1) {
        newsyms.zipWithIndex.map(fsym => {
          //had do to this ugly version since the compile time type is not know at this stage (of the return - Rep[_])
          val otp = exp2tp(fsym._1)
          val tag: TypeRep[Any] = otp.tag.asInstanceOf[TypeRep[Any]]
          val cc: Def[Any] = ReturnArg(sumnodeexp, fsym._1, fsym._2, true, newsyms.size == fsym._2 + 1)
          val newx = toAtom(cc)(tag, null)
          /*if (tag.mf.toString().contains("Function")) {
            val newtp = exp2tp(newx)
            println(newtp)
          }*/
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


  trait ScalaGenBaseCase extends ScalaCodegen {
    val IR: DFT_Exp

    import IR._


    override def emitNode(tp: TP[_], acc: Vector[String],
                          block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
      val ma = tp.rhs match {
        case BaseCase(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " == 2 //check for base case"))
        case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](" + quote(n) + ") //buffer creation"))
        case Radix(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " / 2 //stupid radix choice placeholder"))
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



    case class IMH(base: SInt, strides: Vector[SInt])

    case class GTSkeletonFull(x: Single, y: Single, n: SInt, g: IMH, s: IMH, v: Vector[Rep[Int]])


    implicit def exposeGTSkeletonFull(x: => GTSkeletonFull): ExposeRep[GTSkeletonFull] = {
      new ExposeRep[GTSkeletonFull]() {
        val freshExps = (u: Unit) => {
          def fa(r: Rep[Int]): Vector[Rep[_]] = Vector(Arg[Int])
          def fb(r: Int): Vector[Rep[_]] = Vector.empty
          val t = Vector(Arg[ComplexVector], Arg[ComplexVector]) ++
            x.n.i.fold(fa, fb) ++
            x.g.base.i.fold(fa, fb) ++
            x.g.strides.flatMap(p => p.i.fold(fa, fb)) ++
            x.s.base.i.fold(fa, fb) ++
            x.s.strides.flatMap(p => p.i.fold(fa, fb)) ++
            x.v
          t
        }
        val vec2t: Vector[Exp[_]] => GTSkeletonFull = (in: Vector[Exp[_]]) => {
          assert(in.size >= 2)
          def createimh(inv: Vector[Exp[_]], s: IMH): (Vector[Exp[_]], IMH) = {
            val (nb, ab): (Either[Rep[Int], Int], Vector[Exp[_]]) =
              s.base.i.fold(fa => {
                (Left(inv(0).asInstanceOf[Exp[Int]]), inv.tail)
              }, { fb => (Right(fb), inv) })

            val (ns, as): (Vector[Either[Exp[_], Int]], Vector[Exp[_]]) = s.strides.foldLeft(
              (Vector.empty[Either[Exp[_], Int]], ab)) {
              (acc, ele) => {
                val (nb, t0): (Either[Exp[_], Int], Vector[Exp[_]]) = ele.i.fold(fa => {
                  (Left(ab.head), ab.tail)
                }, { fb => (Right(fb), ab) })
                (acc._1 :+ nb, t0)
              }
            }
            val nsvec = ns.map(p => SInt(p.asInstanceOf[Either[Rep[Int], Int]]))
            (as, IMH(SInt(nb), nsvec))
          }

          val nx = Single(in(0).asInstanceOf[Exp[ComplexVector]])
          val ny = Single(in(1).asInstanceOf[Exp[ComplexVector]])
          val (nn, an): (Either[Rep[Int], Int], Vector[Exp[_]]) =
            x.n.i.fold(fa => {
              (Left(in(2).asInstanceOf[Exp[Int]]), in.tail.tail.tail)
            }, { fb => (Right(fb), in.tail.tail) })
          val (ag, ng) = createimh(an, x.g)
          val (as, ns) = createimh(ag, x.s)
          GTSkeletonFull(nx, ny, SInt(nn), ng, ns, as.asInstanceOf[Vector[Exp[Int]]])
        }

        val t2vec: GTSkeletonFull => Vector[Exp[_]] = (in: GTSkeletonFull) => {
          def fa(r: Rep[Int]): Vector[Rep[Int]] = Vector(r)
          def fb(r: Int): Vector[Rep[Int]] = Vector.empty
          Vector(in.x.y, in.y.y) ++
            x.n.i.fold(fa, fb) ++
            x.g.base.i.fold(fa, fb) ++
            x.g.strides.flatMap(p => p.i.fold(fa, fb)) ++
            x.s.base.i.fold(fa, fb) ++
            x.s.strides.flatMap(p => p.i.fold(fa, fb)) ++
            x.v
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


    implicit val exposeSingle = new ExposeRep[Single]() {
      val freshExps = (u: Unit) => Vector(Arg[ComplexVector])
      val vec2t: Vector[Exp[_]] => Single = (in: Vector[Exp[_]]) => Single(in.head.asInstanceOf[Rep[ComplexVector]])
      val t2vec: Single => Vector[Exp[_]] = (in: Single) => Vector(in.y)
    }


    def iniGTSkeleton(n: SInt): GTSkeletonFull = {
      val im = IMH(SInt(0), Vector(SInt(1))) //h0,1
      val inputvec = Single(Arg[ComplexVector])
      val outputvec = Single(Arg[ComplexVector])
      val ingt = if (n.i.isLeft)
        GTSkeletonFull(inputvec, outputvec, SInt(Arg[Int]), im, im, Vector.empty)
      else
        GTSkeletonFull(inputvec, outputvec, n, im, im, Vector.empty)
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
      val fbase = r.base + r.strides.head * s.base
      val zipit = r.strides.zipAll(s.strides, SInt(0), SInt(0)).map(p => p)
      val fstrides = Vector(r.strides.head * s.strides.head) ++ zipit.tail.map(p => p._1 + r.strides.head * p._2)
      IMH(fbase, fstrides)
    }

    // z is level above GT describtion
    def zGT(z: GTSkeletonFull, innerf: => (GTSkeletonFull => Single)): StagedFunction[GTSkeletonFull, Single] = {
      val f: (GTSkeletonFull => Single) = (z: GTSkeletonFull) => innerf(z)
      val t: StagedFunction[GTSkeletonFull, Single] = doLambda(f, false, true)(exposeGTSkeletonFull(z), exposeSingle)
      t
    }

    def DFT(): (GTSkeletonFull => Single) = {
      val outer: (GTSkeletonFull => Single) = (z: GTSkeletonFull) => {

        val sn: Rep[Int] = z.n.toRep()
        myifThenElse(isbasecase(sn), {
          F2(z)
        }, {
          val k = chooseRadix(z.n)
          val m = z.n / k
          //this is incorrect - should be size of other component
          sumLoop(z.n.toRep(),{
            i => {
              /*val stage1 = {
                val kvar = Arg[Int]
                val ivar = Arg[Int]
                val s1_gather = {
                  val base = SInt(0)
                  val strides = Vector(k, SInt(1))
                  val inner = IMH(base, strides)
                  //stop here -
                  fuseIM(z.g, inner) //gather therefore swapped
                }
                val s1_scatter = {
                  val base = SInt(0)
                  val strides = Vector(SInt(1), m)
                  val inner = IMH(base, strides)
                  fuseIM(z.s, inner)
                }
                z.copy(x = z.x, y = Single(veccreate(sn)), n = m, g = s1_gather, s = s1_scatter, Vector(kvar, ivar))
              }

              val f1: StagedFunction[GTSkeletonFull, Single] = zGT(stage1, DFT())
              val t1 = f1(stage1)

              val stage2 = {
                val mvar = Arg[Int]
                val ivar = Arg[Int]
                val before_fuse_gather = {
                  val base = SInt(0) //: Either[Rep[Int], Option[Int]] = Right(Some(0))
                  val strides = Vector(m, SInt(1)) //: Vector[Either[Rep[Int], Option[Int]]] = Vector(meither, Right(Some(1)))
                  IMH(base, strides)
                }
                val s1_gather = fuseIM(z.g, before_fuse_gather) //gather therefore swapped
                val s1_scatter = fuseIM(z.s, before_fuse_gather)
                z.copy(x = t1, y = z.y, n = k, g = s1_gather, s = s1_scatter, Vector(mvar, ivar))
              }
              val f2: StagedFunction[GTSkeletonFull, Single] = zGT(stage2, DFT())
              val t2 = f2(stage2)
              t2*/

              /*val f1: StagedFunction[GTSkeletonFull, Single] = zGT(z, DFT())
              val t1 = f1(z)
              t1*/

              sumLoop(z.n.toRep(),{
                i => z.x
              })
            }
          })



        })
      }
      outer
    }


    def graphvizexport() = {
      /*val evec: Vector[Either[Rep[Int],Option[Int]]] = Vector(Right(Some(1)))
      val im = IMH2(Right(Some(0)),evec,evec)
      val inputvec = Single(fresh[ComplexVector])
      val ingt = GTSkeletonFull(inputvec,Right(Some(256)),im,im,evec)*/

      lazy val ingt = iniGTSkeleton(SInt(Left(Arg[Int])))
      //val ingt = iniGTSkeleton(SInt(Right(8)))
      val (code, cm) = emitGraph.emitDepGraphf(DFT())(exposeGTSkeletonFull(ingt), exposeSingle)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

    def codeexport() = {
      lazy val ingt = iniGTSkeleton(SInt(Left(Arg[Int])))
      //val ingt = iniGTSkeleton(SInt(Right(8)))
      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
      val esc = codegen.emitSource(DFT(), "testClass", stream2)(exposeGTSkeletonFull(ingt), exposeSingle)
      stream2.flush()
      stream2.close()
    }

  }

  val dsl = new MyDSLProgram
  //dsl.graphvizexport()
  dsl.codeexport()

}

