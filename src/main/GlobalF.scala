/*
package apps


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


object GlobalF extends App {


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

    def iveczipmagic(vec1: Exp[Vector[Int]], vec2: Exp[Vector[Int]]): Exp[Vector[Int]] = IVecZipMagic(vec1,vec2)

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
        case IVecZipMagic(r,s) =>  Vector(emitValDef(tp, "Vector(" + quote(r) + ".head * " + quote(s) + ".head) ++ " + quote(r) + ".zipAll(" + quote(s) + ",0,0).map(p => p._1 + " + quote(r) + ".head * p._2)"))

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


    object Mix{
      def apply(s: SMix, d: DMix): Mix = {
        val na: Either[Rep[Int], Int] = (s.a,d.a) match {
          case (None, Some(r)) => Left(r)
          case (Some(i), None) => Right(i)
          case _ => ???
        }
        val nb: Either[Rep[Int], Int] = (s.b,d.b) match {
          case (None, Some(r)) => Left(r)
          case (Some(i), None) => Right(i)
          case _ => ???
        }
        Mix.apply(SInt(na),SInt(nb),d.c)
      }
    }


    case class Mix(a: SInt, b: SInt, c: Rep[Int])
    {
      def getSMix(): SMix = {
        val sa = a.i.fold(fa => None, fb => Some(fb))
        val sb = b.i.fold(fa => None, fb => Some(fb))
        SMix(sa,sb)
      }
      def getDMix(): DMix = {
        val da = a.i.fold(fa => Some(fa), fb => None)
        val db = b.i.fold(fa => Some(fa), fb => None)
        DMix(da,db,c)
      }
    }
    case class SMix(a: Option[Int], b: Option[Int])

    case class DMix(a: Option[Rep[Int]], b: Option[Rep[Int]], c: Rep[Int])


    implicit def exposeDMix(sm: SMix): ExposeRep[DMix] = {
      new ExposeRep[DMix](){
        val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
          val oa =  if (sm.a.isEmpty) Vector(Arg[Int]) else Vector.empty
          val ob = if (sm.b.isEmpty) Vector(Arg[Int]) else Vector.empty
          Vector.empty[Exp[_]] ++ oa ++ ob ++ Vector(Arg[Int])
        }
        val vec2t: Vector[Exp[_]] => DMix = (in: Vector[Exp[_]]) => {
          val (aa, na) = if(sm.a.isEmpty) (in.tail,Some(in.head.asInstanceOf[Rep[Int]])) else (in,None)
          val (ab, nb) = if(sm.b.isEmpty) (aa.tail,Some(aa.head.asInstanceOf[Rep[Int]])) else (aa,None)
          val nc = ab.head.asInstanceOf[Rep[Int]]
          DMix(na,nb,nc)
        }
        val t2vec: DMix => Vector[Exp[_]]  = (in: DMix) => {
          val na = in.a.map(p => Vector(p)).getOrElse(Vector.empty)
          val nb = in.b.map(p => Vector(p)).getOrElse(Vector.empty)
          Vector.empty[Exp[_]] ++ na ++ nb ++ Vector(in.c)
        }
      }
    }


    def GT(dexpose: ExposeRep[DMix], innerf: DMix => Rep[Int]): StagedFunction[DMix, Rep[Int]] = {
      val inner: DMix => Rep[Int] = (wuf: DMix) => innerf(wuf)
      val intexp: ExposeRep[Rep[Int]] = exposeRepFromRep[Int]
      val t: StagedFunction[DMix, Rep[Int]] = doGlobalLambda(inner, true) (dexpose,intexp)
      t
    }

    def myprog(s: SMix): (DMix => Rep[Int]) = {
      val outer: (DMix => Rep[Int]) = (z: DMix) => {
        val mixin = Mix(s,z)
        val changea = mixin.a * mixin.b * SInt(mixin.c)
        val newmix = mixin.copy(a = changea)
        val static_info: SMix = newmix.getSMix()
        val dyn_info: DMix = newmix.getDMix()
        val exp = exposeDMix(static_info)


        val f = myprog(static_info)
        val f1: StagedFunction[DMix, Rep[Int]] = GT(exp, f)
        val frew = f1(dyn_info)
        /*
        val cond = isbasecase(changea.toRep())
        val res = myifThenElse(cond, {
          z.c
        },{
          frew
          //f1(dyn_info)
          //z.c
        })
        res */
        frew
      }
      outer
    }


    def wrap(s: SMix): (DMix => Rep[Int]) = {
      val outer: (DMix => Rep[Int]) = (z: DMix) => {
        val intexp: ExposeRep[Rep[Int]] = exposeRepFromRep[Int]
        val smix = SMix(Some(1),Some(2))
        val exp = exposeDMix(smix)
        val dmix = DMix(None,None,z.c)
        val f1: StagedFunction[DMix, Rep[Int]] = doGlobalLambda(myprog(smix), true)(exp, intexp)
        //val f1: StagedFunction[DMix, Rep[Int]] = doLambda(myprog(smix),true, true)(exp, intexp)
        f1(dmix)
      }
      outer
    }




   def myprog2(r: Rep[Int]): Rep[Int] = {
     val hot = true

     /*def g(x: Rep[Int]): Rep[Int] = {
       x
     }
     val ft = doLambda(g,true)*/

     def f(x: Rep[Int]): Rep[Int] = {

       def g(r: Rep[Int]): Rep[Int] = {
        r
       }

       val ft = doGlobalLambda(g, true)

       val fs = doGlobalLambda(f, true)

       //val cond = isbasecase(x)
       //val t: Rep[Int] = myifThenElse(cond, {fs(x)},{ ft(x)})
       fs(ft(x))
       //t
     }
     val fs =  doGlobalLambda(f, true)
     fs(r)


   }





    def graphvizexport() = {

      val smix = SMix(None,None)
      val expose = exposeDMix(smix)
      val intexp: ExposeRep[Rep[Int]] = exposeRepFromRep[Int]
      val f: (DMix => Rep[Int]) = wrap(smix)
      //val f: (DMix => Rep[Int]) = myprog(smix)
      val (code, cm) = emitGraph.emitDepGraphf(f)(expose,intexp)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("DFT_recursion_step.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

    def codeexport() = {
      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
      val smix = SMix(None,None)
      val expose = exposeDMix(smix)
      val intexp: ExposeRep[Rep[Int]] = exposeRepFromRep[Int]
      val f: (DMix => Rep[Int]) = wrap(smix)
      //val f: (DMix => Rep[Int]) = myprog(smix)
      val esc = codegen.emitSource(f, "testClass", stream2)     (expose,intexp)
      stream2.flush()
      stream2.close()
    }
  }


  val dsl = new MyDSLProgram


  dsl.codeexport()
  //dsl.graphvizexport()


}

*/
