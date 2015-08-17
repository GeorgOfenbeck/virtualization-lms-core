package examples


import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._



import org.scalatest._

import scala.lms.targets.scalalike._

import scala.lms.targets.graphviz.GraphVizExport


trait TestExp extends BooleanOpsExp{

  case class Nest(b: Block) extends Def[Boolean]
  def nest(b: Exp[Boolean]) = Nest(Block(Vector(b)))

  override def boundExps(e: Any): Vector[Exp[_]] = e match{
    case Nest(b) => b.res
    case _ => {
      super.boundExps(e)
    }
  }
}

trait ScalaGenTest extends ScalaCodegen {
  val IR: TestExp

  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block, String) => String): String = {
    val ma = tp.rhs match {
      case Nest(b) => "<Nest" + quote(tp) + ">\n" + block_callback(b, "") + "</Nest" + quote(tp) + ">\n"
      case _ => super.emitNode(tp, acc, block_callback)
    }
    ma
  }
}

class TestCompile extends Suite {
  def testdsl(): Unit =  {

    class DSL extends BooleanOpsExp with PurePrimitiveOpsExp with InternalFunctionsExp with IfThenElseExp with ScalaCompile with TestExp with ImplicitOpsExp{
      self =>
      override val codegen = new ScalaCodegen
        with EmitHeadInternalFunctionAsClass
        with ScalaGenBooleanOps
        with ScalaGenPrimitivOps
        with ScalaGenIfThenElse
        with ScalaGenTest
      {
        val IR: self.type = self
      }
      val emitGraph = new GraphVizExport {
        override val IR: self.type = self
      }

      def f (x: Int) = x
      sameFunction(f _, f _)

      case class Complex(re: Rep[Int], im: Rep[Int])

      implicit val exposeComplex = new ExposeRep[Complex](){
        val freshExps = (u: Unit) => Vector(Arg[Int],Arg[Int])
        val vec2t: Vector[Exp[_]] => Complex = (in: Vector[Exp[_]]) => Complex(in.head.asInstanceOf[Rep[Int]],in.tail.head.asInstanceOf[Rep[Int]])
        val t2vec: Complex => Vector[Exp[_]] = (in: Complex) => Vector(in.re,in.im)
      }

      //Rep[A] => Rep[B]
      implicit def exposeFunction[A,R](implicit args: ExposeRep[A], returns: ExposeRep[R]): ExposeRep[A => R] =
        new ExposeRep[A => R]() {
          val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
            //we do not have any knowledge about the internals of the function - just its arg / return type
            val tf: (A => R) = (in: A) => {
              returns.vec2t(returns.freshExps())
            }
            val fun = doLambda(tf)
            val funtp = fun2tp(fun)
            Vector(funtp.sym)
          }
          val vec2t: Vector[Exp[_]] => (A => R) = (in: Vector[Exp[_]]) => {
            val ftp = exp2tp(in.head)
            ftp.rhs match {
              case ReturnArg(f,newsym,pos,tuple,last) => {
                val ftp2 = exp2tp(newsym)
                val fun = tp2fun(ftp2)
                val ret: Function[A, R] = fun match {
                  case x: Function[A, R] => x
                  case _ => ???
                }
                ret
              }
              case InternalLambda(f,x,y,args,returns)=> {
                val fun = tp2fun(ftp)
                val ret: Function[A, R] = fun match {
                  case x: Function[A, R] => x
                  case _ => ???
                }
                ret
              }
              case _ => ???
            }

          }
          val t2vec: (A => R) => Vector[Exp[_]] = (in: (A => R)) => {
            val ftp = fun2tp(in)
            Vector(ftp.sym)
          }
        }
          /*{
          val ret: Def[_ => _] = funtp.rhs match {
            case il: InternalLambda => il
            case _ => {
              ??? //should not happen
            }
          }
          ret
        }
        val vec2t: Vector[Exp[_]] => (A => R) = (in: Vector[Exp[_]]) => {
          val ret: (A => R) = funtp.rhs match {
            case il: InternalLambda => {
              il.createApply(in.head.asInstanceOf[Exp[_=>_]])
            }
            case _ => {
              ??? //should not happen
            }
          }
        }
         => Vector(funtp.sym)
      }  */



      
      val FunctionOnComplex: Complex => (Complex => Complex) = (in: Complex) => {
        //val sh = doLambda(innerh)
        //Complex(in.im,sh(in.re))
        val FunctionOnComplex1: (Complex => Complex) = (in1: Complex) => {
          Complex(in1.im, in.re)
        }
        val sf = doLambda(FunctionOnComplex1)
        sf
      }


      def mystagedf(x: Complex => Complex): (Complex => Complex) = {
        //val complex = Complex(Const(1) + Const(1),Const(2) + Const(2))
        //val sf = doLambda(innerf)
        //val stageFunctiononComplex = doLambda(FunctionOnComplex)
        //val retf = stageFunctiononComplex(complex)
        /*val r2 = retf(complex)
        r2.re*/

        //retf
        //val ret = stageFunctiononComplex(complex)
        //ret
        //x(complex)

        x

        //val retcomplex = stageFunctiononComplex(complex)
        //val ret = sf(retcomplex.im)
        //val sg = doLambda(innerf)
        //sg(x)
        //retcomplex.re
        //nest(nest(boolean_negate(x)))
      }

      //val iarg = exposeRepFromRep[Int]
      //val inest = exposeFunction[Complex,Complex]
      //val iret = exposeFunction[Complex,Complex => Complex](exposeComplex,inest)
      //val iret = exposeFunction[Complex,Complex]
      //val iret = exposeComplex

      val iarg = exposeFunction[Complex,Complex]
      val iret = exposeFunction[Complex,Complex]
    }
    val dsl = new DSL
    //val (code, esc) = dsl.emitString.emit("",dsl.mystagedf)(dsl.iarg,dsl.iret)


    //val esc = dsl.codegen.emitSource(dsl.mystagedf,"testClass",new PrintWriter(System.out))(dsl.iarg,dsl.iret)

    val (code, cm) = dsl.emitGraph.emitDepGraphf(dsl.mystagedf)(dsl.iarg,dsl.iret)
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("check.dot"))
    stream.println(code)
    stream.flush()
    stream.close()

    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Test.scala"))
    val esc = dsl.codegen.emitSource(dsl.mystagedf,"testClass",stream2)(dsl.iarg,dsl.iret)
    stream2.flush()
    stream2.close()
    //dsl.compile(dsl.mystagedf)(dsl.iarg,dsl.iret)
    //println(code)
    //println("hae?")
  }
}



