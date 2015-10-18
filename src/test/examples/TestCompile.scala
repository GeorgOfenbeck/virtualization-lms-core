

package examples


import java.io.PrintWriter

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._



import org.scalatest._

import scala.lms.targets.scalalike._

import scala.lms.targets.graphviz.GraphVizExport


/*trait TestExp extends BooleanOpsExp{

  case class Nest(b: Block) extends Def[Boolean]
  def nest(b: Exp[Boolean]) = Nest(Block(Vector(b)))

  override def boundExps(e: Any): Vector[Exp[_]] = e match{
    case Nest(b) => b.res
    case _ => {
      super.boundExps(e)
    }
  }
}*/
/*
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
}*/

class TestCompile extends Suite {
  def testdsl(): Unit =  {

    class DSL extends BooleanOpsExp with PurePrimitiveOpsExp with FunctionsExp with IfThenElsePureExp with ScalaCompile  with ImplicitOpsExp{
      self =>
      override val codegen = new ScalaCodegen
        with EmitHeadInternalFunctionAsClass
        with ScalaGenBooleanOps
        with ScalaGenPrimitivOps
        with ScalaGenIfThenElse
        //with ScalaGenTest
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
/*      implicit def exposeFunction[A,R](implicit args: ExposeRep[A], returns: ExposeRep[R]): ExposeRep[StagedFunction[A,R]] =
        new ExposeRep[StagedFunction[A,R]]() {
          val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
            //we have no knowledge of the function body which we don't care for
            //create a new StagedFunction
            //val fargs = args.freshExps()
            //val freturns = returns.freshExps()
            def helper[T]()(implicit tag: TypeRep[T]): TypeRep[T] = {
              tag match {
                case x@TypeExp(mf,dynTags) => {
                  val f: Unit => (Vector[TypeRep[_]],Vector[TypeRep[_]]) = (u: Unit) => {
                    val a = args.freshExps().map( ele => exp2tp(ele).tag)
                    val r = returns.freshExps().map( ele => exp2tp(ele).tag)
                    //(Vector.empty[TypeExp[_]],Vector.empty[TypeExp[_]])
                    (a,r)
                  }
                  x.copy(dynTags = Some(f))
                }
                case _ => {
                  assert(false, "this should never match")
                  tag
                }
              }
            }
            val tagnew = helper[_ => _ ]
            val lambda: Exp[Function[_,_]] = Arg[_ => _ ](tagnew)
            //val ele: (Vector[Exp[_]],Vector[Exp[_]]) = (fargs,freturns)
            //funexp2AR = funexp2AR + (lambda -> ele)
            //funexp2StagedFunction = funexp2StagedFunction + (lambda -> StagedFunction(null,lambda,args,returns))
            Vector(lambda)
          }
          val vec2t: Vector[Exp[_]] => StagedFunction[A,R] = (in: Vector[Exp[_]]) => {
            val f : (A => R) = (ina: A) => ???
            StagedFunction(f,in.head.asInstanceOf[Rep[_=>_]],args,returns)
          }
          val t2vec: StagedFunction[A,R] => Vector[Exp[_]] = (in: StagedFunction[A,R]) => {
            Vector(in.exp)
          }
        }*/


      val FunctionOnComplex: Complex => StagedFunction[Complex,Complex] = (in: Complex) => {
        val FunctionOnComplex1: Complex => Complex = (in1: Complex) => {
          val repfun: (Rep[Int] => Rep[Int]) = (in: Rep[Int]) => in
          val reps = doLambda(repfun)
          Complex(in1.im, reps(in.re))
        }
        val sf = doLambda(FunctionOnComplex1)
        sf
      }

      /*
      def mystagedf(x: StagedFunction[Complex,Complex]): StagedFunction[Complex,Complex] = {
        /*val g: StagedFunction[Complex,Complex] => StagedFunction[Complex,Complex] =
          (inf: StagedFunction[Complex,Complex] ) => inf
        val sf = doLambda(g)

        val c1 = sf(x)
        c1*/
        val c = Complex(unit(3),unit(4))
        val innerf: Complex => Complex = (in: Complex) => {
          /*val b = Complex(c.re + c.re, c.im + c.im)
          val res = myifThenElse(unit(true),c,b)
          res*/

          val g: Complex => Complex = (in: Complex ) => {
            in
          }

          val ret = doLambda(g)
          g(in)



        }



        //val sf = doLambda(FunctionOnComplex)
        val ret = doLambda(innerf)
        ret

      }
      */


      def createsf(deepth: Int): StagedFunction[Rep[Int],Rep[Int]] = {

        if (deepth == 0) {
          val f: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {
            val t = unit(1)
            i + t

          }
          val sf = doLambda(f)
          sf
        } else {
          val f = createsf(deepth - 1)
          val g = createsf(deepth - 1)
          val h: Rep[Int] => Rep[Int] = (i: Rep[Int]) => {

            val nums = for (j <- 0 until 100)
              yield (i + unit(j))

            val t0 = nums.reduce( (a,b) => {
              val t = a + b
              t
            })
                                                                                  val t1 = t0
            val t2 = t0 * unit(-1)

            val t3 = t1 + t2
            val c = i - t3
            f.apply(c) + g.apply(c)
          }
          val sf = doLambda(h)
          sf
        }
      }

      val mystagedf: Rep[Int] => Rep[Int] = (i: Rep[Int]) => createsf(5).apply(i)
      //val iarg = exposeRepFromRep[Int]
      //val inest = exposeFunction[Complex,Complex]
      //val iret = exposeFunction[Complex,Complex => Complex](exposeComplex,inest)
      //val iret = exposeFunction[Complex,Complex]
      //val iret = exposeComplex
      val iarg = exposeRepFromRep[Int]
      val iret = exposeRepFromRep[Int]
      //val iarg = exposeComplex
      //val iret = exposeComplex

      //val iarg = exposeFunction[Complex,Complex]
      //val iret = exposeFunction[Complex,Complex]
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
/*



//Rep[A] => Rep[B]
implicit def exposeFunction[A,R](implicit args: ExposeRep[A], returns: ExposeRep[R]): ExposeRep[A => R] =
new ExposeRep[A => R]() {
val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
//we do not have any knowledge about the internals of the function - just its arg / return type
/*            val tf: (A => R) = (in: A) => {
              returns.vec2t(returns.freshExps())
            }
            val fun = doLambda(tf)
            val funtp = fun2tp(fun)
            Vector(funtp.sym)*/
val ret: Rep[_ => _] = Arg[_ => _]
Vector(ret)
}
val vec2t: Vector[Exp[_]] => (A => R) = (in: Vector[Exp[_]]) => {
//val ftp = exp2tp(in.head)
val f: A => R = (in: A) => ???
/*ftp.rhs match {
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
}*/
f
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
*/



