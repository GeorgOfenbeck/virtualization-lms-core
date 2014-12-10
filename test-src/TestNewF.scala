
import java.io.PrintWriter

import _root_.util.CMGraphExport
import org.scalatest.FunSpec
import scala.virtualization.lms._
import common._
import scala.virtualization.lms.internal._

class TestNewF extends FunSpec{


  describe("trying the new style functions") {





    class DSL extends NumericOpsExp with PureFunctionsExp with ReifyPure{
      self =>
      val IR: self.type = self




      case class Complex[T](val re: Rep[T], val im: Rep[T])

      import scala.reflect.runtime.universe._

      implicit def exposeRepFromComplex[T](implicit tag: TypeTag[T]): ExposeRep[Complex[T]] = new ExposeRep[Complex[T]](){
        val freshExps: Unit => hlist = (u: Unit) => Vector(fresh[T](tag), fresh[T](tag))
        val hlist2t: hlist => Complex[T] = (v: hlist) => {
          val re: Exp[T] = v(0).asInstanceOf[Exp[T]] //RF!!!!
          val im: Exp[T] = v(1).asInstanceOf[Exp[T]]
          Complex(re,im)
        }
        val t2hlist: Complex[T] => hlist = (c: Complex[T]) => Vector(c.re,c.im)
      }


      case class ComplexVector[T](val vec: Vector[Complex[T]])


      implicit def exposeRepFromVComplex[T](implicit tag: TypeTag[T] , instance_size: Int,
                                            exposeComplex: ExposeRep[Complex[T]]
                                            ): ExposeRep[ComplexVector[T]] = new ExposeRep[ComplexVector[T]](){

        val freshExps: Unit => hlist = (u: Unit) => {
                    val t = for(i <- 0 until instance_size) yield exposeComplex.freshExps()
                    val con: hlist = t.foldLeft(Vector.empty[Exp[_]])((acc,ele) => {
                      acc ++ ele
                    })
                    con

        }
        val hlist2t: hlist => ComplexVector[T] = (h: hlist) => {
          val vc = h.grouped(2)
          val v = vc.foldLeft(Vector.empty[Complex[T]])((acc,ele) => {
            val c = exposeComplex.hlist2t(ele)
            acc :+ c
          })
          ComplexVector(v)
        }
        val t2hlist: ComplexVector[T] => hlist = (v: ComplexVector[T]) => {
          v.vec.foldLeft(Vector.empty[Exp[_]])((acc,ele) => {
            val part = exposeComplex.t2hlist(ele)
            acc ++ part
          })
        }
      }

      def myVfunction[X:Numeric:TypeRep:TypeTag](sample: X, size: Int): Rep[_ => _] = {

        val tmpf: ComplexVector[X] => ComplexVector[X] = (inv: ComplexVector[X]) => {
          val t = inv.vec.map(in =>
            Complex(
              numeric_plus(in.re, in.re),
              numeric_plus(in.im, in.im)
            )
          ).toVector
          ComplexVector(t)
        }
        implicit val instance_size: Int = size
        fun(tmpf) (exposeRepFromVComplex[X],exposeRepFromVComplex[X])
      }


      def simplef[T:Numeric:TypeRep](in: Rep[T]): Rep[T] = {
          numeric_plus(in,in)
      }

      def myfunction[T:Numeric:TypeRep](in: Complex[T]): Complex[T] = {
        Complex(
          numeric_plus(in.re,in.re),
          numeric_plus(in.im, in.im)
        )
      }
    }

    val dsl = new DSL
    import dsl._


    val myf: Complex[Double] => Complex[Double] = dsl.myfunction[Double]
    val simplef: Rep[Double] => Rep[Double] = dsl.simplef[Double]
    val compf = dsl.myVfunction(10.0, 2)


    //val reified = dsl.reifyProgram(myf)
    val reified = dsl.reifyProgram(compf)
    val scheduled = CodeMotion(reified)
    val x = scheduled.block_cache
    println(x)
    println("aha - no idea why this works")

    val graph = new CMGraphExport {
      override val CM: CodeMotion = scheduled
    }
    graph.emitDepGraph (new java.io.PrintWriter(new java.io.FileOutputStream("bla.graph")))

    val traverser = Traversal(scheduled)

    val forward = traverser.getForwardIterator()


    def trav (t: Traverser): Unit = {
      t.scheduleoptions.map(x => println(x._1))
      println("--------")

      if(!t.scheduleoptions.isEmpty)
        trav(t.scheduleoptions.head._2())
    }

    trav(forward)

  }

}
