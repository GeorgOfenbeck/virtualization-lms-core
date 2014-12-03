import java.io.PrintWriter

import _root_.util.CMGraphExport
import org.scalatest.FunSpec
import scala.virtualization.lms._
import common._
import scala.virtualization.lms.internal.CodeMotion

class TestNewF extends FunSpec{


  describe("trying the new style functions") {





    class DSL extends NumericOpsExp with PureFunctionsExp with ReifyPure{
      self =>
      val IR: self.type = self




      case class Complex[T](val re: Rep[T], val im: Rep[T])

      import scala.reflect.runtime.universe._
      import shapeless._


      private def helper1[T](u : Unit)(implicit tag: TypeTag[T]): ::[Exp[T],::[Exp[T], HNil]] =fresh[T](tag) :: fresh[T](tag) :: HNil
      private def helper2[T](x : ::[Exp[T],::[Exp[T], HNil]]): Complex[T] = Complex(x.head,x.tail.head)
      private def helper3[T](x: Complex[T]): ::[Exp[T],::[Exp[T], HNil]] = x.re :: x.im :: HNil
      private def helper4[T](x: ::[Exp[T],::[Exp[T], HNil]]): Vector[Exp[_]] = Vector(x.head,x.tail.head)

      implicit def exposeRepFromComplex[T](implicit tag: TypeTag[T]): ExposeRep[Complex[T]] = new ExposeRep[Complex[T]](){
        type hlist = ::[Exp[T],::[Exp[T], HNil]]
        val freshExps: Unit => hlist = helper1
        val hlist2t: hlist => Complex[T] = helper2
        val t2hlist: Complex[T] => hlist = helper3
        val hlist2Exps: hlist => Vector[Exp[_]] = helper4
      }


      case class ComplexVector[T](val vec: Vector[Complex[T]])

      /*private def vhelper1[T](u : Unit)(implicit tag: TypeTag[T]): ::[Exp[T],::[Exp[T], HNil]] =fresh[T](tag) :: fresh[T](tag) :: HNil
      private def vhelper2[T](x : ::[Exp[T],::[Exp[T], HNil]]): Complex[T] = Complex(x.head,x.tail.head)
      private def vhelper3[T](x: Complex[T]): ::[Exp[T],::[Exp[T], HNil]] = x.re :: x.im :: HNil
      private def vhelper4[T](x: ::[Exp[T],::[Exp[T], HNil]]): Vector[Exp[_]] = Vector(x.head,x.tail.head)

      implicit def exposeRepFromComplex[T](implicit tag: TypeTag[T]): ExposeRep[Vector[Complex[T]]] = new ExposeRep[Vector[Complex[T]]](){
        type hlist = HList
        val freshExps: Unit => hlist =
        val hlist2t: hlist => Complex[T] = helper2
        val t2hlist: Complex[T] => hlist = helper3
        val hlist2Exps: hlist => Vector[Exp[_]] = helper4
      }*/



      def myVfunction[T:Numeric:TypeRep](inv: ComplexVector[T]): Def[_ => _] = {

      implicit def exposeRepFromVComplex[T](implicit tag: TypeTag[T], instance: Vector[Complex[T]], exposeComplex: ExposeRep[Complex[T]]): ExposeRep[Vector[Complex[T]]] = new ExposeRep[Vector[Complex[T]]](){
        type hlist = HList
        val freshExps: Unit => hlist = (u: Unit) => {
          val t = instance.map( x => exposeComplex.freshExps)
          val ini: HList = HNil
          val con: HList = t.foldLeft(ini)((acc,ele) => {
            ele :: acc
          })
          con
        }
        val hlist2t: hlist => Vector[Complex[T]] = (h: hlist) => {
          ???
        }
        val t2hlist: Vector[Complex[T]] => hlist = {
          ???
        }
        val hlist2Exps: hlist => Vector[Exp[_]] = (h: hlist) => {
          val t = h.head


        }
      }


        val t = inv.vec.map( in =>
          Complex(
            numeric_plus(in.re,in.re),
            numeric_plus(in.im, in.im)
          )
        ).toVector
        ComplexVector(t)
        ???
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
    val reified = dsl.reifyProgram(myf)
    val scheduled = CodeMotion(reified)
    val x = scheduled.block_cache
    println(x)
    println("aha - no idea why this works")

    val graph = new CMGraphExport {
      override val CM: CodeMotion = scheduled
    }
    graph.emitDepGraph (new java.io.PrintWriter(new java.io.FileOutputStream("bla.graph")))



  }

}
