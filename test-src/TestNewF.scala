/*

import java.io.PrintWriter


import org.scalatest.FunSpec
import util.CMGraphExport
import scala.virtualization.lms.internal._
import scala.virtualization.lms.common._



class TestNewF extends FunSpec{


  describe("trying the new style functions") {

    /*trait SimpleNumericOps extends ImplicitOps with TypeRepBase{
      class NumericOpsCls(lhs: Rep[Double])(implicit val reptag: TypeRep[Double]){
        def +(rhs: Rep[Double]) = numeric_plus(lhs,rhs)
        def -(rhs: Rep[Double]) = numeric_minus(lhs,rhs)
        def *(rhs: Rep[Double]) = numeric_times(lhs,rhs)
        def /(rhs: Rep[Double]) = numeric_divide(lhs,rhs)
      }
      def numeric_plus(lhs: Rep[Double], rhs: Rep[Double])(implicit reptag: TypeRep[Double]): Rep[Double]
      def numeric_minus(lhs: Rep[Double], rhs: Rep[Double])(implicit reptag: TypeRep[Double]): Rep[Double]
      def numeric_times(lhs: Rep[Double], rhs: Rep[Double])(implicit reptag: TypeRep[Double]): Rep[Double]
      def numeric_divide(lhs: Rep[Double], rhs: Rep[Double])(implicit reptag: TypeRep[Double]): Rep[Double]
    }
    trait SimpleNumericOpsExp extends SimpleNumericOps with ImplicitOpsExp with BaseExp with TypeRepBase  {
      case class NumericPlus(lhs: Exp[Double], rhs: Exp[Double])(implicit val reptag: TypeRep[Double]) extends Def[Double]
      case class NumericMinus(lhs: Exp[Double], rhs: Exp[Double])(implicit val reptag: TypeRep[Double]) extends Def[Double]
      case class NumericTimes(lhs: Exp[Double], rhs: Exp[Double])(implicit val reptag: TypeRep[Double]) extends Def[Double]
      case class NumericDivide(lhs: Exp[Double], rhs: Exp[Double])(implicit val reptag: TypeRep[Double]) extends Def[Double]
      def numeric_plus(lhs: Exp[Double], rhs: Exp[Double])(implicit reptag: TypeRep[Double]) : Exp[Double] = NumericPlus(lhs, rhs)
      def numeric_minus(lhs: Exp[Double], rhs: Exp[Double])(implicit reptag: TypeRep[Double]) : Exp[Double] = NumericMinus(lhs, rhs)
      def numeric_times(lhs: Exp[Double], rhs: Exp[Double])(implicit reptag: TypeRep[Double]) : Exp[Double] = NumericTimes(lhs, rhs)
      def numeric_divide(lhs: Exp[Double], rhs: Exp[Double])(implicit reptag: TypeRep[Double]) : Exp[Double] = NumericDivide(lhs, rhs)
    }*/


    class DSL extends NumericOpsExp with PureFunctionsExp with ReifyPure with TransformingPure{
      self =>
      val IR: self.type = self


      import scala.reflect.runtime.universe._

      case class Complex[T](val re: Rep[T], val im: Rep[T])



      implicit def exposeRepFromComplex[T](implicit tag: TypeTag[T]): ExposeRep[Complex[T]] = new ExposeRep[Complex[T]](){
        val freshExps: Unit => hlist = (u: Unit) => Vector(Arg[T](tag), Arg[T](tag))
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

    trait MyTransformer extends ForwardTransformer {
      val IR: DSL
      import IR._
      override def transformStm(stm: TP[_]): Exp[_] = stm match {
        case TP(s,NumericPlus(a,b),tag) =>
          println("replacing " + stm)
          val ta: Exp[_] = a
          val tb: Exp[_] = b

          val na: Exp[Double] = apply(ta).asInstanceOf[Exp[Double]]
          val nb: Exp[Double] = apply(tb).asInstanceOf[Exp[Double]]

          numeric_minus(na,nb)
        case _ => super.transformStm(stm)
      }
    }


    import dsl._


    val myf: Complex[Double] => Complex[Double] = dsl.myfunction[Double]
    val simplef: Rep[Double] => Rep[Double] = dsl.simplef[Double]
    //val compf = dsl.myVfunction(10.0, 2)


    val reified = dsl.reifyProgram(myf)
    //val reified = dsl.reifyProgram(compf)
    val scheduled = CodeMotion(reified)

    //do transform here




    val trans = new MyTransformer {
      val IR: dsl.type = dsl
    }



    //


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
      val id2tp = t.cminfo.reifiedIR.id2tp

      t.scheduleoptions.map(x => {
        val tp = id2tp(x._1)

        println(s"${x._1} -> Type: ${tp.tag.tag.toString()}")
        //println(x._1)
      })

      println("--------")

      if(!t.scheduleoptions.isEmpty) {
        val blocks = traverser.cminfo.reifiedIR.IR.blocks(id2tp(t.scheduleoptions.head._1))
        if (!blocks.isEmpty)
        {
          blocks map (block => {
            val btrav: Traverser = traverser.getForwardIterator(block)
            trav(btrav)
          })
        }
        trav(t.scheduleoptions.head._2())
      }
    }

    trav(forward)

  }

}*/

