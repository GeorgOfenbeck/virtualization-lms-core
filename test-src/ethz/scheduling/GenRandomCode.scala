/*
package ethz.scheduling

import java.io.PrintWriter

import org.scalacheck._
import Gen._

import scala.reflect.SourceContext
import scala.virtualization.lms.common._



class GenRandomCode extends PrimitiveOpsExp with OrderingOpsExp with VariablesExp
with IfThenElseExp with WhileExp with RangeOpsExp with ArrayOpsExp with PrintExp with CompileScala { self =>

  val codegen = new ScalaGenPrimitiveOps with ScalaGenOrderingOps
    with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenRangeOps with ScalaGenArrayOps
    with ScalaGenPrint { val IR: self.type = self }

  case class inipos(val arr: Exp[Array[Int]], val pos: Int)

  case class Instructions(
                           val syms: Vector[Exp[Int]],
                           val fsyms: Vector[(Unit => Int)],

                           val arrays: Vector[Exp[Array[Int]]],
                           val farrays: Vector[Unit => Array[Int]],

                           val initialized: Set[inipos],
                           val nests: Int
                           )

  case class CodeStyle(
                        val nrinstructions: Int,
                        val nestdepth: Int,
                        val nestperlevel: Int
                        ) {
    def decinstr(): CodeStyle = this.copy(nrinstructions = this.nrinstructions-1)
    def decnest(): CodeStyle = this.copy(nestdepth = this.nestdepth-1)
  }

  def GenFresh(prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = lzy {
    for {
      sofarf <- prev
      nrparams <- Gen.choose(1,3)
    //sym <- Gen.const(fresh[Int])
    } yield {
      val sofar = sofarf()
      val f: Unit => Instructions = (u: Unit) => {
        val newsyms: Vector[Exp[Int]] = {
          val l = for (i <- 0 until nrparams) yield fresh[Int]
          l.toVector
        }
        sofar.copy(syms = sofar.syms ++ newsyms)
      }
      f
    }
  }

  /*
  def GenRandomInstruction(codestyle: CodeStyle, root: Gen[Exp[Int] => Instructions]): Gen[Exp[Int] => Instructions] = lzy {
    if (codestyle.nrinstructions > 0) {
      val bla1 = GenRandomInstruction(codestyle.decinstr(), root).flatMap(
        sofar => GenArith(sofar)
      )
      bla1
    }
    else {
      root
    }
  } */

  def GenArith(prev: Gen[Exp[Int] => Instructions]): Gen[Exp[Int] => Instructions] = {

    val f: Exp[Int] => Instructions = (in: Exp[Int]) => {
      val ret = for {
        sofarf <- prev
      } yield {
        val instr = sofarf(in)
        instr
      }
      re
    }
    f

    /*for {
      sofar <- prev
    //const <- Gen.const(Const(1))
      const <- Gen.oneOf(sofar.syms)
      sym <- Gen.oneOf(sofar.syms)
      lhs <- Gen.oneOf(const, sym)
      rhs <- Gen.oneOf(const, sym)
    } yield {
      val f: Exp[Int] => Instructions = (u: Unit) => {
        val newsym = int_plus(lhs, rhs)
        sofar.copy(syms = sofar.syms :+ newsym)
      }
      f
    }*/
  }





  def GenBla(style: CodeStyle): Gen[Rep[Int] => Rep[Int] ] = {
    for {
      block <- GenBlock(style)

      //withinst <- GenRandomInstruction(style,block)
    } yield {
      val f: (Rep[Int] => Rep[Int]) = (input: Rep[Int]) => {
        val x = withinst(input)
        //val x = block(input)
        x.syms.last
      }
      f
    }

  }

  def GenBlock(style: CodeStyle): Gen[Rep[Int] => Instructions] = {
    for {
      empty <- GenEmpty()
      first <- GenFirst(style,empty)
    } yield {
      val f: (Rep[Int] => Instructions) = (input: Rep[Int]) => {
        val x = first(input)
        x
      }
      f
    }
  }


  def GenEmpty(): Gen[Unit => Instructions] = {
    for {
      g <- Gen.const(Instructions(Vector.empty, Vector.empty, Vector.empty, Vector.empty, Set.empty, 0))
    } yield {
      val f: Unit => Instructions = (u: Unit) => g
      f
    }
  }



  def GenFirst(codestyle: CodeStyle, prev: Gen[Unit => Instructions]): Gen[Exp[Int] => Instructions] = {
    for {
      sofarf <- prev

    } yield {
      val f: (Exp[Int] => Instructions) = (in: Exp[Int]) =>
      {
        val sofar = sofarf()
        sofar.copy(syms = sofar.syms :+ in)
      }
      f
    }
  }


}


import org.scalatest.FunSpec

class GenBla extends FunSpec{

  describe("check gen") {
    println("starting")
    val dsl = new GenRandomCode()
    val style = dsl.CodeStyle(20,2,4)
    val empty = dsl.GenEmpty()
    val inigen = dsl.GenFresh(empty)
    val newint = dsl.GenRandomInstruction(style,inigen)
    val bla = dsl.GenBlock(style).sample.get
    dsl.codegen.emitSource(bla,"Test", new PrintWriter(System.out))
    //  println(dsl.globalDefs)
  }
}


*/
