
package ethz.scheduling

import java.io.PrintWriter

import org.scalacheck._
import Gen._

import scala.reflect.SourceContext
import scala.virtualization.lms.common._



class GenRandomCode extends PrimitiveOpsExp with OrderingOpsExp with VariablesExp
 with WhileExp with RangeOpsExp with ArrayOpsExp with PrintExp with CompileScala { self =>

  val codegen = new ScalaGenPrimitiveOps with ScalaGenOrderingOps
    with ScalaGenVariables with ScalaGenWhile with ScalaGenRangeOps with ScalaGenArrayOps
    with ScalaGenPrint { val IR: self.type = self }

  case class inipos(val arr: Exp[Array[Int]], val pos: Int)

  case class Instructions(
                           val syms: Vector[Exp[Int]],
                           val fsyms: Vector[(Unit => Int)],

                           val arrays: Vector[Exp[Array[Int]]],
                           val farrays: Vector[Unit => Array[Int]],

                           val freshs: Vector[Exp[Int]],

                           val initialized: Set[inipos],
                           val nests: Int,

                           val result: Option[Exp[Int]]
                           )

  case class CodeStyle(
                        val nrinstructions: Int,
                        val nestdepth: Int,
                        val nestperlevel: Int
                        ) {
    def decinstr(): CodeStyle = this.copy(nrinstructions = this.nrinstructions-1)
    def decnest(): CodeStyle = this.copy(nestdepth = this.nestdepth-1)
  }

  def GenFresh(freshsyms: Vector[Exp[Int]], prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = lzy {
    for {
      sofarf <- prev
      nrparams <- Gen.choose(1,5)
    //sym <- Gen.const(fresh[Int])
    } yield {

      val f: Unit => Instructions = (u: Unit) => {
        lazy val instr = {
          val sofar = sofarf()
          val newsyms: Vector[Exp[Int]] = {
            val l = for (i <- 0 until nrparams) yield freshsyms(i)
            l.toVector
          }
          sofar.copy(syms = sofar.syms ++ newsyms, freshs = sofar.syms ++ newsyms)
        }
        instr
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

  def emitSource[T : Manifest, R : Manifest](freshsyms: Vector[codegen.IR.Sym[Int]], f: Unit => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val body = codegen.reifyBlock(f())
    codegen.emitSource(freshsyms.toList, body, className, stream)
  }

  def GenF(prev: Gen[Unit => Instructions]): (Gen[ (Unit => Exp[Int] )]) = {
    for {
      sofarf <- prev
    } yield {
      val f: Unit => Exp[Int] = (u: Unit) => {
        lazy val instr = {
          val sofar = sofarf()
          sofar.result.get
        }
        instr
      }
      //(f,sofar.freshs)
      f
    }
  }


  def GenReduce(prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = {

    val ret = for {
      sofarf <- prev
    } yield {

      val f: Unit => Instructions = (u: Unit) => {
        lazy val instr = {
          val sofar = sofarf()
          val accumulator = array_obj_new[Int](Const(1))
          val newsym = array_update(accumulator, Const(0), Const(0))
          sofar.syms.foreach(
            ele => {
              val sum = array_apply(accumulator, Const(0))
              val newsym = int_plus(sum, ele)
              val res = array_update(accumulator, Const(0), newsym)
            })

          val sum = array_apply(accumulator, Const(0))
          val copy = sofar.copy(result = Some(sum))
          copy
        }
        instr
      }
      f
    }
    ret
  }


  def OneOfFix[T]( gn: Gen[T]*): Gen[T] = {
    val first: Gen[T] = gn(0)
    val sec: Gen[T] = gn(1)
    val rest: Seq[Gen[T]] = gn.tail.tail
    Gen.oneOf(first,sec,rest: _*)

  }


  def bla (codestyle: CodeStyle, sofar: Unit => Instructions) = {
    val seq = Seq(GenArith(sofar),GenNewArr(sofar))
    val seq2 = if (sofar().arrays.isEmpty) seq else seq :+ GenStoreArr(sofar)
    val seq3 = if (sofar().initialized.isEmpty) seq2 else seq2 :+ GenLoadArr(sofar)
    //val seq4 = if (codestyle.nestdepth > 0 && sofar.nests < codestyle.nestperlevel) seq3 :+ GenNestBlock(codestyle.decnest(), sofar) else seq3
    //val seq5 = seq4 :+ GenPrint(sofar)
    seq3
    //Seq(GenArith(sofar),GenArith(sofar))
  }

  def GenRandomInstruction(codestyle: CodeStyle, prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = lzy {
    if (codestyle.nrinstructions > 0) {
      for {
        sofar <- prev
        recurse <- GenRandomInstruction(codestyle.decinstr(),sofar)
        res <- OneOfFix(bla(codestyle,recurse) :_*)
      } yield {
        res
      }

    }
    else
      prev
  }


  def GenArith(prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = {

    val ret = for {
      sofarf <- prev
      lhs <- Gen.oneOf(sofarf().syms)
      rhs <- Gen.oneOf(sofarf().syms)
    } yield {
      val f: Unit => Instructions = (u: Unit) => {
        lazy val instr = {
          val sofar = sofarf()
          val newsym = int_plus(lhs, rhs)
          val copy = sofar.copy(syms = sofar.syms :+ newsym)
          copy
        }
        instr
      }
      f
    }
    ret
   }


  def GenNewArr(prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = lzy {
    for {
      sofarf <- prev
      arrsize <- Gen.choose(1,3)
    } yield {
      val f: Unit => Instructions = (u: Unit) => {
          val sofar = sofarf()
          val newsym = array_obj_new[Int](Const(arrsize)) //GO: maybe also check dynamic sizes?
          val copy = sofar.copy(arrays = sofar.arrays :+ newsym)
          copy
        }
      f
    }
  }




  def GenLoadArr(prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = lzy {
    for {
      sofarf <- prev
      chosenarr <- Gen.oneOf(sofarf().initialized.toList)
    } yield {
      val f: Unit => Instructions = (u: Unit) => {
        lazy val instr = {
          val sofar = sofarf()
          val newsym = array_apply(chosenarr.arr, Const(0)) //GO: access other elements
          val copy = sofar.copy(syms = sofar.syms :+ newsym)
          copy
        }
        instr
      }
      f
    }
  }


  def GenStoreArr(prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = {
    prev.flatMap(
      sofarf => {
          val sofar = sofarf()
          val ret1 = Gen.oneOf(sofar.arrays).flatMap {
            choosenarr => {
              val ret2 = Gen.oneOf(sofar.syms).flatMap {
                sym => {
                  val f: Unit => Instructions = (u: Unit) => {
                    val newsym = array_update(choosenarr, Const(0), sym) //GO: access other elements
                    val ini = inipos(choosenarr, 0)
                    val copy = sofar.copy(initialized = sofar.initialized + ini)
                    copy
                  }
                  f
                }
              }
              ret2
              }
          }
          ret1
      }
    )
  }

/*
  def GenStoreArr1(prev: Gen[Unit => Instructions]): Gen[Unit => Instructions] = lzy {
    for {
      sofarf <- prev
      //chosenarr <- Gen.oneOf(sofarf().arrays)
      //sym <- Gen.oneOf(sofarf().syms)
    } yield {
      val f: Unit => Instructions = (u: Unit) => {
        val sofar = sofarf()
         val random = for {
           chosenarr <- Gen.oneOf(sofar.arrays)
           sym <- Gen.oneOf(sofar.syms)
         } yield {
           val newsym = array_update(chosenarr, Const(0), sym) //GO: access other elements
           val ini = inipos(chosenarr, 0)
           val copy = sofar.copy(initialized = sofar.initialized + ini)
           copy
         }
        random
      }
      f
    }
  }
*/



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
      g <- Gen.const(Instructions(Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Set.empty, 0, None))
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
    val style = dsl.CodeStyle(50,2,4)
    val empty = dsl.GenEmpty()

    val freshsyms: Vector[dsl.codegen.IR.Sym[Int]] = (for (i <- 0 until 5) yield dsl.codegen.IR.fresh[Int]).toVector
    val inigen = dsl.GenFresh(freshsyms,empty)
    //val code = dsl.GenRandomInstruction(style,inigen)
    val code1 = dsl.GenNewArr(inigen)


    val code = dsl.GenStoreArr(code1)
    //val onearith = dsl.GenArith(inigen)
    val reduce = dsl.GenReduce(code)
    val genf = dsl.GenF(reduce)


    genf.flatMap(x => x)
    val sample = genf.sample

//    dsl.emitSource(freshsyms,sample,"Test", new PrintWriter(System.out))
    //val newint = dsl.GenRandomInstruction(style,inigen)
    //dsl.codegen.emitSource(bla,"Test", new PrintWriter(System.out))
    //  println(dsl.globalDefs)
  }
}



