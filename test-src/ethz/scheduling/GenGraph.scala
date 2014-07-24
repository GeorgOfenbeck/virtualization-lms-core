package ethz.scheduling

import java.io.PrintWriter

import org.scalacheck._
import Gen._

import scala.reflect.SourceContext
import scala.virtualization.lms.common._


trait Print extends Base {
  implicit def unit(s: String): Rep[String]
  def print(s: Rep[Any]): Rep[Unit]
}

trait PrintExp extends Print with EffectExp {
  implicit def unit(s: String): Rep[String] = Const(s)
  case class Print(s: Rep[Any]) extends Def[Unit]
  def print(s: Rep[Any]) = reflectEffect(Print(s))
  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case Print(s) => Print(f(s))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
      case Reflect(Print(s), u, es) => reflectMirrored(Reflect(Print(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenPrint extends ScalaGenEffect {
  val IR: PrintExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Print(s) =>  emitValDef(sym, "println(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


class GenGraph extends PrimitiveOpsExp with OrderingOpsExp with VariablesExp
with IfThenElseExp with WhileExp with RangeOpsExp with ArrayOpsExp with PrintExp with CompileScala { self =>

  val codegen = new ScalaGenPrimitiveOps with ScalaGenOrderingOps
    with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenRangeOps with ScalaGenArrayOps
    with ScalaGenPrint { val IR: self.type = self }

  case class inipos(val arr: Exp[Array[Int]], val pos: Int)

  case class Instructions(
                           val syms: Vector[Exp[Int]],
                           val arrays: Vector[Exp[Array[Int]]],
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


  def OneOfFix[T]( gn: Gen[T]*): Gen[T] = {
    val first: Gen[T] = gn(0)
    val sec: Gen[T] = gn(1)
    val rest: Seq[Gen[T]] = gn.tail.tail
    Gen.oneOf(first,sec,rest: _*)

  }

  def GenRandomInstruction(codestyle: CodeStyle, prev: Gen[Instructions]): Gen[Instructions] = lzy {
    if (codestyle.nrinstructions > 0) {
      val bla1 = GenRandomInstruction(codestyle.decinstr(), prev).flatMap(
        sofar => {
          val seq = Seq(GenArith(sofar),GenNewArr(sofar))
          val seq2 = if (sofar.arrays.isEmpty) seq else seq :+ GenStoreArr(sofar)
          val seq3 = if (sofar.initialized.isEmpty) seq2 else seq2 :+ GenLoadArr(sofar)
          val seq4 = if (codestyle.nestdepth > 0 && sofar.nests < codestyle.nestperlevel) seq3 :+ GenNestBlock(codestyle.decnest(), sofar) else seq3
          val seq5 = seq4 :+ GenPrint(sofar)
          OneOfFix(seq5: _*)

          //In the current version of scalacheck a call with a container would call the wrong overload
          /*/** Picks a random value from a list */
            def oneOf[T](xs: Seq[T]): Gen[T] =

            instead of:

             /** Picks a random generator from a list */
            def oneOf[T](g0: Gen[T], g1: Gen[T], gn: Gen[T]*): Gen[T] = {
            */
        }
      )
    }
    else {
      prev
    }
  }

  def GenPrint(sofar: Instructions): Gen[Instructions] = lzy {
      this.print("bla")
      sofar
  }


  def GenNewArr(sofar: Instructions): Gen[Instructions] = lzy {
    for {
      arrsize <- Gen.choose(1,3)
    } yield {
      val newsym = array_obj_new[Int](Const(arrsize)) //GO: maybe also check dynamic sizes?
      sofar.copy(arrays = sofar.arrays :+ newsym)
    }
  }

  def GenLoadArr(sofar: Instructions): Gen[Instructions] = lzy {
    for {
      chosenarr <- Gen.oneOf(sofar.initialized.toList)
    } yield {
      val newsym = array_apply(chosenarr.arr,Const(0)) //GO: access other elements
      sofar.copy(syms = sofar.syms :+ newsym)
    }
  }

  def GenStoreArr(sofar: Instructions): Gen[Instructions] = lzy {
    for {
      chosenarr <- Gen.oneOf(sofar.arrays)
      sym <- Gen.oneOf(sofar.syms)
    } yield {
      val newsym = array_update(chosenarr,Const(0),sym) //GO: access other elements
      val ini = inipos(chosenarr,0)
      sofar.copy(initialized = sofar.initialized + ini)
    }
  }


  def GenNestBlock(style: CodeStyle, sofar: Instructions): Gen[Instructions] = lzy {
    val range: Rep[Range] = range_until(Const(0), Const(1))
    val block: Rep[Int] => Rep[Unit] = (i: Rep[Int])=> {
      val withiter = sofar.copy(syms = sofar.syms :+ i)
      val nest = GenRandomInstruction(style.decnest(), withiter).sample
      val unit: Rep[Unit] = null //this is annoying
      unit
    }
    range_foreach(range,block)
    sofar.copy(nests = sofar.nests + 1)
  }


  def GenArith(sofar: Instructions): Gen[Instructions] = lzy {
    for {
      //const <- Gen.const(Const(1))
      const <- Gen.oneOf(sofar.syms)
      sym <- Gen.oneOf(sofar.syms)
      lhs <- Gen.oneOf(const, sym)
      rhs <- Gen.oneOf(const, sym)
    } yield {
      val newsym = int_plus(lhs, rhs)
      sofar.copy(syms = sofar.syms :+ newsym)
    }
  }

  def GenFresh(prev: Gen[Instructions]): Gen[Instructions] = lzy {
    for {
      sofar <- prev
      sym <- Gen.const(fresh[Int])
    } yield sofar.copy(syms = sofar.syms :+ sym)
  }

  def GenEmpty(): Gen[Instructions] = {
    Gen.const(Instructions(Vector.empty, Vector.empty, Set.empty,0))
  }
}


  import org.scalatest.FunSpec

  class GenTest extends FunSpec{

    describe("check gen") {
      println("starting")
      val dsl = new GenGraph()
      val style = dsl.CodeStyle(20,2,4)
      val empty = dsl.GenEmpty()
      val inigen = dsl.GenFresh(empty)
      val newint = dsl.GenRandomInstruction(style,inigen)
      val bla = (unit: dsl.Rep[Int]) => {
        newint.sample.get
        dsl.Const(1)
      }
      dsl.codegen.emitSource(bla,"Test", new PrintWriter(System.out))
    //  println(dsl.globalDefs)
    }
  }


