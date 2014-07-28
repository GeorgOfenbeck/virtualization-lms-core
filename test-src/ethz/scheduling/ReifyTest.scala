package ethz.scheduling

/**
 * Georg Ofenbeck
 First created:
 * Date: 24/07/2014
 * Time: 15:41 
 */
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


class CheckDSL extends PrimitiveOpsExp with OrderingOpsExp with VariablesExp
with IfThenElseExp with WhileExp with RangeOpsExp with ArrayOpsExp with PrintExp with CompileScala {
  self =>

  val codegen = new ScalaGenPrimitiveOps with ScalaGenOrderingOps
    with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenRangeOps with ScalaGenArrayOps
    with ScalaGenPrint {
    val IR: self.type = self
  }
}


import org.scalatest.FunSpec

class ReifyTest extends FunSpec{
  describe("check reify") {
    val dsl = new CheckDSL()
    import dsl._
    val bla = (unit: dsl.Rep[Int]) => {
      val range: Rep[Range] = range_until(Const(0), Const(1))
      for (i <- range){
        for (j <- range) {
          dsl.print(Const("inner"))
          dsl.print(j)
        }
        dsl.print(Const("outer"))
        dsl.print(i)
      }
      dsl.print(unit)
    }
    dsl.codegen.emitSource(bla,"Test", new PrintWriter(System.out))
    /*
    import dsl._
    def rblock(in: dsl.Exp[Int], otherstuff: Exp[Int]): dsl.Block[Int] = {
      val f: (Exp[Int] => Exp[Int]) = (in: Exp[Int]) => {
        otherstuff
        print(in)
        in
      }
      dsl.codegen.reifyBlock(f(in))
    }

    val bla = (unit: dsl.Rep[Int]) => {
      newint.sample.get
      dsl.Const(1)
    }
    dsl.codegen.emitSource(bla,"Test", new PrintWriter(System.out)) */
    //  println(dsl.globalDefs)
  }
}
