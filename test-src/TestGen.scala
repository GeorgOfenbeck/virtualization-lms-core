import org.scalatest.FunSpec

/**
 * Georg Ofenbeck
 First created:
 * Date: 13/08/2014
 * Time: 15:25 
 */
class TestGen  extends FunSpec {


  describe("checking it out") {

    trait Expression {

      abstract class Exp[+T: Manifest] {
        // constants/symbols (atomic)

      }

      abstract class Def[+T] {
        // operations (composite)
        override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
      }

      case class Sym[+T: Manifest](val id: Int) extends Exp[T] {
      }

      abstract class Stm

      // statement (links syms and definitions)
      case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm

      protected implicit def toAtom[T: Manifest](d: Def[T]): Exp[T] = ???


      var globalDefs: List[Def[Any]] = Nil
    }

    trait PrimitivDSL extends Expression {

      //case class DoublePlus(lhs: Exp[Double], rhs: Exp[Double]) extends Def[Double]
      //def double_plus(lhs: Exp[Double], rhs: Exp[Double]): Exp[Double] = DoublePlus(lhs,rhs)

      case class DoublePlus() extends Def[Double]

      def double_plus(): Exp[Double] = DoublePlus()
    }

    trait GenPrimitivDSL extends GenBase {
      val IR: PrimitivDSL

      import IR._

      override def emit(in: Def[Any]) = {
        println("child emit")
        in match {
          case DoublePlus() => println("seems to work")
          case _ => super.emit(in)
        }
      }
    }



    trait GenBase {
      val IR: Expression

      import IR._

      def emit(in: Def[Any]) = {
        println("base emit")
        in match {
          case _ => assert(false, "no translate")
        }
      }




    }

    trait Traverser {
      val codegen: GenBase
      val mydefs: List[codegen.IR.Def[Any]]

      def traverse() = {
        mydefs map (stm => codegen.emit(stm))
      }
    }

    object Traversal {
      def getTraverser(dsl: Expression with GenBase): Traverser = {
        val x = new Traverser {
          override val codegen: dsl.type = dsl
          override val mydefs = dsl.IR.globalDefs
        }
        x
      }

    }

    class Foo extends PrimitivDSL with GenPrimitivDSL {
      self =>
      val IR: self.type = self
    }



    val dsl = new Foo
    dsl.globalDefs = List(dsl.DoublePlus())
    val trav = Traversal.getTraverser(dsl)


    trav.traverse()


  }
}
