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
                           val initialized: Set[inipos]
                           )


  def GenRandomInstruction(n: Int, prev: Gen[Instructions]): Gen[Instructions] = lzy {
    if (n > 0)
    for {
      sofar <- GenRandomInstruction(n-1,prev)
      arith <- GenArith(sofar)
      newarr <- GenNewArr(sofar)
      store <- GenStoreArr(sofar)
      //store <- if (sofar.arrays.isEmpty) List() else List(GenStoreArr(sofar))
      //load <- if (sofar.arrays.isEmpty) List() else List(GenLoadArr(sofar))
/*      choice <- if (sofar.arrays.isEmpty)
                  Gen.oneOf(arith,newarr)//,store,load))
                else
                  Gen.oneOf(arith,newarr)        */
      choice <- Gen.oneOf(arith,newarr)

      //choice <- Gen.oneOf(List(arith,newarr) ::: store ::: load)
    } yield {
      if (sofar.syms.isEmpty && sofar.arrays.isEmpty) {
        assert(false, "we dont have any inital seed for our random instructions")
        ???
      }
      else {
        choice
      }
    }
    else {
      prev
    }
  }

  def GenNewArr(sofar: Instructions): Gen[Instructions] = {
    for {
      arrsize <- Gen.choose(1,3)
    } yield {
      val newsym = array_obj_new[Int](Const(arrsize)) //GO: maybe also check dynamic sizes?
      sofar.copy(arrays = sofar.arrays :+ newsym)
    }
  }

  def GenLoadArr(sofar: Instructions): Gen[Instructions] = {
    for {
      chosenarr <- Gen.oneOf(sofar.initialized.toList)
    } yield {
      val newsym = array_apply(chosenarr.arr,Const(0)) //GO: access other elements
      sofar.copy(syms = sofar.syms :+ newsym)
    }
  }

  def GenStoreArr(sofar: Instructions): Gen[Instructions] = {
    for {
      chosenarr <- Gen.oneOf(sofar.arrays)
      sym <- Gen.oneOf(sofar.syms)
    } yield {
      val newsym = array_update(chosenarr,Const(0),sym) //GO: access other elements
      val ini = inipos(chosenarr,0)
      sofar.copy(initialized = sofar.initialized + ini)
    }

  }



  def GenArith(sofar: Instructions): Gen[Instructions] = {
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

  def GenFresh(prev: Gen[Instructions]): Gen[Instructions] = {
    for {
      sofar <- prev
      sym <- Gen.const(fresh[Int])
    } yield sofar.copy(syms = sofar.syms :+ sym)
  }

  def GenEmpty(): Gen[Instructions] = {
    Gen.const(Instructions(Vector.empty, Vector.empty, Set.empty))
  }
}


  import org.scalatest.FunSpec

  class GenTest extends FunSpec{

    describe("check gen") {
      val dsl = new GenGraph()
      val empty = dsl.GenEmpty()
      val inigen = dsl.GenFresh(empty)
      val newint = dsl.GenRandomInstruction(20,inigen)
      val bla = (unit: dsl.Rep[Int]) => {
        newint.sample.get
        dsl.Const(1)
      }
      dsl.codegen.emitSource(bla,"Test", new PrintWriter(System.out))
      println(dsl.globalDefs)
    }
  }

/*
  def GenRandomInstructions() : Gen[List[Exp[Any]]] = {
    val x : List[Exp[Any]] = List(Const(1))
    val bla = for {
      ini <- Gen.const(x)
    }  yield ini
    bla
  }


  def GenRandomInt(n: Int) : Gen[List[Exp[Int]]] = {
    val ini = Gen.const(List(Const(1)))
    GenRandomInt(n-1,ini)
  }




  def GenRandomInt(n: Int, sofar : Gen[List[Exp[Int]]]) : Gen[List[Exp[Int]]] = {
    if (n == 0)
      sofar
    else {
      val res = for {
        const <- Gen.const(Const(1))
        prev <- sofar
        newint <- if (!prev.isEmpty) {
          val whatisit = GenArith(sofar)
          val hm = Gen.oneOf(Gen.const(Const(1)), whatisit)
          hm
        }
        else
          Gen.const(Const(1))
      } yield newint :: prev
      GenRandomInt(n-1,res)
    }
  }

  def GenNewArray(): Gen[Exp[Any]] = {
    val x =  for {
      s <- Gen.choose(1, 2)
      i <- Gen.const(array_obj_new[Int](Const(s)))
    } yield i
    x
  }

  def GenArith(sofar: Gen[List[Exp[Int]]]): Gen[Exp[Int]] = {
    val x = for {
      prev <- sofar
      prevsize <- prev.size
      n1 <- Gen.choose(0,prevsize-1)
      n2 <- Gen.choose(0,prevsize-1)
    } yield{
      val lhs = prev(n1)
      val rhs = prev(n2)
      val newint = int_plus(lhs,rhs)
      newint
    }
    x
  }



/*
  def GenRec(full: Int, remain: Int, sofar: Gen[List[Exp[Any]]]): Gen[List[Exp[Any]]] = {
    if (remain > 0)
    {
      val outer_res =   for {
        num <- Gen.choose(1,2)
        pos <- Gen.choose(0,full-remain)
        prev <- sofar
      } yield {
        val res= if (num == 1)
        {
          num :: prev
        }
        else
        {
          (prev(pos) * 2) :: prev
        }
        res
      }
      GenRec(full,remain-1,outer_res)
    }
    else
    {
      sofar
    }
  }
*/
  */


