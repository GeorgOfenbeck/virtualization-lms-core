package ethz.scheduling



import java.io.PrintWriter

import org.scalacheck._
import Gen._

import scala.reflect.SourceContext
import scala.virtualization.lms.common._



class GenRCode extends PrimitiveOpsExp with OrderingOpsExp with VariablesExp
with WhileExp with RangeOpsExp with ArrayOpsExp with PrintExp with CompileScala {
  self =>

  val codegen = new ScalaGenPrimitiveOps with ScalaGenOrderingOps
    with ScalaGenVariables with ScalaGenWhile with ScalaGenRangeOps with ScalaGenArrayOps
    with ScalaGenPrint {
    val IR: self.type = self
  }


  var instructions: Option[Instructions] = None

  var randomf: Option[Exp[Int] => Exp[Int]] = None

  case class inipos(val arr: Exp[Array[Int]], val pos: Int)

  case class Instructions(
                           val syms: Vector[Exp[Int]],
                           val fsyms: Vector[(Unit => Int)],

                           val arrays: Vector[Exp[Array[Int]]],
                           val farrays: Vector[Unit => Array[Int]],

                           val freshs: Vector[Sym[Int]],

                           val initialized: Set[inipos],
                           val nests: Int,

                           val result: Option[Exp[Int]]
                           )

  case class CodeStyle(
                        val nrinstructions: Int,
                        val nestdepth: Int,
                        val nestperlevel: Int
                        ) {
    def decinstr(): CodeStyle = this.copy(nrinstructions = this.nrinstructions - 1)

    def decnest(): CodeStyle = this.copy(nestdepth = this.nestdepth - 1)
  }


  def emitSource[T: Manifest, R: Manifest](freshsyms: Vector[codegen.IR.Sym[Int]], f: Unit => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val body = codegen.reifyBlock(f())
    codegen.emitSource(freshsyms.toList, body, className, stream)
  }

  def GenEmpty(): Gen[Instructions] = {
    for {
      g <- Gen.const(Instructions(Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Set.empty, 0, None))
    } yield {
      g
    }
  }

  def GenFresh(freshsyms: Vector[Sym[Int]], prev: Gen[Instructions]): Gen[Instructions] = lzy {
    for {
      sofar <- prev
      nrparams <- Gen.choose(1, 5)
    //sym <- Gen.const(fresh[Int])
    } yield {
      val newsyms: Vector[Sym[Int]] = {
        val l = for (i <- 0 until nrparams) yield freshsyms(i)
        l.toVector
      }
      sofar.copy(syms = sofar.syms ++ newsyms, freshs = sofar.freshs ++ newsyms)

    }
  }

  def GenReduce(prev: Gen[Instructions]): Gen[Instructions] = {

    for {
      sofar <- prev
    } yield {
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
  }


  def GenF(prev: Gen[Instructions]): (Gen[Exp[Int]]) = {
    for {
      sofar <- prev
    } yield  sofar.result.get

  }


  def GenFan(): Gen[Exp[Int] => Vector[Exp[Int]]] = {
    for {
      number <- Gen.choose(1,100)
    } yield {
      val f: Exp[Int] => Vector[Exp[Int]] = (in: Exp[Int]) => {
        (for (i <- 0 until number) yield int_plus(in,Const(number))).toVector
      }
      f
    }
  }

  def GenRandomStore(): Gen[ Instructions => Instructions] = lzy {
    for {
      choice <- Gen.choose(1,1000)
      symchoice <- Gen.choose(1,1000)
    } yield{
      val f: (Instructions => Instructions) = (instr: Instructions) => {
        if (instr.arrays.size > 0) {
          val choiceidx = choice % instr.arrays.size
          val symchoiceidx = symchoice % instr.syms.size
          val chosenarray = instr.arrays(choiceidx)
          array_update(chosenarray, Const(0), instr.syms(symchoiceidx))
          val ini = inipos(chosenarray, 0)
          val newinst = instr.copy(initialized = instr.initialized + ini)
          newinst
        }
        else
          instr
      }
      f
    }
  }

  def GenRandomLoad(): Gen[ Instructions => Instructions] = lzy {
    for {
      choice <- Gen.choose(1,1000)
    } yield{
      val f: (Instructions => Instructions) = (instr: Instructions) => {
        if (instr.initialized.size > 0) {
          val choiceidx = choice % instr.initialized.size
          val pos = instr.initialized.toVector(choiceidx)

          val chosenarray = pos.arr
          val newsym = array_apply(pos.arr,Const(pos.pos))
          val newinst = instr.copy(syms = instr.syms :+ newsym)
          newinst
        }
        else
          instr
      }
      f
    }
  }


  def GenRandomNewArrays(): Gen[Instructions => Instructions] = lzy {
    for {
      arrsize <- Gen.choose(1,2)
    } yield {
      val f: (Instructions => Instructions) = (instr: Instructions) => {
        val newarr = array_obj_new[Int](Const(arrsize))
        val newinst = instr.copy(arrays = instr.arrays :+ newarr)
        newinst
      }
      f
    }
  }

  def GenRandomAdds(): Gen[Instructions => Instructions] = lzy {
    for {
      lhs <- Gen.choose(1, 1000)
      rhs <- Gen.choose(1, 1000)
    } yield {
      val f: (Instructions => Instructions) = (instr: Instructions) => {
        val in = instr.syms
        val lhsidx: Int = lhs % in.size
        val rhsidx: Int = rhs % in.size
        val newele = int_minus(in(lhsidx), in(rhsidx))
        val newinst = instr.copy(syms = instr.syms :+ newele)
        newinst
      }
      f
    }
  }

  def GenPrint(): Gen[Instructions => Instructions] = lzy {
    for {
      number <- Gen.choose(1, 1000)
    } yield {
      val f: (Instructions => Instructions) = (instr: Instructions) => {
        this.print(Const(number))
        instr
      }
      f
    }
  }


  def GenRandomInstr(nr: Int): Gen[Instructions => Instructions] = lzy{
    if (nr > 0)
      for {
        randominstr <- Gen.oneOf(GenRandomAdds(),GenRandomNewArrays(),GenRandomStore(),GenRandomLoad(),GenPrint())
        recurse <- GenRandomInstr(nr-1)
      } yield {
        val f: (Instructions => Instructions) = (instr: Instructions) => {
          recurse(randominstr(instr))
        }
        f
      }
    else
    {
      val f: (Instructions => Instructions) = (instr: Instructions) => instr
      Gen.const(f)
    }
  }


  def GenReduce(): (Gen[Exp[Int] => Exp[Int]]) = {

    for {
      number <- Gen.choose(1, 1000)
      nradds <- Gen.choose(30,40)
      fan <- GenFan()
      randomadd <- GenRandomInstr(nradds)
    }
    yield {
      val f: Exp[Int] => Exp[Int] = (in: Exp[Int]) => {
        val accumulator = array_obj_new[Int](Const(1))
        val newsym = array_update(accumulator, Const(0), Const(0))
        val fanout = fan(in)
        val empty = Instructions(Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Set.empty, 0, None)
        val ini = empty.copy(syms = fanout)
        val sofar = randomadd(ini)
        sofar.syms.foreach(
          ele => {
            val sum = array_apply(accumulator, Const(0))
            val const = int_plus(sum, Const(number))
            val newsym = int_plus(const, ele)
            val res = array_update(accumulator, Const(0), newsym)
          })
        val sum = array_apply(accumulator, Const(0))
        sum
      }
      f
    }
  }

}






import org.scalacheck.Prop._
import org.scalacheck._
class RandomDSLTest extends Properties("bitstuff") {


  println("hm?")

    def GenNewDSL(): Gen[GenRCode] = {
      for {
        number <- Gen.choose(1,100000)
      } yield
      {
        new GenRCode()
      }
    }


    def GenDSL(): Gen[GenRCode] = lzy{
     for {
      dsl <- GenNewDSL()
      randomf <- dsl.GenReduce()
    } yield {
      dsl.randomf = Some(randomf)
      dsl
    }
  }


  property("mult") = forAll(GenDSL) {
    gen => {
        val f = gen.randomf.get
        gen.codegen.emitSource(f, "Test", new PrintWriter(System.out))
     true
    }
  }
}
