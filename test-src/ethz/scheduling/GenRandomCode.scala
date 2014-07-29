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


class GenRCode extends PrimitiveOpsExp with OrderingOpsExp with VariablesExp
with WhileExp with RangeOpsExp with ArrayOpsExp with PrintExp with CompileScala {
  self =>

  val codegen = new ScalaGenPrimitiveOps with ScalaGenOrderingOps
    with ScalaGenVariables with ScalaGenWhile with ScalaGenRangeOps with ScalaGenArrayOps
    with ScalaGenPrint {
    val IR: self.type = self
  }




  var randomf: Option[Exp[Int] => Exp[Int]] = None
  var randomuf: Option[Int => Int] = None

  case class inipos(val arr: Exp[Array[Int]], val pos: Int)
  case class uinipos(val arr: Array[Int], val pos: Int)

  case class Instructions(
                           val syms: Vector[Exp[Int]],
                           val arrays: Vector[Exp[Array[Int]]],
                           val initialized: Set[inipos]

                           )

  case class uInstructions
  (
    val usyms: Vector[Int],
    val farrays: Vector[Unit => Array[Int]],
    val initialized: Set[uinipos]
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



  def GenFan(): (Gen[ ((Exp[Int] => Vector[Exp[Int]]), (Int => Vector[Int]) ) ]) = {
    for {
      number <- Gen.choose(1,100)
    } yield {
      val fstaged: Exp[Int] => Vector[Exp[Int]] = (in: Exp[Int]) => {
        (for (i <- 0 until number) yield int_plus(in,Const(number))).toVector
      }
      val f: Int => Vector[Int] = (in: Int) => {
        (for (i <- 0 until number) yield (in + number)).toVector
      }
      (fstaged,f)
    }
  }

  def GenRandomStore(): Gen[ ((Instructions => Instructions), (uInstructions => uInstructions) )] = lzy {
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
      val g: (uInstructions => uInstructions) = (instr: uInstructions) => {
        if (instr.farrays.size > 0) {
          val choiceidx = choice % instr.farrays.size
          val symchoiceidx = symchoice % instr.usyms.size
          val chosenarray = instr.farrays(choiceidx)()
          chosenarray(0) = instr.usyms(symchoiceidx)
          val ini = uinipos(chosenarray, 0)
          val newinst = instr.copy(initialized = instr.initialized + ini)
          newinst
        }
        else
          instr
      }
      (f,g)
    }
  }

  def GenRandomLoad(): Gen[ ((Instructions => Instructions), (uInstructions => uInstructions) )] = lzy {
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
      val g: (uInstructions => uInstructions) = (instr: uInstructions) => {
        if (instr.initialized.size > 0) {
          val choiceidx = choice % instr.initialized.size
          val pos = instr.initialized.toVector(choiceidx)

          val chosenarray = pos.arr
          val newsym = pos.arr(pos.pos)
          val newinst = instr.copy(usyms = instr.usyms :+ newsym)
          newinst
        }
        else
          instr
      }
      (f,g)
    }
  }


  def GenRandomNewArrays(): Gen[ ((Instructions => Instructions), (uInstructions => uInstructions) )] = lzy {
    for {
      arrsize <- Gen.choose(1,2)
    } yield {
      val f: (Instructions => Instructions) = (instr: Instructions) => {
        val newarr = array_obj_new[Int](Const(arrsize))
        val newinst = instr.copy(arrays = instr.arrays :+ newarr)
        newinst
      }
      val g: (uInstructions => uInstructions) = (instr: uInstructions) => {
        //val newarr = array_obj_new[Int](Const(arrsize))
        val arr: Array[Int] = new Array[Int](arrsize)
        val farray: (Unit => Array[Int]) = (u: Unit) => {
          arr
        }
        val newinst = instr.copy(farrays = instr.farrays :+ farray)
        newinst
      }
      (f,g)
    }
  }

  def GenRandomAdds(): Gen[ ( (Instructions => Instructions), (uInstructions => uInstructions) ) ] = lzy {
    for {
      lhs <- Gen.choose(1, 1000)
      rhs <- Gen.choose(1, 1000)
    } yield {
      val f: (Instructions => Instructions) = (instr: Instructions) => {

        //assert(instr.syms.size == instr.usyms.size)
        val in = instr.syms
        val lhsidx: Int = lhs % in.size
        val rhsidx: Int = rhs % in.size
        val newele = int_minus(in(lhsidx), in(rhsidx))
        val newinst = instr.copy(syms = instr.syms :+ newele)
        newinst
      }
      val g: (uInstructions => uInstructions) = (instr: uInstructions) => {
        val in = instr.usyms
        val lhsidx: Int = lhs % in.size
        val rhsidx: Int = rhs % in.size
        val newint = in(lhsidx) - in(rhsidx)
        val newinst = instr.copy(usyms = instr.usyms :+ newint)
        newinst
      }
      (f,g)
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


  def GenRandomBlock(codestyle: CodeStyle): Gen[ ( (Instructions => Instructions), (uInstructions => uInstructions) ) ] = lzy {
    for {
      repeats <- Gen.choose(1, 2)
      body <- GenRandomInstr(codestyle,0)
    } yield {
      val f: (Instructions => Instructions) = (sofar: Instructions) => {
        val range: Rep[Range] = range_until(Const(0), Const(1))
        val block: Rep[Int] => Rep[Unit] = (i: Rep[Int])=> {
          val withiter = sofar.copy(syms = sofar.syms :+ i)
          val res = body._1(withiter)
          Reduce(res)
          Const(())
        }
        range_foreach(range,block)
        sofar
      }
      val g: (uInstructions => uInstructions) = (sofar: uInstructions) => {

        for (i <- 0 until 1)
        {
          val withiter = sofar.copy(usyms = sofar.usyms :+ i)
          val res = body._2(withiter)
          ReduceF(res)
        }
        sofar
      }
      (f,g)

    }
  }





/*
  def GenRandomInstr(codestyle: CodeStyle, blocks: Int): Gen[Instructions => Instructions] = lzy{
    if (codestyle.nrinstructions > 0) {
      if (codestyle.nestdepth > 0 && blocks < codestyle.nestperlevel) {
        for {
          workaround <- Gen.choose(0,5)
          randominstr <- if (workaround == 0) GenRandomBlock(codestyle.decnest()) else Gen.oneOf(GenRandomAdds(), GenRandomNewArrays(), GenRandomStore(), GenRandomLoad(), GenPrint())
          recurse <- if (workaround == 0 ) GenRandomInstr(codestyle, blocks + 1) else GenRandomInstr(codestyle, blocks)
        } yield {
          val f: (Instructions => Instructions) = (instr: Instructions) => {
            recurse(randominstr(instr))
          }
          f
        }
      }
      else
      {
        for {
          randominstr <- Gen.oneOf(GenRandomAdds(), GenRandomNewArrays(), GenRandomStore(), GenRandomLoad(), GenPrint())
          recurse <- GenRandomInstr(codestyle.decinstr(), blocks)
        } yield {
          val f: (Instructions => Instructions) = (instr: Instructions) => {
            recurse(randominstr(instr))
          }
          f
        }
      }
    }
    else
    {
      val f: (Instructions => Instructions) = (instr: Instructions) => instr
      Gen.const(f)
    }
  }
 */
  def GenRandomInstr(codestyle: CodeStyle, blocks: Int): Gen[ ( (Instructions => Instructions ), (uInstructions => uInstructions) )] = lzy{
    if (codestyle.nrinstructions > 0) {
      if (codestyle.nestdepth > 0 && blocks < codestyle.nestperlevel) {
        for {
          workaround <- Gen.choose(0,5)
          randominstr <- if (workaround == 0) GenRandomBlock(codestyle.decnest()) else Gen.oneOf(GenRandomAdds(), GenRandomNewArrays(), GenRandomStore(), GenRandomLoad())
          recurse <- if (workaround == 0 ) GenRandomInstr(codestyle.decinstr(), blocks + 1) else GenRandomInstr(codestyle.decinstr(), blocks)
        } yield {
          val f: (Instructions => Instructions) = (instr: Instructions) => {
            recurse._1(randominstr._1(instr))
          }
          val g: (uInstructions => uInstructions) = (uinstr: uInstructions) => {
            recurse._2(randominstr._2(uinstr))
          }
          (f, g)
        }
      }
      else {
        for {
          randominstr <- Gen.oneOf(GenRandomAdds(), GenRandomNewArrays(), GenRandomStore(), GenRandomLoad()) //, , , GenPrint())
          recurse <- GenRandomInstr(codestyle.decinstr(), blocks)
        } yield {
          val f: (Instructions => Instructions) = (instr: Instructions) => {
            recurse._1(randominstr._1(instr))
          }
          val g: (uInstructions => uInstructions) = (uinstr: uInstructions) => {
            recurse._2(randominstr._2(uinstr))
          }
          (f, g)
        }
      }
    }
    else
    {
      val f: (Instructions => Instructions) = (instr: Instructions) => instr
      val g: (uInstructions => uInstructions) = (uinstr: uInstructions) => uinstr
      Gen.const((f,g))
    }
  }



  def GenF(): ( (Gen[ (Exp[Int] => Exp[Int], Int => Int )])) = {
  //def GenF(): ( (Gen[ (Exp[Int] => Exp[Int])])) = {
    for {
      number <- Gen.choose(1, 1000)
      nrinstr <- Gen.choose(30,40)
      deepth <- Gen.choose(0,1)
      perlevel <- Gen.choose(1,2)
      fan <- GenFan()
      randoinstr <- GenRandomInstr(CodeStyle(nrinstr,deepth,perlevel),0)
    }
    yield {
      val empty = Instructions(Vector.empty,Vector.empty, Set.empty)
      val uempty = uInstructions(Vector.empty,Vector.empty, Set.empty)
      val fstaged: Exp[Int] => Exp[Int] = (in: Exp[Int]) => {
        val fanout = fan._1(in) //create a bit of diversity at the start
        val ini = empty.copy(syms = fanout)
        val randombody = randoinstr._1(ini)
        val res = Reduce(ini)
        res
      }
      val f: Int => Int = (in: Int) => {
        val fanout = fan._2(in) //create a bit of diversity at the start
        val ini = uempty.copy(usyms = fanout)
        val randombody = randoinstr._2(ini)
        val res = ReduceF(ini)
        res
      }
      (fstaged,f)
    }
  }


  def ReduceF(in: uInstructions): Int = {
    in.usyms.foldLeft(0){
      (acc,ele) => acc + ele
    }
  }

  def Reduce(in: Instructions): Exp[Int] = {
        val accumulator = array_obj_new[Int](Const(1))
        val newsym = array_update(accumulator, Const(0), Const(0))
        in.syms.foreach(
          ele => {
            val sum = array_apply(accumulator, Const(0))
            val newsym = int_plus(sum, ele)
            val res = array_update(accumulator, Const(0), newsym)
          })
        val sum = array_apply(accumulator, Const(0))
        sum
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
      randomf <- dsl.GenF()
    } yield {
      dsl.randomf = Some(randomf._1)
      dsl.randomuf = Some(randomf._2)
      dsl
    }
  }


  property("mult") = forAll(GenDSL) {
    gen => {
        val f = gen.randomf.get
        val compiled = gen.compile(f)
        val fcompose = gen.randomuf.get

        val cres = compiled(1)
        val fres = fcompose(1)
        println(cres, fres)
        cres == fres
        //gen.codegen.emitSource(f, "Test", new PrintWriter(System.out))

    }
  }
}
