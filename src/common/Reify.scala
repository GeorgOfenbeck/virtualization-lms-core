package scala.virtualization.lms
package common


import scala.virtualization.lms.internal._
import scala.collection.immutable.IntMap

trait Reification {
  val IR: Blocks
  val globalDefs: Vector[IR.Stm]
  val localDefs: Vector[IR.Stm]
  val globalDefsCache: IntMap[IR.Stm]
  val result: IR.Block[Any]
  val args: Vector[IR.Sym[Any]]
}


trait Reify {
  self =>
  val IR: Blocks with PureFunctionsExp
  import IR._


  def reifyProgram[T : Manifest, R : Manifest](f: T => Exp[R], builder: => (T, Vector[IR.Sym[Any]])): Reification = {
    IR.reset
    val (s, args) = builder
    reifyProgram(f(s),args)
  }

  def reifyProgramX[T : Manifest, R : Manifest](f: Exp[T] => Exp[R]): Reification = reifyProgram(f)

  def reifyProgram[T : Manifest, R : Manifest](f: Exp[T] => Exp[R]): Reification = {
    IR.reset
    val s = fresh[T]
    reifyProgram(f(s),Vector(s))
  }

  def reifyProgram[T1 : Manifest, T2 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2]) => Exp[R]): Reification = {
    IR.reset
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    reifyProgram(f(s1,s2), Vector(s1,s2))
  }

  def reifyProgram[T1 : Manifest, T2 : Manifest, T3 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3]) => Exp[R]): Reification = {
    IR.reset
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    reifyProgram(f(s1,s2,s3),Vector(s1,s2, s3))
  }

  def reifyProgram[T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[R]): Reification = {
    IR.reset
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    reifyProgram(f(s1,s2,s3,s4), Vector(s1,s2,s3,s4))
  }

  def reifyProgram[T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5]) => Exp[R]): Reification = {
    IR.reset
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    reifyProgram(f(s1,s2,s3,s4,s5), Vector(s1,s2,s3,s4,s5))
  }


  def reifyProgram[A: Manifest, B: Manifest](f: Lambda[A,B] ): Reification = {
    val (progresult, defs) = reifySubGraph(f.f(f.x))
    reflectSubGraph(defs)
    val immutable_out = new Reification {
      val IR: self.IR.type = self.IR
      val globalDefs: Vector[IR.Stm] = self.IR.globalDefs
      val localDefs: Vector[IR.Stm] = self.IR.localDefs
      val globalDefsCache: IntMap[IR.Stm] = self.IR.globalDefsCache
      val result = Block(progresult)
      val args = Vector(f.x)
    }
    immutable_out
  }


  protected def reifyProgram[T: Manifest](x: => Exp[T], pargs: Vector[Sym[Any]]): Reification = {
    val (progresult, defs) = reifySubGraph(x)
    reflectSubGraph(defs)
    val immutable_out = new Reification {
      val IR: self.IR.type = self.IR
      val globalDefs: Vector[IR.Stm] = self.IR.globalDefs
      val localDefs: Vector[IR.Stm] = self.IR.localDefs
      val globalDefsCache: IntMap[IR.Stm] = self.IR.globalDefsCache
      val result = Block(progresult)
      val args = pargs
    }
    immutable_out
  }
}


/*
package scala.virtualization.lms
package common



import scala.virtualization.lms.internal._

trait ReifiedProgram {
  val globalDefs: List[Stm]
  val globalDefsCache: Map[Sym[Any],Stm]
  val localDefs: List[Stm]
}


trait ReifyProgram extends Expressions{

  def reifyProgram[T: Manifest] (x: => Exp[T]): ReifiedProgram = {
    println("hae?")

    val mutablecopy = new ReifyProgram {
      override implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] =
      {

        findOrCreateDefinitionExp(d, List(pos)) // TBD: return Const(()) if type is Unit??
      }
    }



    val (result, defs) = mutablecopy.reifySubGraph(() => x)
    mutablecopy.reflectSubGraph(defs)

    val reified = new ReifiedProgram {
      override val globalDefs: List[Stm] = mutablecopy.globalDefs
      override val globalDefsCache: Map[Sym[Any], Stm] = mutablecopy.globalDefsCache
      override val localDefs: List[Stm] = mutablecopy.localDefs
    }
    reified
    }
}
*/
