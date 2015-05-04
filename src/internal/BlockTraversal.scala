
package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


trait BlockTraversal extends GraphTraversal {
  val IR: Expressions
  import IR._

  type Block[+T]
  
  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T]

  def compactize(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = { throw new Exception("Method compactize should be overriden.") }

  def getFreeVarBlock(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = Nil

  def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = Nil // TODO: Nil or Exception??

  def getBlockResult[A](s: Block[A]): Exp[A] = getBlockResultFull(s) // = s.res
  def getBlockResultFull[A](s: Block[A]): Exp[A] // = s.res
  
  def traverseBlock[A](block: Block[A]): Unit
  def traverseTP(tp: TP[_]): Unit
  
  def reset: Unit = ()
}




trait NestedBlockTraversal extends BlockTraversal with NestedGraphTraversal {
  val IR: Expressions with Effects
  import IR._

  // ----- block definition

  type Block[+T] = IR.Block[T]
  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T] = IR.reifyEffects(x)

  override def getBlockResultFull[A](s: Block[A]): Exp[A] = s.res
  
  override def getBlockResult[A](s: Block[A]): Exp[A] = s match {
    case Block(Def(Reify(x, _, _))) => x
    case Block(x) => x
  }

  
  def focusBlock[A](result: Block[Any])(body: => A): A = 
    focusFatBlock(Vector(result))(body)
    
  def focusFatBlock[A](result: Vector[Block[Any]])(body: => A): A =
    focusSubGraph[A](result.map(getBlockResultFull))(body)


  def focusExactScope[A](resultB: Block[Any])(body: Vector[TP[_]] => A): A =
    focusExactScopeFat(Vector(resultB))(body)
  
  def focusExactScopeFat[A](resultB: Vector[Block[Any]])(body: Vector[TP[_]] => A): A =
    focusExactScopeSubGraph[A](resultB.map(getBlockResultFull))(body)
  
  // ---- bound and free vars

  def boundInScope(x: Vector[Exp[Any]]): Vector[Sym[Any]] = {
    //(x.flatMap(syms):::innerScope.flatMap(t => t.lhs:::boundSyms(t.rhs))).distinct
  }
  
  def usedInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
    (y.flatMap(syms):::innerScope.flatMap(t => syms(t.rhs))).distinct
  }
  
  def readInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
    (y.flatMap(syms):::innerScope.flatMap(t => readSyms(t.rhs))).distinct
  }
  
  // bound/used/free variables in current scope, with input vars x (bound!) and result y (used!)
  def boundAndUsedInScope(x: List[Exp[Any]], y: List[Exp[Any]]): (List[Sym[Any]], List[Sym[Any]]) = {
    (boundInScope(x), usedInScope(y))
  }

  def freeInScope(x: List[Exp[Any]], y: List[Exp[Any]]): List[Sym[Any]] = {
    val (bound, used) = boundAndUsedInScope(x,y)
    // aks: freeInScope used to collect effects that are not true input dependencies. TR, any better solution?
    // i would expect read to be a subset of used, but there are cases where read has symbols not in used (TODO: investigate)
    val read = readInScope(y)
    (used intersect read) diff bound
  }

  // TODO: remove
  override def getFreeVarBlock(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = {
    focusBlock(start) {
      freeInScope(local, List(getBlockResultFull(start)))
    }
  }

  override def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = Nil // FIXME: should have generic impl



  // ----- high level api

  def traverseBlock[A](block: Block[A]): Unit = {
    focusBlock(block) {
      traverseBlockFocused(block)
    }
  }

  def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block) { levelScope =>
      traverseTPsInBlock(levelScope)
    }
  }

  def traverseTPsInBlock[A](stms: Vector[TP[_]]): Unit = {
    stms foreach traverseStm
  }

  def traverseStm(tp: TP[_]): Unit = { // override this to implement custom traversal
    blocks(tp.rhs) foreach traverseBlock
  }

  // ----- reset

  override def reset { // used anywhere?
    innerScope = null
    //shallow = false
    IR.reset
    super.reset
  }
}


