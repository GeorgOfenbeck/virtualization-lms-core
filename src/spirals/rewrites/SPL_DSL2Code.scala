package ch.ethz.spirals.rewrites

import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._


import ch.ethz.spirals.dsls._
import ch.ethz.spirals.datatypes._

trait SPL_DSL2Code extends PureDefaultTraversal {
 val IR: SPL_Exp with PureFunctionsExp

 import MathUtilities._



 trait EmitSPLCode extends Schedule{
  val traversal: Traversal {
   val cminfo : CodeMotion {
    val reifiedIR: ReificationPure {
     val IR: SPL_Exp with PureFunctionsExp}}}


  def emitH[R[_]](base: R[Int], strides: Vector[R[Int]])(implicit numeric: ch.ethz.spirals.datatypes.NumericOps[R[Int]])
  : (Vector[R[Int]]) => R[Int] = {
   val f: (Vector[R[Int]] => R[Int]) = (loopvars: Vector[R[Int]]) => {
    loopvars.zip(strides).foldLeft(base){
     import numeric._
     (acc,ele) => acc + (ele._1 * ele._2)
    }
   }
   f
  }


  def emitNode[V[_],E[_],R[_],T](tp: traversal.cminfo.reifiedIR.IR.TP[_], fmap: Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]],
               block_callback: traversal.cminfo.reifiedIR.IR.Block => Iterator[traversal.cminfo.reifiedIR.IR.TP[_]]
                ): Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]] =  {
   import traversal.cminfo.reifiedIR.IR._
   tp.rhs match{
    //--------------------------------Compose -----------------------------
    case Compose(Exp(a),Exp(b)) => {
     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => fmap(a)(fmap(b)(in))
     fmap + (tp.exp.id -> f)
    }
    //-------------------------------Tensor--------------------------------
    case Tensor(Const(I(n)),Exp(b)) => {
     val range: Int = ???
     val domain: Int = ???

     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => {
      val g: (Vector[R[Int]]) => R[Int] = (s: Vector[R[Int]]) => {
       ???
      }
      //in.GT(fmap(b),)
      ???
     }
     fmap + (tp.exp.id -> f)
    }
    case Tensor(Exp(a), Const(I(n))) => {
     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => {
      ???
     }
     fmap + (tp.exp.id -> f)
    }
    //------------------------------default traversal-----------------------------------------
    case Lambda(_,_,block,_,_) => block_callback(block).foldLeft(Map.empty[Int, CVector[V,E,R,T] => CVector[V,E,R,T]]){
     (acc,ele) => {
      emitNode(ele,acc, block_callback)
     }
    }
   }
  }


 }

 //-----------------------------------------Matrix Representation Part --------------------------------
 def SPL2Code[V[_],E[_],R[_] ,T] (splgenf: => IR.Exp[SPL])
                                 (implicit
 vrep: LiftOps[V],
 erep: ElementOps[E, R[T]],
 nrep: ch.ethz.spirals.datatypes.NumericOps[R[T]]
 )
 : Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T] ] = {
  val myf: IR.Exp[Unit] => IR.Exp[SPL] = (u: IR.Exp[Unit]) => splgenf
  val expos_u = IR.exposeRepFromRep[Unit]
  val expos_spl = IR.exposeRepFromRep[SPL]
  val emit = new EmitSPLCode {
   override val traversal = default_traversal(myf)(expos_u, expos_spl)
  }
  val col = new emit.MyCollection
  def tmp(b: emit.traversal.cminfo.reifiedIR.IR.Block): Iterator[emit.traversal.cminfo.reifiedIR.IR.TP[_]] = {
   val x = col.iterator(b)
   x
  }

  col.iterator().foldLeft(Map.empty[Int, CVector[V,E,R,T] => CVector[V,E,R,T]]){
   (acc,ele) => {
    emit.emitNode(ele,acc, tmp)
   }
  }
 }
}


