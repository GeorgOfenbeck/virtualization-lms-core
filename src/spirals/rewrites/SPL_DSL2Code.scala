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
                ) : Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]] =  {

   import traversal.cminfo.reifiedIR.IR._
   def getH(in: CVector[V,E,R,T], base: V[Int], strides: Vector[V[Int]]): (Vector[V[Int]]) => V[Int] = {
    val f: (Vector[V[Int]]) => V[Int] = (loopvars: Vector[V[Int]]) => {
     in.irep.plus(base,loopvars.zip(strides).foldLeft(in.vrep(0))({
      (acc,ele) => in.irep.plus(acc,(in.irep.plus(ele._1, ele._2)))
     }))
    }
    f
   }
   tp.rhs match{
    //--------------------------------Compose -----------------------------
    case Compose(Exp(a),Exp(b)) => {
     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => fmap(a)(fmap(b)(in))
     fmap + (tp.exp.id -> f)
    }
    //-------------------------------Tensor--------------------------------
    case Tensor(Const(I(k)),Exp(b)) => {
     val n = getSPLMeta(Exp(b)).size
     val A = fmap(b)
     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => {
      val h = getH(in,in.vrep(0),Vector(in.vrep(1),in.vrep(n)))
      in.GT(A,h,h,Vector(k))(in)
     }
     fmap + (tp.exp.id -> f)
    }
    case Tensor(Exp(a), Const(I(k))) =>{
     val n = getSPLMeta(Exp(a)).size
     val A = fmap(a)

     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => {
      val h = getH(in,in.vrep(0),Vector(in.vrep(k),in.vrep(1)))
      in.GT(A,h,h,Vector(k))(in)
     }

     fmap + (tp.exp.id -> f)
    }
    //-------------------------------SPl Objects--------------------------------
    case Const(i: I) => {
     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => {
      in
     }
     fmap + (tp.exp.id -> f)
     }
      case Const(f2: F_2) => {
       val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => {
        val idx0 = in.irep.fromInt(0)
        val idx1 = in.irep.fromInt(1)
        val t0 = in(idx0)
        val t1 = in(idx1)
        val out = in.create(in.irep.fromInt(2))

        val res1 = in.erep.plus(t0,t1)
        val res2 = in.erep.minus(t0,t1)

        out.update(idx0,res1)
        out.update(idx1,res2)
        out
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
 def SPL2Code[V[_],E[_],R[_],T](splgenf: => IR.Exp[SPL], sample: CVector[V,E,R,T])
                                   = {
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

  val bydecompmap = col.iterator().foldLeft(Map.empty[Int, CVector[V,E,R,T] => CVector[V,E,R,T]]){
   (acc,ele) => {
    emit.emitNode(ele,acc, tmp)
   }
  }

  val finalnode = bydecompmap.map(x => x._1).reduceLeft(_ max _)
  val finalcall = bydecompmap(finalnode)
  finalcall

 }
}


