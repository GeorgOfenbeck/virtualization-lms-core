package ch.ethz.spirals.rewrites

import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._


import ch.ethz.spirals.dsls._
import ch.ethz.spirals.datatypes._

trait SigmaSPL2Code extends PureDefaultTraversal {
 val IR: DFTSigmaSPLExp with PureFunctionsExp

 import MathUtilities._



 trait EmitSigmaSPLCode extends Schedule{
  val traversal: Traversal {
   val cminfo : CodeMotion {
    val reifiedIR: ReificationPure {
     val IR: DFTSigmaSPLExp with PureFunctionsExp}}}


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


  def emitNode[V[_],E[_],R[_],T](tp: traversal.cminfo.reifiedIR.IR.TP[_], fmap: Map[Int, Either[ (CVector[V,E,R,T] => CVector[V,E,R,T]), (Vector[V[Int]] => V[Int] )]],
                                 block_callback: traversal.cminfo.reifiedIR.IR.Block => Iterator[traversal.cminfo.reifiedIR.IR.TP[_]],
                                 sample: CVector[V,E,R,T]
                                  ) : Map[Int, Either[ (CVector[V,E,R,T] => CVector[V,E,R,T]), (Vector[V[Int]] => V[Int] )]] =  {

   import traversal.cminfo.reifiedIR.IR._

   def getH(in: CVector[V,E,R,T], base: V[Int], strides: Vector[V[Int]]): (Vector[V[Int]]) => V[Int] = {
    val f: (Vector[V[Int]]) => V[Int] = (loopvars: Vector[V[Int]]) => {
     in.irep.plus(base,loopvars.zip(strides).foldLeft(in.vrep(0))({
      (acc,ele) => in.irep.plus(acc,(in.irep.times(ele._1, ele._2)))
     }))
    }
    f
   }
   tp.rhs match{
    //-------------------------------Tensor--------------------------------
    /*case GT(x,a,IMH_H(frag,base,strides,range,domain),s,v) => {
     val A = fmap(a.id)
     val h = getH(sample,sample.vrep(base),strides.map( stride => sample.vrep(stride) ))
     val f: CVector[V,E,R,T] => CVector[V,E,R,T] = sample.GT(A,h,h,v)
     fmap + (tp.exp.id -> Left(f))
    }*/
    //-------------------------------SPl Objects--------------------------------
    case SPL_F2() => {
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
     fmap + (tp.exp.id -> Left(f))
    }

    //------------------------------default traversal-----------------------------------------
    case Lambda(_,_,block,_,_) => {
     val empt = Map.empty[Int, Either[ (CVector[V,E,R,T] => CVector[V,E,R,T]), (Vector[V[Int]] => V[Int] )]]
     block_callback(block).foldLeft(empt){
      (acc,ele) => {
       emitNode(ele,acc, block_callback,sample)
      }
     }
    }
   }
  }
 }




 //-----------------------------------------Matrix Representation Part --------------------------------
 def SigmaSPL2Code[V[_],E[_],R[_],T](splgenf: => IR.Exp[IR.SigmaSPLVector], sample: CVector[V,E,R,T])
 = {
  val myf: IR.Exp[Unit] => IR.Exp[IR.SigmaSPLVector] = (u: IR.Exp[Unit]) => splgenf
  val expos_u = IR.exposeRepFromRep[Unit]
  val expos_spl = IR.exposeRepFromRep[IR.SigmaSPLVector]
  val emit = new EmitSigmaSPLCode {
   override val traversal = default_traversal(myf)(expos_u, expos_spl)
  }
  val col = new emit.MyCollection
  def tmp(b: emit.traversal.cminfo.reifiedIR.IR.Block): Iterator[emit.traversal.cminfo.reifiedIR.IR.TP[_]] = {
   val x = col.iterator(b)
   x
  }



  val bydecompmap = col.iterator().foldLeft(Map.empty[Int,
    Either[
      (CVector[V,E,R,T] => CVector[V,E,R,T] ),
  (Vector[V[Int]] => V[Int] )]]
  ){
   (acc,ele) => {
    emit.emitNode(ele,acc, tmp, sample)
   }
  }

  val finalnode = bydecompmap.map(x => x._1).reduceLeft(_ max _)
  val finalcall = bydecompmap(finalnode)
  finalcall

 }
}


