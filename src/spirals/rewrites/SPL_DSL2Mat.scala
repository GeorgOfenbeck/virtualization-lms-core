package ch.ethz.spirals.rewrites

import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._


import org.apache.commons.math3.linear.BlockFieldMatrix
import org.apache.commons.math3.complex.{ComplexField, Complex}
import ch.ethz.spirals.dsls._

trait SPL_DSL2Mat extends PureDefaultTraversal {
  val IR: SPL_Exp with PureFunctionsExp
  
  var finalnode: Int = 0
  var tmpfix = true

  import MathUtilities._
  import org.apache.commons.math3.linear.BlockFieldMatrix
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  trait EmitSPLMatrix extends Schedule{
    val traversal: Traversal {
      val cminfo : CodeMotion {
        val reifiedIR: ReificationPure {
          val IR: SPL_Exp with PureFunctionsExp}}}


    def emitNode(tp: traversal.cminfo.reifiedIR.IR.TP[_], mmap: Map[Int, BlockFieldMatrix[Complex]], block_callback: traversal.cminfo.reifiedIR.IR.Block => Iterator[traversal.cminfo.reifiedIR.IR.TP[_]]): Map[Int, BlockFieldMatrix[Complex]] =  {
      import traversal.cminfo.reifiedIR.IR._      
      tp.rhs match{
        //--------------------------------Compose -----------------------------
        case Compose(Exp(a),Exp(b)) => mmap + (tp.exp.id -> mmap(a).multiply( mmap(b) ))
        //-------------------------------Tensor--------------------------------
        case Tensor(Exp(a),Exp(b)) => mmap + (tp.exp.id -> kronecker(mmap(a),mmap(b)))
        //-------------------------------SPl Objects--------------------------------
        case ConstDef(x: SPL) => mmap + (tp.exp.id -> x.toMatrix())
        //------------------------------default traversal-----------------------------------------
        case Lambda(_,_,block,_,_) => block_callback(block).foldLeft(Map.empty[Int, BlockFieldMatrix[Complex]]){
          (acc,ele) => {
            emitNode(ele,acc, block_callback)
          }
        }
    //    case _ => super.emitNode(sym,rhs,block_callback)
      }
    }
  }

  //-----------------------------------------Matrix Representation Part --------------------------------
  def SPL2Mat (splgenf: => IR.Exp[SPL]): Map[Int, BlockFieldMatrix[Complex] ] = {
    val myf: IR.Exp[Unit] => IR.Exp[SPL] = (u: IR.Exp[Unit]) => splgenf
    val expos_u = IR.exposeRepFromRep[Unit]
    val expos_spl = IR.exposeRepFromRep[SPL]
    val emit = new EmitSPLMatrix {
      override val traversal = default_traversal(myf)(expos_u, expos_spl)
    }
    val col = new emit.MyCollection
    def tmp(b: emit.traversal.cminfo.reifiedIR.IR.Block): Iterator[emit.traversal.cminfo.reifiedIR.IR.TP[_]] = {
      val x = col.iterator(b)
      x
    }

    val it = col.iterator()
    val hnext = it.hasNext
    it.foldLeft(Map.empty[Int, BlockFieldMatrix[Complex]]){
      (acc,ele) => {
        emit.emitNode(ele,acc, tmp)
      }
    }
  }

  /*
  def SPL2Mat (splgenf: => IR.Exp[SPL]): (Map[Int, BlockFieldMatrix[Complex] ], Int) = {
    val myf: IR.Exp[Unit] => IR.Exp[SPL] = (u: IR.Exp[Unit]) => splgenf
    val expos_u = IR.exposeRepFromRep[Unit]
    val expos_spl = IR.exposeRepFromRep[SPL]
    val emit = new EmitSPLMatrix {
      override val traversal = default_traversal(myf)(expos_u, expos_spl)
    }
    emit.emit()    
    (f_array, finalnode)
  } */

  



}


