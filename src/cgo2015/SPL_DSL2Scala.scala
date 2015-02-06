package ch.ethz.spirals.cgo2015

import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._


import org.apache.commons.math3.linear.BlockFieldMatrix
import org.apache.commons.math3.complex.{ComplexField, Complex}

trait SPL_DSL2Scala extends PureDefaultTraversal {
  val IR: SPL_Exp with PureFunctionsExp

  var f_array = Map.empty[Int, Vector[Double] => Vector[Double] ]
  var sizeinfo = Map.empty[Int, Int ]
  var finalnode: Int = 0
  var tmpfix = true

  import org.apache.commons.math3.linear.BlockFieldMatrix
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  trait EmitSPL2Scala extends Emit{
    val traversal: Traversal {
      val cminfo : CodeMotion {
        val reifiedIR: ReificationPure {
          val IR: SPL_Exp with PureFunctionsExp}}}
    
    override def emitNode(sym: traversal.cminfo.reifiedIR.IR.Exp[_], rhs: traversal.cminfo.reifiedIR.IR.Def[_], block_callback: traversal.cminfo.reifiedIR.IR.Block => Unit): Unit =  {
      import traversal.cminfo.reifiedIR.IR._      
      rhs match{
        //--------------------------------Compose -----------------------------
        case Compose(Exp(a),Exp(b)) => {
          val f: Vector[Double] => Vector[Double] = (in: Vector[Double]) => f_array(a)(f_array(b)(in))
          f_array = f_array + (sym.id -> f)
          sizeinfo = sizeinfo + (sym.id -> sizeinfo(a))
        }
        //-------------------------------Tensor--------------------------------
        case Tensor(Exp(a),Exp(b)) => {
          val f: Vector[Double] => Vector[Double] =(id2tp(a).rhs,id2tp(b).rhs) match {
            case (ConstDef(I(n)),_) => (in: Vector[Double]) => {
              in.grouped(sizeinfo(b)).flatMap( chunk => f_array(b)(chunk)).toVector
            }
            case (_,ConstDef(I(n))) => (in: Vector[Double]) => {
              val x = in.grouped(sizeinfo(a)).toList.transpose
              val y = x.map( chunk => f_array(a)(chunk.toVector)).transpose
              val z = y.flatten
              z.toVector
            }
            case _ => ??? //we dont support anyting else for this tutorial
          }
          sizeinfo = sizeinfo + (sym.id -> sizeinfo(a)*sizeinfo(b))
          f_array = f_array + (sym.id -> f)
        }
        //-------------------------------SPl Objects--------------------------------
        case ConstDef(x: SPL) => {
          val f: Vector[Double] => Vector[Double] =
            x match{
              case F_2() => (in: Vector[Double]) => Vector(in(0)+in(1),in(0)-in(1))
              case I(n) => (in: Vector[Double]) => in
              case _ => ??? //we dont support anyting else for this tutorial
          }
          f_array = f_array + (sym.id -> f)
          sizeinfo = sizeinfo + (sym.id -> 2)
        }
        //------------------------------default traversal-----------------------------------------
        case Lambda(_,_,block,_,_) => if (tmpfix){ tmpfix = false; finalnode = block.res.head.id; block_callback(block)}
        case _ => super.emitNode(sym,rhs,block_callback)  
      }
    }
  }

  //-----------------------------------------Matrix Representation Part --------------------------------
  def SPL2Scala (splgenf: => IR.Exp[SPL]): (Map[Int, Vector[Double] => Vector[Double] ], Int) = {
    val myf: IR.Exp[Unit] => IR.Exp[SPL] = (u: IR.Exp[Unit]) => splgenf
    val expos_u = IR.exposeRepFromRep[Unit]
    val expos_spl = IR.exposeRepFromRep[SPL]
    val emit = new EmitSPL2Scala {
      override val traversal = default_traversal(myf)(expos_u, expos_spl)
    }
    emit.emit()    
    (f_array, finalnode)
  }

  



}


/*

    in.grouped(in_size/n).toList flatMap (part => A(part))
    //split n into (in_size/n) parts - pass each part to A and return concatenated list
  }


  def A_tensor_I (in: List[CIR_DSL.Complex], in_size: Int, n: Int, A: ((List[CIR_DSL.Complex]) => List[CIR_DSL.Complex])): List[CIR_DSL.Complex] ={
    (in.grouped(n).toList.transpose map (part => A(part)) transpose).flatten
               */