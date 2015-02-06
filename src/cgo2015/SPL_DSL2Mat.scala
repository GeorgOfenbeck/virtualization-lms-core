package ch.ethz.spirals.cgo2015

import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._


import org.apache.commons.math3.linear.BlockFieldMatrix
import org.apache.commons.math3.complex.{ComplexField, Complex}

trait SPL_DSL2Mat extends PureDefaultTraversal {
  val IR: SPL_Exp with PureFunctionsExp

  var f_array = Map.empty[Int, BlockFieldMatrix[Complex] ]
  var finalnode: Int = 0
  var tmpfix = true


  def kronecker (A: BlockFieldMatrix[Complex], B: BlockFieldMatrix[Complex] ): BlockFieldMatrix[Complex] = {
    val x = A.getRowDimension() * B.getRowDimension()
    val y = A.getColumnDimension() * B.getColumnDimension()
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), x, y)
    for (i <- 0 until x)
      for (j <- 0 until y)
      {
        val aentry = A.getEntry( (i/B.getRowDimension()), j/B.getColumnDimension() )
        val bentry = B.getEntry(i % B.getRowDimension(), j % B.getColumnDimension())
        m.setEntry(i,j, aentry.multiply(bentry))
      }
    m
  }
  
  import org.apache.commons.math3.linear.BlockFieldMatrix
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  trait EmitSPLMatrix extends Emit{
    val traversal: Traversal {
      val cminfo : CodeMotion {
        val reifiedIR: ReificationPure {
          val IR: SPL_Exp with PureFunctionsExp}}}
    
    override def emitNode(sym: traversal.cminfo.reifiedIR.IR.Exp[_], rhs: traversal.cminfo.reifiedIR.IR.Def[_], block_callback: traversal.cminfo.reifiedIR.IR.Block => Unit): Unit =  {
      import traversal.cminfo.reifiedIR.IR._      
      rhs match{
        //--------------------------------Compose -----------------------------
        case Compose(Exp(a),Exp(b)) => f_array = f_array + (sym.id -> f_array(a).multiply( f_array(b) ))
        //-------------------------------Tensor--------------------------------
        case Tensor(Exp(a),Exp(b)) => f_array = f_array + (sym.id -> kronecker(f_array(a),f_array(b)))
        //-------------------------------SPl Objects--------------------------------
        case ConstDef(x: SPL) => f_array = f_array + (sym.id -> x.toMatrix())
        //------------------------------default traversal-----------------------------------------
        case Lambda(_,_,block,_,_) => if (tmpfix){ tmpfix = false; finalnode = block.res.head.id; block_callback(block)}
        case _ => super.emitNode(sym,rhs,block_callback)  
      }
    }
  }

  //-----------------------------------------Matrix Representation Part --------------------------------
  def SPL2Mat (splgenf: => IR.Exp[SPL]): (Map[Int, BlockFieldMatrix[Complex] ], Int) = {
    val myf: IR.Exp[Unit] => IR.Exp[SPL] = (u: IR.Exp[Unit]) => splgenf
    val expos_u = IR.exposeRepFromRep[Unit]
    val expos_spl = IR.exposeRepFromRep[SPL]
    val emit = new EmitSPLMatrix {
      override val traversal = default_traversal(myf)(expos_u, expos_spl)
    }
    emit.emit()    
    (f_array, finalnode)
  }

  



}

