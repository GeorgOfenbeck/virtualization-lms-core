package ch.ethz.spirals.cgo2015

import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._


import org.apache.commons.math3.linear.BlockFieldMatrix
import org.apache.commons.math3.complex.{ComplexField, Complex}

trait SPL_DSL2Mat extends PureDefaultTraversal  with ReifyPure {

  val IR: SPL_Exp with PureFunctionsExp

  var f_array = Map.empty[Int, BlockFieldMatrix[Complex] ]
  import org.apache.commons.math3.linear.BlockFieldMatrix
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  trait EmitSPLMatrix extends Emit{

    val traversal: Traversal {
      val cminfo : CodeMotion {
        val reifiedIR: ReificationPure {
          val IR: SPL_Exp with PureFunctionsExp
        }
      }
    }
    override def emitNode(sym: traversal.cminfo.reifiedIR.IR.Exp[_], rhs: traversal.cminfo.reifiedIR.IR.Def[_], block_callback: traversal.cminfo.reifiedIR.IR.Block => Unit): Unit =  {
      import traversal.cminfo.reifiedIR._
      import traversal.cminfo.reifiedIR.IR._
      
      rhs match{

        //--------------------------------Compose -----------------------------
        case Compose(Exp(a),Exp(b)) => {
          f_array = f_array + (sym.id -> f_array(a).multiply( f_array(b) ))
          println("adding " + sym.id)
        }
        //case Compose(Exp(a),Const(x: SPL)) => f_array(a).multiply( x.toMatrix() )
        //case Compose(Const(x: SPL), Exp(b)) =>  x.toMatrix().multiply( f_array(b) )
        //case Compose(Const(x: SPL), Const(y: SPL)) =>  x.toMatrix().multiply( y.toMatrix() )
        //-------------------------------Tensor--------------------------------
        case Tensor(Exp(a),Exp(b)) => {
          f_array = f_array + (sym.id -> kronecker(f_array(a),f_array(b)))
          println("adding " + sym.id)
        }
        //case Tensor(Exp(a),Const(x: SPL)) => kronecker(f_array(a), x.toMatrix())
        //case Tensor(Const(x: SPL), Exp(b)) =>  kronecker(x.toMatrix(),f_array(b))
        //case Tensor(Const(x: SPL), Const(y: SPL)) =>  kronecker(x.toMatrix(),y.toMatrix())
        case Lambda(_,_,block,_,_) => block_callback(block)
        case ConstDef(x: SPL) => {
          f_array = f_array + (sym.id -> x.toMatrix())
          println("adding " + sym.id)
        }
        case _ => {
          val x = 10
          super.emitNode(sym,rhs,block_callback)
        }
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
    emit.emit()
    //default_traversal2(myf)(expos_u, expos_spl)

    /*val deflist = buildScheduleForResult(start)
    val f_array = new Array[ BlockFieldMatrix[Complex] ](size)
    val index_array = new HashMap[Int,Int]
    var i : Int = 0
    for (TP(sym, rhs) <- deflist) {
      val index = sym match {
        case Sym(n) => n
        case _ => -1
      }
      index_array += (index -> i)
      f_array(i) = matrix_emitNode(sym, rhs,f_array , index_array)
      i = i + 1
    }
    f_array
*/
    f_array
  }

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

/*
  def matrix_emitNode(sym: Sym[Any], rhs: Def[Any],f_array: Array[ BlockFieldMatrix[Complex]] , lt : HashMap[Int,Int]):
  // returns matrix
  (  BlockFieldMatrix[Complex] ) =
    rhs match {


    //-------------------------------SPL_DirectSum--------------------------------
    case DirectSum(Sym(a),Sym(b)) => matrix_directsum(f_array(a),f_array(b))
    case DirectSum(Sym(a),Const(x: SPL)) => matrix_directsum(f_array(a),x.toMatrix())
    case DirectSum(Const(x: SPL), Sym(b)) => matrix_directsum(x.toMatrix(),f_array(b))
    case DirectSum(Const(x: SPL), Const(y: SPL)) => matrix_directsum(x.toMatrix(),y.toMatrix())
  }*/



}

