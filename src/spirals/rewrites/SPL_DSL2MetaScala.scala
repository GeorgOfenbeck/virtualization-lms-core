package ch.ethz.spirals.rewrites

import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._


import org.apache.commons.math3.linear.BlockFieldMatrix
import org.apache.commons.math3.complex.{ComplexField, Complex}
import ch.ethz.spirals.dsls._

trait SPL_DSL2MetaScala extends PureDefaultTraversal {
  val IR: SPL_Exp with PureFunctionsExp
  val targetIR: StagedScala_DSL
  import targetIR.StagedComplex

  var f_array = Map.empty[Int, Vector[StagedComplex] => Vector[StagedComplex] ]
  var sizeinfo = Map.empty[Int, Int ]
  var finalnode: Int = 0
  var tmpfix = true

  import org.apache.commons.math3.linear.BlockFieldMatrix
  import org.apache.commons.math3.complex.{ComplexField, Complex}

  trait EmitSPL2MetaScala extends Emit{
    val traversal: Traversal {
      val cminfo : CodeMotion {
        val reifiedIR: ReificationPure {
          val IR: SPL_Exp with PureFunctionsExp}}}
    
    override def emitNode(sym: traversal.cminfo.reifiedIR.IR.Exp[_], rhs: traversal.cminfo.reifiedIR.IR.Def[_],
                          block_callback: traversal.cminfo.reifiedIR.IR.Block => Unit): Unit =  {
      import traversal.cminfo.reifiedIR.IR._      
      rhs match{
        //--------------------------------Compose -----------------------------
        case Compose(Exp(a),Exp(b)) => {
          val f: Vector[StagedComplex] => Vector[StagedComplex] = (in: Vector[StagedComplex]) => f_array(a)(f_array(b)(in))
          f_array = f_array + (sym.id -> f)
          sizeinfo = sizeinfo + (sym.id -> sizeinfo(a))
        }
        //-------------------------------Tensor--------------------------------
        case Tensor(Exp(a),Exp(b)) => {
          val f: Vector[StagedComplex] => Vector[StagedComplex] = (id2tp(a).rhs,id2tp(b).rhs) match {
            case (ConstDef(I(n)),_) => {
              sizeinfo = sizeinfo + (sym.id -> sizeinfo(b)*n)
             (in: Vector[StagedComplex]) =>
               in.grouped(sizeinfo(b)).flatMap( chunk => f_array(b)(chunk)).toVector
            }
            case (_,ConstDef(I(n))) => {
              sizeinfo = sizeinfo + (sym.id -> sizeinfo(a)*n)
              (in: Vector[StagedComplex]) =>
                in.grouped(n).toList.transpose.map( chunk => f_array(a)(chunk.toVector)).transpose.flatten.toVector
            }
            case _ => ??? //we dont support anyting else for this tutorial
          }

          f_array = f_array + (sym.id -> f)
        }
        //-------------------------------SPl Objects--------------------------------
        case ConstDef(x: SPL) => {
          val f: Vector[StagedComplex] => Vector[StagedComplex] =
            x match{
              case F_2() => (in: Vector[StagedComplex]) => {
                //if (in.size != 2)
                  //println(" .")
                  // assert(in.size == 2)
                Vector(in(0)+in(1),in(0)-in(1))
              }
              case I(n) => (in: Vector[StagedComplex]) => in
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
  def SPL2MetaScala (splgenf: => IR.Exp[SPL]): (Map[Int, Vector[StagedComplex] => Vector[StagedComplex] ], Int) = {
    val myf: IR.Exp[Unit] => IR.Exp[SPL] = (u: IR.Exp[Unit]) => splgenf
    val expos_u = IR.exposeRepFromRep[Unit]
    val expos_spl = IR.exposeRepFromRep[SPL]
    val emit = new EmitSPL2MetaScala {
      override val traversal = default_traversal(myf)(expos_u, expos_spl)
    }
    emit.emit()    
    (f_array, finalnode)
  }

}

