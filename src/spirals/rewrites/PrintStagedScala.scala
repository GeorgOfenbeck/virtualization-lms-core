package ch.ethz.spirals.rewrites



import scala.virtualization.lms.common._
import scala.virtualization.lms._
import internal._
import ch.ethz.spirals.dsls._




trait PrintStagedScala extends PureDefaultTraversal {
  val IR: StagedScala_Exp

  var f_array = Map.empty[Int, Vector[MyComplex] => Vector[MyComplex] ]
  var sizeinfo = Map.empty[Int, Int ]
  var finalnode: Int = 0
  var tmpfix = true


  trait EmitSPL2Scala extends Emit{
    val traversal: Traversal {
      val cminfo : CodeMotion {
        val reifiedIR: ReificationPure {
          val IR: StagedScala_Exp}}}

    override def emitNode(sym: traversal.cminfo.reifiedIR.IR.Exp[_], rhs: traversal.cminfo.reifiedIR.IR.Def[_], block_callback: traversal.cminfo.reifiedIR.IR.Block => Unit): Unit =  {
      import traversal.cminfo.reifiedIR.IR._
      rhs match{

        case NumericPlus(Exp(a),Exp(b)) => println(s"x${sym.id} = x$a + x$b")
        case NumericMinus(Exp(a),Exp(b)) => println(s"x${sym.id} = x$a - x$b")
        //------------------------------default traversal-----------------------------------------
        case Lambda(_,_,block,_,_) => if (tmpfix){ tmpfix = false; finalnode = block.res.head.id; block_callback(block)}
        case _ => super.emitNode(sym,rhs,block_callback)
      }
    }
  }

  //-----------------------------------------Matrix Representation Part --------------------------------
  def printStagedScala (splgenf: Vector[IR.StagedComplex] => Vector[IR.StagedComplex], size: Int): Unit = {
    val myf: IR.ComplexVector[Double] => IR.ComplexVector[Double]= (u: IR.ComplexVector[Double]) => {
      val r = splgenf(u.vec)
      IR.ComplexVector[Double](r)
    }

    import IR._
    //implicit val instance_size: Int = size
    //val expos_u =
    //val expos_spl = IR.exposeRepFromVComplex[Double]

    val emit = new EmitSPL2Scala {
      override val traversal = default_traversal(myf)(IR.exposeRepFromVComplex[Double](size), IR.exposeRepFromVComplex[Double](size))
    }
    emit.emit()
  }





}
