package ch.ethz.spirals.rewrites
import ch.ethz.spirals.dsls._
import scala.lms._
import internal._


  trait SPL_DSL2StagedScala extends Emit[Map[Int, Vector[StagedComplex] => Vector[StagedComplex]]] {

    override def emitNode(tp: self.IR.TP[_], fmap: Map[Int, Vector[StagedComplex] => Vector[StagedComplex]],
                          block_callback: (self.IR.Block, Map[Int, Vector[StagedComplex] => Vector[StagedComplex]]) => Map[Int, Vector[StagedComplex] => Vector[StagedComplex]])
    : Map[Int, Vector[StagedComplex] => Vector[StagedComplex]] = {
      tp.rhs match {
        //--------------------------------Compose -----------------------------
        case IR.Compose(Exp(a), Exp(b), size) => {
          val f: Vector[StagedComplex] => Vector[StagedComplex] = (in: Vector[StagedComplex]) => fmap(a)(fmap(b)(in))
          fmap + (tp.sym.id -> f)
        }
        //-------------------------------Tensor--------------------------------
        case IR.Tensor(Exp(a), Exp(b), size) => {
          val f: Vector[StagedComplex] => Vector[StagedComplex] = (id2tp(a).rhs, id2tp(b).rhs) match {
            case (ConstDef(I(n)), _) => {
              (in: Vector[StagedComplex]) =>
                in.grouped(size / n).flatMap(chunk => fmap(b)(chunk)).toVector
            }
            case (_, ConstDef(I(n))) => {
              (in: Vector[StagedComplex]) =>
                in.grouped(n).toList.transpose.map(chunk => fmap(a)(chunk.toVector)).transpose.flatten.toVector
            }
            case _ => ??? //we dont support anyting else for this tutorial
          }
          fmap + (tp.sym.id -> f)
        }
        //-------------------------------SPl Objects--------------------------------
        case ConstDef(x: SPL) => {
          val f: Vector[StagedComplex] => Vector[StagedComplex] =
            x match {
              case I(n) => (in: Vector[StagedComplex]) => in
              case F_2() => (in: Vector[StagedComplex]) => {
                Vector(in(0) + in(1), in(0) - in(1))
              }
              case _ => ??? //we dont support anyting else for this tutorial
            }
          fmap + (tp.sym.id -> f)
        }
        //------------------------------default traversal-----------------------------------------
        case InternalLambda(_, _, block, _, _) => block_callback(block, fmap)
        case _ => super.emitNode(tp, fmap, block_callback)
      }
    }
  }
}
