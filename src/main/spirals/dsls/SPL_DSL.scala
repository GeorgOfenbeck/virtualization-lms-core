package ch.ethz.spirals.dsls
import scala.lms._
import scala.lms.internal.InternalFunctionsExp

/**
 * This is the conversion of the original SPL to the non-pointfree version
 */
trait SPL_Base extends Base {
  //=============================================================================
  // SPL Operators
  //=============================================================================
  def SPLtoRep(i: SPL): Rep[SPL] = unit(i)
  implicit def mkSPLRepOps(lhs: Rep[SPL]): SPLOps = new SPLOps(lhs)
  class SPLOps(x: Rep[SPL]) {
    def tensor(y: Rep[SPL]) = infix_tensor(x, y)
    def compose(y: Rep[SPL]) = infix_compose(x, y)
  }
  def infix_tensor(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
  def infix_compose(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
}


trait SPL_Exp extends SPL_Base with BaseExp with InternalFunctionsExp {
  def getSPLSize(x: Exp[SPL]): Int = {
    val otp = getTP(x)
    val rhs = otp.map(t => t.rhs)
    rhs match {
      case Some(spl: SPLDef) => spl.size
      case Some(_) => { assert(false, "Seems we are trying to extract SPL size info from a node that is not an SPL object - namely: " + otp); ???}
      case _ => { assert(false, "apparently the Exp is not part of the internal IR - how did you manage this?"); ???}
    }
  }

  def createTensorMeta(x: Exp[SPL], y: Exp[SPL]): Int = getSPLSize(x) * getSPLSize(y)
  def createComposeMeta(x: Exp[SPL], y: Exp[SPL]): Int = getSPLSize(x)

  class SPLDef(val size: Int) extends Def[SPL]

  case class Tensor(x: Exp[SPL], y: Exp[SPL], override val size: Int) extends SPLDef(size)
  case class Compose(x: Exp[SPL], y: Exp[SPL], override val size: Int) extends SPLDef(size)

  def infix_tensor(x: Exp[SPL], y: Exp[SPL]):Exp[SPL] = Tensor(x, y, createTensorMeta(x,y))
  def infix_compose(x: Exp[SPL], y: Exp[SPL]): Exp[SPL] = Compose(x, y, createComposeMeta(x,y))
}

class SPL_DSL extends SPL_Exp