package ch.ethz.spirals.cgo2015

import virtualization.lms.common._

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

trait SPL_Exp extends SPL_Base with BaseExp with PureFunctionsExp{
  case class Tensor(x: Exp[SPL], y: Exp[SPL]) extends Def[SPL]
  case class Compose(x: Exp[SPL], y: Exp[SPL]) extends Def[SPL]
  def infix_tensor(x: Exp[SPL], y: Exp[SPL]) = Tensor(x, y)
  def infix_compose(x: Exp[SPL], y: Exp[SPL]) = Compose(x, y)
}

class SPL_DSL extends SPL_Exp