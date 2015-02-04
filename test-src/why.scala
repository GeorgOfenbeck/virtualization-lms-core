
package ch.ethz.spirals.cgo2015

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck._

import scala.virtualization.lms.common.ReifyPure


class CheckSPL extends Properties("SPL") {
  implicit val shrinkBreakDown: Shrink[BreakDown] = Shrink {
    bd: BreakDown =>
      val children: List[BreakDown] = bd.applied_bd.map( resolved => resolved.children).getOrElse(List())
      children.filter(bd => bd.nt.size > 2).toStream
  }
  //property("matrix") = forAll(BreakdownRules.genRandomRuleTree(DFT(6,1))) {
  def checkMatrix (bd: BreakDown): Boolean = {
    val allowed_error:Double = 1E-6
    val dsl = new SPL_DSL
    val spltransformation = new BreakDown2SPL_DSL with SPL_DSL2Mat {
      val IR: SPL_Exp = dsl
    }
    val bydef = bd.nt.toMatrix()

    val bydecomp =spltransformation.SPL2Mat(spltransformation.bd2spl(bd))


    val ret = if (bydef.getColumnDimension() == bydecomp.last.getColumnDimension() && bydef.getRowDimension == bydecomp.last.getRowDimension) {

      val errors = for {
        i <- 0 until bydef.getRowDimension
        j <- 0 until bydef.getColumnDimension
      } yield bydef.getEntry(i,j).subtract(bydecomp.last.getEntry(i,j))

      val filtered = errors.filter( error => (error.getReal > allowed_error || error.getImaginary > allowed_error))
      filtered.isEmpty



    }
    else
      false


    if (!ret && bydef.getColumnDimension() <= 32)
    {

      println("-------------------")
      for (i <- 0 until bydecomp.size) {
        MathUtilities.printm(bydecomp(i))
        println("!")
      }
      println("@")
      MathUtilities.printm(bydef)
      println("diff")
      MathUtilities.printm(bydecomp.last.subtract(bydef))
    }
    ret
  }
}










object ValidateSPL extends org.scalacheck.Properties("WHT") {
  property("WHT matrix test") = {
    val x = new CheckSPL
    forAll(BreakdownRules.genRandomWHTRuleTree) (x.checkMatrix)
  }

}


import org.scalatest.FunSpec

class WHY extends FunSpec {
  describe("Debug") {
    val x = new CheckSPL
    //forAll(BreakdownRules.genRandomWHTRuleTree) (x.checkMatrix)
    BreakdownRules.genRandomWHTRuleTree.sample.map ( t => x.checkMatrix(t))
  }
}