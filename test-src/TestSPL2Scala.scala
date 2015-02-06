package ch.ethz.spirals.cgo2015


import org.apache.commons.math3.complex.{ComplexField, Complex}
import org.apache.commons.math3.linear.BlockFieldMatrix
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck._


import scala.virtualization.lms.common.ReifyPure





class CheckSPL2Scala extends Properties("SPL") {
  implicit val shrinkBreakDown: Shrink[BreakDown] = Shrink {
    bd: BreakDown =>
      val children: List[BreakDown] = bd.applied_bd.map( resolved => resolved.children).getOrElse(List())
      children.filter(bd => bd.nt.size > 2).toStream
  }
  //property("matrix") = forAll(BreakdownRules.genRandomRuleTree(DFT(6,1))) {
  def checkMatrixvsScala (bd: BreakDown): Boolean = {
    val allowed_error:Double = 1E-6

    val bydef = bd.nt.toMatrix()


    val size = bd.nt.toMatrix().getColumnDimension()
    val testin = new BlockFieldMatrix[Complex](ComplexField.getInstance(), size, 1)
    for (i <- 0 until 1) {
      val x = new Array[Complex](size)
      for (j <- 0 until size) {
          x.update(j, new Complex(j, j))
      }
      testin.setColumn(i,x)
    }
    val bydefres = bydef.multiply(testin)



    val dsl = new SPL_DSL
    val spltransformation = new BreakDown2SPL_DSL with SPL_DSL2Scala {
      val IR: SPL_Exp = dsl
    }
    val (byscaladecomp, finalnode) = spltransformation.SPL2Scala(spltransformation.bd2spl(bd))
    val finalfunction = byscaladecomp(finalnode)

    val inforfunction = (for (i <- 0 until size) yield MyComplex(i,i)).toVector


    val res = finalfunction(inforfunction)

    val scalaout = new BlockFieldMatrix[Complex](ComplexField.getInstance(), size, 1)
    for (i <- 0 until 1) {
      val x = new Array[Complex](size)
      for (j <- 0 until size) {
        x.update(j, new Complex(res(j).re, res(j).im))
      }
      scalaout.setColumn(i,x)
    }
    println(" check")

    val ret = if (bydefres.getColumnDimension() == scalaout.getColumnDimension() && bydefres.getRowDimension == scalaout.getRowDimension) {

      val errors = for {
        i <- 0 until bydefres.getRowDimension
        j <- 0 until bydefres.getColumnDimension
      } yield bydefres.getEntry(i,j).subtract(scalaout.getEntry(i,j))

      val filtered = errors.filter( error => (error.getReal > allowed_error || error.getImaginary > allowed_error))
      filtered.isEmpty

    }
    else
      false

    ret
  }
}

import org.scalatest.FunSpec

class TestSPL2Scala extends FunSpec {
  describe("Debug me") {
    val x = new CheckSPL2Scala
    //forAll(BreakdownRules.genRandomWHTRuleTree) (x.checkMatrixvsScala)
    BreakdownRules.genRandomWHTRuleTree(512).sample.map ( t => x.checkMatrixvsScala(t))
  }
}