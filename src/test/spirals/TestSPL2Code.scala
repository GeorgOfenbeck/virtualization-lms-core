
import ch.ethz.spirals._
import org.scalatest.FunSpec
import rewrites._
import dsls._


import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck._


class CheckSPL2Code extends Properties("SPL") {
 implicit val shrinkBreakDown: Shrink[BreakDown] = Shrink {
  bd: BreakDown =>
   val children: List[BreakDown] = bd.applied_bd.map(resolved => resolved.children).getOrElse(List())
   children.filter(bd => bd.nt.size > 2).toStream
 }
 //property("matrix") = forAll(BreakdownRules.genRandomRuleTree(DFT(6,1))) {


 def checkCode(bd: BreakDown): Boolean = {
  val allowed_error: Double = 1E-6
  val dsl = new SPL_DSL
  val spltransformation = new BreakDown2SPL_DSL with SPL_DSL2Code {
   val IR: SPL_Exp = dsl
  }
  val bydef = bd.nt.toMatrix()



  import ch.ethz.spirals.datatypes._
  import ch.ethz.spirals.datatypes.DataTypeFactories._
  import UnstagedImplicitOps._


  val size = bd.nt.size

  val bla = new SplitComplexArray[NoRep, Array, NoRep, Double](size)

  val inv = new Array[org.apache.commons.math3.complex.Complex](size)
  val ele = bla.apply(0)
  for (i <- 0 until size) {
   val newele  = ele.copy(_re = i.toDouble+1, _im = i.toDouble +1.5)
   inv.update(i,new org.apache.commons.math3.complex.Complex(i.toDouble+1, i.toDouble + 1.5))
   bla.update(i,newele)
  }

  import org.apache.commons.math3.linear._
  import org.apache.commons.math3.complex._
  val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), size, 1)
  m.setColumn(0,inv)

  val resbydef = bydef.multiply(m)
  //MathUtilities.printm(resbydef)

  val pcall = spltransformation.SPL2Code(spltransformation.bd2spl(bd),bla)
  val res = pcall(bla)

  val outv = new Array[org.apache.commons.math3.complex.Complex](size)
  for (i <- 0 until size) {
   outv.update(i,new org.apache.commons.math3.complex.Complex(res(i)._re, res(i)._im))
  }
  m.setColumn(0,outv)
  //println("---------------------------------------")
  //MathUtilities.printm(m)

  val errors = for {
   j <- 0 until outv.length} yield outv(j).subtract(resbydef.getEntry(j,0))

  val filtered = errors.filter( error => (error.getReal.abs > allowed_error || error.getImaginary.abs > allowed_error))
  filtered.isEmpty

 }
}











object TestSPL2Code extends org.scalacheck.Properties("WHT") {
 property("WHT code test") = {
  val x = new CheckSPL2Code
  forAll(BreakdownRules.genRandomWHTRuleTree) (x.checkCode)
 }
}



/*
import org.scalatest.FunSpec

class dfadsfFunSpec extends FunSpec {
 describe("Debug!") {
  val x = new CheckSPL2Code
  val bd = BreakdownRules.genRandomWHTRuleTree(4).sample.get
  x.checkCode(bd)
 }

}
*/