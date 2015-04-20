
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

  val bla = new SplitComplexArray[NoRep, Array, NoRep, Double](4)
  val pcall = spltransformation.SPL2Code(spltransformation.bd2spl(bd),bla)
  val res = pcall(bla)
  println(res)

  true
 }
}









/*

object ValidateSPL extends org.scalacheck.Properties("WHT") {
 property("WHT code test") = {
  val x = new CheckSPL2Matrix
  forAll(BreakdownRules.genRandomWHTRuleTree) (x.checkMatrix)
 }
}
*/



import org.scalatest.FunSpec

class dfadsfFunSpec extends FunSpec {
 describe("Debug!") {
  val x = new CheckSPL2Code
  val bd = BreakdownRules.genRandomWHTRuleTree(4).sample.get
  x.checkCode(bd)
 }

}
