/**
 * Georg Ofenbeck
 First created:
 * Date: 15/04/2015
 * Time: 16:51 
 */

import ch.ethz.spirals.datatypes.ArrayOpsUnstaged.UnstagedArray
import ch.ethz.spirals.datatypes.DataTypeFactories.SplitComplexArray
import ch.ethz.spirals.datatypes.ElementOpsUnstaged.Complex
import org.scalatest.FunSpec

import ch.ethz.spirals.datatypes._

class TestFactory extends FunSpec {
 describe("Debug!") {



  import UnstagedImplicitOps._
  val bla = new SplitComplexArray[NoRep,Array,NoRep,Double](10)
  bla.update(1,Complex(1.0,1.0))

  println("seems to work")
 }
}
