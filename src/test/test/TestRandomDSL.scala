package test


import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.lms.internal._
import scala.lms.ops._

/*
object RandomClass extends GenRandomBooleanOps{
 def apply(): Gen[RandomClass] = {
  for {
   i <- randomConst()
  }
   yield{
    new RandomClass {
     override def f(): Rep[Boolean] = i
    }
   }
 }
}*/

class MRandomClass extends BooleanOpsExp
with PurePrimitiveOpsExp
with ImplicitOpsExp
with InternalFunctionsExp
with GenRandomBooleanOps
with GenRandomPrimitiveOps{

 type avail = Boolean


 def rNrArgs = Gen.chooseNum(0,24)


 //what types we want to operate on?

 //what are the operations accessible given the set of types



 def genF1() = {
  for {
   inputs <- genF
   avOps <- filterOps(inputs.toSet)
  } yield{

  }
 }

 def



 def genF() = {
  for {
   nrArgs <- rNrArgs
   inputs <- genTypes(nrArgs)
  } yield {
    //val x = filterOps(inputs.toSet)
    //println(x)
   inputs
  }
 }


 def g(): Gen[Char] = Gen.alphaLowerChar
}

import org.scalacheck.Gen.{choose, numChar, alphaChar}
import org.scalatest._
import prop._

class TestRandomDSL extends PropSpec with PropertyChecks {


 println("hello")
 val x = new MRandomClass


 val gen = x.genF()

 println(gen.sample.get)

 /*property("mult"){

  forAll(gen) {
   cha => {
    println(cha)
    true
   }
  }

 }*/
}
