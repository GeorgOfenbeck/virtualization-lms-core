package test
import org.scalacheck.Gen.{choose, numChar, alphaChar}
import org.scalacheck.Shrink
import org.scalatest._
import prop._


object ShrinkDebug extends org.scalacheck.Properties("MySpec2") {
 import org.scalacheck.{Gen, Prop, Arbitrary}

 case class Single(x: Int)
 case class Duo(list: List[Int])

 def genSingle(): Gen[Single] = {
  for {
   i <- org.scalacheck.Gen.chooseNum(1,10)
  } yield Single(i)
 }

 def genDuo(x: Single): Gen[Duo] = {
  for {
   i <- org.scalacheck.Gen.chooseNum(8,20)
   l <- org.scalacheck.Gen.listOfN(100,i)
  } yield Duo(l)
  }


 implicit def getshrinker(s: Single): Shrink[Duo] = {
  bla.shrinkDuo
 }

 import org.scalacheck.Shrink.shrink

 object bla {
  implicit val shrinkDuo: Shrink[Duo] = Shrink({
   case Duo(list) => {
    println("shrinking")
    shrink(list) map (x => Duo(x))
   }
  })
 }




 property("my prop") = {
  Prop.forAll(genSingle){
   single => {
    Prop.forAll(genDuo(single)){
     duo => {
      duo.list.filter(ele => ele > single.x).isEmpty
     }
    }

    /*Prop.forAll(org.scalacheck.Gen.listOfN(100,single.x)){
     duo => {
      println(duo)
      duo.filter(ele => single.x >= ele ).isEmpty
     }
    }*/
   }
  }


 }

}
