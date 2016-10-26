package Karatsuba

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._

object KaratsubaBench extends org.scalacheck.Properties("Karatsuba") {

  val mykara = new testClass
  val rnd = scala.util.Random
  def convert(before: java.math.BigInteger): MyBigInt = {
    MyBigInt(before.toByteArray)
  }

  //def apply(bitlength: Int, certainty: Int, rnd: scala.util.Random)

  def mymult(a: MyBigInt, b: MyBigInt): MyBigInt = {
    mykara.apply(a,b,a.mag.length,b.mag.length,a.signum,b.signum)
  }

  def chooseBigInt: Gen[BigInt] =
    sized((s: Int) => choose(-s, s)) map (x => BigInt(x))

  def chooseReallyBigInt: Gen[BigInt] = for {
    bi <- chooseBigInt
    n <- choose(32, 128)
  } yield bi << n

  def chooseKaraBigInt: Gen[BigInt] = for {

    bi <- choose(3000,4000)
    n <- BigInt.apply(bi,rnd)
  } yield n

  property("ByteRep Same") = forAll(chooseReallyBigInt) { l =>

    val ba = l.toByteArray
    val mbi = convert(l.bigInteger)
    val mbiba = mbi.toByteArray()
    ba.corresponds(mbiba) {
      _ == _
    }
  }


  property("ByteRep Same") = forAll(chooseBigInt) { l =>
    val plus3 = l + 3
    val check = l * plus3

    val mbi = convert(l.bigInteger)
    val mbi2 = convert(plus3.bigInteger)

    val mres = mbi.multiply(mbi2)
    val myres = mymult(mbi,mbi2)

    mres.toByteArray().corresponds(check.toByteArray) {
      _ == _
    } && mres.toByteArray().corresponds(myres.toByteArray()) {
      _ == _
    }
  }

  property("ByteRep Same large") = forAll(chooseReallyBigInt) { l =>
    val plus3 = l + 3
    val check = l * plus3

    val mbi = convert(l.bigInteger)
    val mbi2 = convert(plus3.bigInteger)

    val mres = mbi.multiply(mbi2)
    val myres = mymult(mbi,mbi2)

    mres.toByteArray().corresponds(check.toByteArray) {
      _ == _
    }&& mres.toByteArray().corresponds(myres.toByteArray()) {
      _ == _
    }
  }

  property("ByteRep Kara large") = forAll(chooseKaraBigInt) { l =>
    val plus3 = l + 3
    val check = l * plus3

    val mbi = convert(l.bigInteger)
    val mbi2 = convert(plus3.bigInteger)

    val mres = mbi.multiply(mbi2)
    val myres = mres
    //val myres = mymult(mbi,mbi2)

    mres.toByteArray().corresponds(check.toByteArray) {
      _ == _
    }&& mres.toByteArray().corresponds(myres.toByteArray()) {
      _ == _
    }
  }
}