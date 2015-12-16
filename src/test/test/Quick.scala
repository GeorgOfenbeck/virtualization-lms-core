
import java.io.{PrintWriter, StringWriter}

import org.scalacheck.Shrink

import scala.lms.targets.scalalike._
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util
/**
  * Georg Ofenbeck
 First created:
  * Date: 15/12/2015
  * Time: 18:20 
  */
object Quick extends org.scalacheck.Properties("Random Testing"){
 import org.scalacheck.{Gen, Prop, Arbitrary}

 val Primes_low =  List(   2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
  59, 61, 67, 71, 73, 79, 83, 89, 97,101,103,107,109,113,127,131,137,139,
  149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,
  241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,
  353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,
  461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,
  587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,
  691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,
  823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,
  947,953,967,971,977,983,991,997 )

 case class MyStuff (val x: Vector[Int])

 def genMyStuff(): Gen[MyStuff] = {
  for {
   i <- Gen.containerOf[Vector,Int](Gen.choose(900,2000))
  } yield MyStuff(i)
 }

 implicit val shrinkStuff: Shrink[MyStuff] = Shrink(
  {
   case s: MyStuff => {
    println("shrinking" + s.x)
    if (!s.x.isEmpty) {
     import Shrink._
     //val t = shrink(MyStuff(secondhalf)) map (t => if (t.x.isEmpty) MyStuff(Vector.empty) else MyStuff(firsthalf ++ t.x))
     Stream.concat(
      Stream(MyStuff(s.x.splitAt(s.x.size / 2)._1)),
      shrink(MyStuff(s.x.splitAt(s.x.size / 2)._1))
     )
    }
    else
     Stream.empty
   }
 })

 property("my prop") = {
  Prop.forAll(genMyStuff()) {
   stuff =>
      println(stuff.x)
      stuff.x.filter(p => Primes_low.contains(p)).isEmpty
  }
 }



}
