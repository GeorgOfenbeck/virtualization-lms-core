package SpiralSThesisGui

import scala.util.Random
import SpiralSThesis._
/**
  * Created by rayda on 02-Feb-17.
  */

import java.io.{PrintWriter, StringWriter}

import org.scalacheck.Shrink

import scala.lms.targets.scalalike._
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util
object TestVariants  extends org.scalacheck.Properties("Random Testing"){
  import org.scalacheck.{Gen, Prop, Arbitrary}

  var defradix: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map(
    4 -> 2
    , 8 -> 2
    , 16 -> 2
    , 32 -> 2
    , 64 -> 2
    , 128 -> 2
    , 256 -> 2
    , 512 -> 2
    , 1024 -> 2
    , 2048 -> 2
    , 4096 -> 2
    , 8192 -> 2
    , 16384 -> 2
    , 32768 -> 2
    , 65536 -> 2
  ).withDefaultValue(256)


  def randomRadix(): Gen[Map[Int,Int]] = {
    val rand = new Random()
    val m = (2 until 16).foldLeft(Map.empty[Int,Int]){
      (acc,ele) => {
        val num = Math.pow(2,ele).toInt
        val divpair = Bla.DivisorPairs(num)
        val choice = rand.nextInt(divpair.size)
        acc + (num -> divpair(choice)._1)
      }
    }
    m.withDefaultValue(256)
  }

  property("fully dynamic") = {
    Prop.forAll(randomRadix()) {
      radix => {
        val dsl = new CorewGlue(testsize = 4, //2^testsize
          radix_choice = radix,
          static_size = None,
          interleaved = true,
          thread = false,
          base_default = 1,
          twid_inline = false,
          twid_default_precomp = false,
          validate = true,
          inplace = false
        )
        dsl.codeexport("F:\\Phd\\git\\code\\SpiralSTarget\\src\\main\\Test.scala")
        dsl.graphexport()
        val f = dsl.compile()
        val perf = f();
        true
      }

    }


  }

}
