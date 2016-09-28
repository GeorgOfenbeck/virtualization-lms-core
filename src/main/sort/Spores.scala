package sort

import scala.spores._
import scala.pickling._
import scala.pickling.Defaults._
import scala.pickling._
import scala.pickling.json._
import SporePicklers._
case class Helper(name: String)
/**
  * Created by rayda on 07-Sep-16.
  */
object Spores extends App{


  val helper = Helper("the helper")

  val fun: Spore[Int, Unit] = spore {
    val h = helper
    (x: Int) => {
      val result = x + " " + h.toString
      println("The result is: " + result)
    }
  }

  val res = fun.pickle

  println(res.value)



}
