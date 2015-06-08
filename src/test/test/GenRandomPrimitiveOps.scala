package test

import org.scalacheck._
import Gen._

import scala.lms.ops._


trait GenRandomPrimitiveOps extends GenRandomOps{
 this: PurePrimitiveOpsExp =>


 val plus: AvailOps = {
  val f: Function1[(Int,Int),Int] = (pair: (Int,Int)) => pair._1 + pair._2
  val op = OpDescription(Vector("Int", "Int"),Vector("Int"),f,f)
  Map(Set("Int") -> ("int_plus",op))
 }

 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set("Int")))
 }


 override def ops(map: AvailOps) = {
  //map ++ negate ++ or ++ and
  super.ops(map ++ plus)
 }

}
