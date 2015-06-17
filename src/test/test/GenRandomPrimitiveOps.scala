package test

import org.scalacheck._
import Gen._

import scala.lms.ops._


trait GenRandomPrimitiveOps extends GenRandomOps{
 this: PurePrimitiveOpsExp =>

 val plus: AvailOps = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Int]
   val r = x.tail.head.asInstanceOf[Int]
   Vector(r+l)
  }
  val sf: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val t = x.head.asInstanceOf[Int]
   Vector(t)
  }
  val op = OpDescription(Vector("Int","Int"),Vector("Int"),f,sf)
  Map(Set("Int") -> Op("boolean_negate", op))
 }



 /*override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set("Int")))
 }


 override def ops(map: AvailOps) = {
  super.ops(map ++ plus)
 }*/


}
