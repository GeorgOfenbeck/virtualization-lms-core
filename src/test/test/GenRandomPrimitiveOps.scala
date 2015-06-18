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
   val l = x.head.asInstanceOf[Rep[Int]]
   val r = x.tail.head.asInstanceOf[Rep[Int]]
   Vector(int_plus(l,r))
  }
  val op = OpDescription(Vector(manifest[Int],manifest[Int]),Vector(manifest[Int]),f,sf)
  val con: GenTypes = manifest[Int]
  val key: Set[GenTypes] = Set(con)
  Map(key -> Op("int_plus", op))
 }



 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set(manifest[Int])))
 }


 override def ops(map: AvailOps) = {
  super.ops(map ++ plus)
 }


}
