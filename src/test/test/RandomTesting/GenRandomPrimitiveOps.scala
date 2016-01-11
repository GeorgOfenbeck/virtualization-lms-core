package RandomTesting

import org.scalacheck._
import Gen._

import scala.lms.ops._


trait GenRandomPrimitiveOps extends GenRandomOps{
 this: PurePrimitiveOpsExp =>

 val plus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Int]
   val r = x.tail.head.asInstanceOf[Int]
   Vector(r+l)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Int]]
   val r = x.tail.head.asInstanceOf[Rep[Int]]
   Vector(int_plus(l,r))
  }
  Op("int_plus", Vector(Tag(manifest[Int]),Tag(manifest[Int])),Vector(Tag(manifest[Int])),f,sf, None)
 }



 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set(Tag(manifest[Int]))))
 }


 override def ops(map: AvailOps) = {
  super.ops(registerOp(plus,map))
 }


}
