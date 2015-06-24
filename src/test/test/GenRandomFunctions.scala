package test

import org.scalacheck._


import scala.lms.BaseExp
import scala.lms.ops._


trait GenRandomFunctions extends GenRandomOps{
 this: BaseExp  =>

 val internalfunction: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   ???
  }

  val sf: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   ???
  }
  val op = OpDescription(Vector(manifest[Boolean],manifest[Wildcard1],manifest[Wildcard1]),Vector(manifest[Wildcard1]),f,sf)
  Op("internalfunction", op)
 }


 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set(manifest[Boolean])))
 }

 override def ops(map: AvailOps) = {
  super.ops(registerOp(internalfunction,map))
 }

}
