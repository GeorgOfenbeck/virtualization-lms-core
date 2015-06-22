package test

import org.scalacheck._
import Gen._

import scala.lms.ops._

trait GenRandomIFThenElse extends GenRandomOps{
 this: IfThenElseExp =>

 val ifthenelse: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val conditional = x(0).asInstanceOf[Boolean]
   val ifbranch = x(1).asInstanceOf[Any]
   val elsebranch = x(2).asInstanceOf[Any]

   val res = if (conditional) ifbranch else elsebranch
   Vector(res)
  }

  val sf: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val conditional = x(0).asInstanceOf[Rep[Boolean]]
   val ifbranch = x(1).asInstanceOf[Rep[Any]]
   val elsebranch = x(2).asInstanceOf[Rep[Any]]
   //val res = ifThenElse(conditional,ifbranch,elsebranch)
   Vector(ifbranch) //RF!
  }
  val op = OpDescription(Vector(manifest[Boolean],manifest[Wildcard1],manifest[Wildcard1]),Vector(manifest[Wildcard1]),f,sf)
  Op("ifthenelse", op)
 }


 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set(manifest[Boolean])))
 }

 override def ops(map: AvailOps) = {
  super.ops(registerOp(ifthenelse,map))
 }

}
