package test

import org.scalacheck._
import Gen._

import scala.lms.ops.BooleanOpsExp




trait GenRandomBooleanOps extends GenRandomOps{
 this: BooleanOpsExp =>



  val negate: Op = {
    val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
      val t = x.head.asInstanceOf[Boolean]
      Vector(!t)
    }
    val sf: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
      val t = x.head.asInstanceOf[Rep[Boolean]]
      Vector(boolean_negate(t))
    }
    val op = OpDescription(Vector(manifest[Boolean]),Vector(manifest[Boolean]),f,sf, None)
    Op("boolean_negate", op)
  }

 //val or = Map(Vector("Boolean","Boolean") -> Vector("Boolean"))
 //val and = Map(Vector("Boolean","Boolean") -> Vector("Boolean"))

 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set(manifest[Boolean])))
 }

 override def ops(map: AvailOps) = {
  super.ops(registerOp(negate,map))
 }

 /*def randomInstr(x: Exp[Boolean], y: Exp[Boolean]): Gen[Exp[Boolean]] ={
  for { choice <-
        Gen.oneOf(
         boolean_and(x,y),
         boolean_or(x,y)
        )} yield choice
 }*/


 /*def randomInstr(): Gen[(Exp[Boolean],Exp[Boolean]) => Exp[Boolean]] = {
   for {
     choice <- Gen.choose(1,3)
   } yield{
     choice match {
       case 1 =>
     }
   }
 }*/




}
