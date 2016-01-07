package RandomTesting

import org.scalacheck._
import Gen._

import scala.lms.ops.BooleanOpsExp




trait GenRandomBooleanOps extends GenRandomOps{
 this: BooleanOpsExp =>



  val negate: Op = {

    val f: Function1[Vector[SoV[NoRep,_]],Vector[SoV[NoRep,_]]] = (x: Vector[SoV[NoRep,_]]) => {
      val t = x.head.asInstanceOf[Boolean]
      Vector(SoV[NoRep,Boolean](!t,Tag(manifest[Boolean])))
    }

    val sf: Function1[Vector[SoV[Rep,_]],Vector[SoV[Rep,_]]] = (x: Vector[SoV[Rep,_]]) => {
      val t = x.head.asInstanceOf[Rep[Boolean]]
      Vector(SoV(boolean_negate(t),Tag(manifest[Boolean])))
    }



    Op("boolean_negate",Vector(Tag(manifest[Boolean])),Vector(Tag(manifest[Boolean])),f,sf, None)
  }

 //val or = Map(Vector("Boolean","Boolean") -> Vector("Boolean"))
 //val and = Map(Vector("Boolean","Boolean") -> Vector("Boolean"))

 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set(Tag(manifest[Boolean]))))
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
