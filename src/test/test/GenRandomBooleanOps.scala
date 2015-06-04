package test

import org.scalacheck._
import Gen._

import scala.lms.ops.BooleanOpsExp




trait GenRandomBooleanOps extends GenRandomOps{
 this: BooleanOpsExp =>


 val f: PartialFunction[Any, Any] = {    case x: Boolean => !x  }

 val negate: AvailOps = {
  val f: Function1[Boolean,Boolean] = (x: Boolean) => !x
  val op = OpDescription(Vector("Boolean"),Vector("Boolean"),f,f)
  Map(Set("Boolean") -> ("boolean_negate", op))
 }

 //val or = Map(Vector("Boolean","Boolean") -> Vector("Boolean"))
 //val and = Map(Vector("Boolean","Boolean") -> Vector("Boolean"))

 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes + (Set("Boolean")))
 }


 override def ops(map: AvailOps) = {

  //map ++ negate ++ or ++ and
  super.ops(map ++ negate)
 }


 def randomConst(): Gen[Exp[Boolean]] = {
  for { choice <- Gen.oneOf(true,false)} yield Const(choice)
 }

 def randomInstr(x: Exp[Boolean], y: Exp[Boolean]): Gen[Exp[Boolean]] ={
  for { choice <-
        Gen.oneOf(
         boolean_and(x,y),
         boolean_or(x,y)
        )} yield choice
 }


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
