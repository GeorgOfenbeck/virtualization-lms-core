package test

/**
 * Georg Ofenbeck
 First created:
 * Date: 01/06/2015
 * Time: 12:34 
 */
trait GenRandomOps {


 type GenTypes = String
 type Op = (String, Vector[GenTypes], Vector[GenTypes],PartialFunction[Any,Any],PartialFunction[Any,Any])
 type AvailOps = Map[Set[GenTypes], Op]
 type AvailUniqueTypes = Set[GenTypes]
 type AvailTypeTuples = Set[AvailUniqueTypes]


 def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = availTypes
 def ops (availOps: AvailOps): AvailOps = availOps

 import org.scalacheck.Gen._
 import org.scalacheck.Prop._
 import org.scalacheck._

 lazy val allops = ops(Map.empty)


 def filterOps(availTypes: AvailUniqueTypes): AvailOps = {
  val currops = allops.filter( x => x._1.subsetOf(availTypes))
  currops
 }


 def chooseParams(avInputs: Vector[GenTypes], op: Op) = {
  //continue here:
  // pick random inputs for each slot of op
  // call the op
  //op._2
 }


 def genTypes(max: Int): Gen[Vector[GenTypes]] = {
  for {
   longlist <- Gen.listOfN(max,genTypeTuple())
  } yield {
   val cc = longlist.foldLeft(Vector.empty[GenTypes])( (acc,ele) => {
    if (acc.length + ele.size > max)
     acc
    else {
     val t1: Vector[GenTypes] = ele.toVector
     val t: Vector[GenTypes] = acc ++ t1
     t
    }
   })
   cc
  }
 }

 def genTypeTuple(): Gen[Set[GenTypes]] = {
  val all = supported_types(Set.empty)

  val t = for {
   i <- Gen.oneOf(all.toSeq)
  } yield i
  t
 }


}
