package test

import scala.annotation.tailrec

/**
 * Georg Ofenbeck
 First created:
 * Date: 01/06/2015
 * Time: 12:34 
 */
trait GenRandomOps {



 type GenTypes = String
 type Op = (String, OpDescription)
 type AvailOps = Map[Set[GenTypes], Op]
 type AvailUniqueTypes = Set[GenTypes]
 type AvailTypeTuples = Set[AvailUniqueTypes]

 case class OpDescription(args: Vector[GenTypes],
                          returns:Vector[GenTypes],
                          rf: _ => _,
                          sf: _ => _)

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

 def findAllIndex(targettype: GenTypes, options: Vector[GenTypes]): Vector[Int] = {
  options.zipWithIndex.filter(p => p._1 == targettype).map(e => e._2)
 }

 def chooseRandomParam(targettype: GenTypes, options: Vector[GenTypes]): Gen[Int] = {
  val tmatches = findAllIndex(targettype,options)
  Gen.oneOf(tmatches).map(choice => choice)
 }

 def chooseParams(avInputs: Vector[GenTypes], op: Op): Gen[Vector[Int]] = {
  val mapping = op._2.args.foldLeft(Vector.empty[Gen[Int]]) {
   (acc,ele) => { acc :+ chooseRandomParam(ele,avInputs) }}
  sequence(mapping)
  ???
 }


 def genNextNode(avInputs: Vector[GenTypes], op: Op): Gen[Vector[GenTypes]] = {
  val randomparas = chooseParams(avInputs,op)

/*  randomparas.map(x => {
   op._2.rf()
  })*/
  ???
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
