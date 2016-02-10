package RandomTesting

import org.scalacheck._
import Gen._

import scala.lms.ops._


trait GenRandomPrimitiveOps extends GenRandomOps{
 this: PurePrimitiveOpsExp =>

 val int_plus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Int]
   val r = x.tail.head.asInstanceOf[Int]
   Vector(l+r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Int]]
   val r = x.tail.head.asInstanceOf[Rep[Int]]
   Vector(int_plus(l,r))
  }
  Op("int_plus", Vector(Tag(manifest[Int]),Tag(manifest[Int])),Vector(Tag(manifest[Int])),f,sf, None)
 }

 val int_minus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Int]
   val r = x.tail.head.asInstanceOf[Int]
   Vector(l-r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Int]]
   val r = x.tail.head.asInstanceOf[Rep[Int]]
   Vector(int_minus(l,r))
  }
  Op("int_minus", Vector(Tag(manifest[Int]),Tag(manifest[Int])),Vector(Tag(manifest[Int])),f,sf, None)
 }

 val int_times: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Int]
   val r = x.tail.head.asInstanceOf[Int]
   Vector(l*r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Int]]
   val r = x.tail.head.asInstanceOf[Rep[Int]]
   Vector(int_times(l,r))
  }
  Op("int_times", Vector(Tag(manifest[Int]),Tag(manifest[Int])),Vector(Tag(manifest[Int])),f,sf, None)
 }

 val int_divide: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Int]
   val r = x.tail.head.asInstanceOf[Int]
   Vector(l/r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Int]]
   val r = x.tail.head.asInstanceOf[Rep[Int]]
   Vector(int_divide(l,r))
  }
  Op("int_divide", Vector(Tag(manifest[Int]),Tag(manifest[Int])),Vector(Tag(manifest[Int])),f,sf, None)
 }


 val long_plus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Long]
   val r = x.tail.head.asInstanceOf[Long]
   Vector(l+r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Long]]
   val r = x.tail.head.asInstanceOf[Rep[Long]]
   Vector(long_plus(l,r))
  }
  Op("long_plus", Vector(Tag(manifest[Long]),Tag(manifest[Long])),Vector(Tag(manifest[Long])),f,sf, None)
 }

 val long_minus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Long]
   val r = x.tail.head.asInstanceOf[Long]
   Vector(l-r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Long]]
   val r = x.tail.head.asInstanceOf[Rep[Long]]
   Vector(long_minus(l,r))
  }
  Op("long_minus", Vector(Tag(manifest[Long]),Tag(manifest[Long])),Vector(Tag(manifest[Long])),f,sf, None)
 }

 
 
 val long_times: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Long]
   val r = x.tail.head.asInstanceOf[Long]
   Vector(l*r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Long]]
   val r = x.tail.head.asInstanceOf[Rep[Long]]
   Vector(long_times(l,r))
  }
  Op("long_times", Vector(Tag(manifest[Long]),Tag(manifest[Long])),Vector(Tag(manifest[Long])),f,sf, None)
 }

 val long_divide: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Long]
   val r = x.tail.head.asInstanceOf[Long]
   Vector(l/r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Long]]
   val r = x.tail.head.asInstanceOf[Rep[Long]]
   Vector(long_divide(l,r))
  }
  Op("long_divide", Vector(Tag(manifest[Long]),Tag(manifest[Long])),Vector(Tag(manifest[Long])),f,sf, None)
 }


 val float_plus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Float]
   val r = x.tail.head.asInstanceOf[Float]
   Vector(l+r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Float]]
   val r = x.tail.head.asInstanceOf[Rep[Float]]
   Vector(float_plus(l,r))
  }
  Op("float_plus", Vector(Tag(manifest[Float]),Tag(manifest[Float])),Vector(Tag(manifest[Float])),f,sf, None)
 }

 val float_minus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Float]
   val r = x.tail.head.asInstanceOf[Float]
   Vector(l-r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Float]]
   val r = x.tail.head.asInstanceOf[Rep[Float]]
   Vector(float_minus(l,r))
  }
  Op("float_minus", Vector(Tag(manifest[Float]),Tag(manifest[Float])),Vector(Tag(manifest[Float])),f,sf, None)
 }



 val float_times: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Float]
   val r = x.tail.head.asInstanceOf[Float]
   Vector(l*r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Float]]
   val r = x.tail.head.asInstanceOf[Rep[Float]]
   Vector(float_times(l,r))
  }
  Op("float_times", Vector(Tag(manifest[Float]),Tag(manifest[Float])),Vector(Tag(manifest[Float])),f,sf, None)
 }

 val float_divide: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Float]
   val r = x.tail.head.asInstanceOf[Float]
   Vector(l/r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Float]]
   val r = x.tail.head.asInstanceOf[Rep[Float]]
   Vector(float_divide(l,r))
  }
  Op("float_divide", Vector(Tag(manifest[Float]),Tag(manifest[Float])),Vector(Tag(manifest[Float])),f,sf, None)
 }


 val double_plus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Double]
   val r = x.tail.head.asInstanceOf[Double]
   Vector(l+r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Double]]
   val r = x.tail.head.asInstanceOf[Rep[Double]]
   Vector(double_plus(l,r))
  }
  Op("double_plus", Vector(Tag(manifest[Double]),Tag(manifest[Double])),Vector(Tag(manifest[Double])),f,sf, None)
 }

 val double_minus: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Double]
   val r = x.tail.head.asInstanceOf[Double]
   Vector(l-r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Double]]
   val r = x.tail.head.asInstanceOf[Rep[Double]]
   Vector(double_minus(l,r))
  }
  Op("double_minus", Vector(Tag(manifest[Double]),Tag(manifest[Double])),Vector(Tag(manifest[Double])),f,sf, None)
 }



 val double_times: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Double]
   val r = x.tail.head.asInstanceOf[Double]
   Vector(l*r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Double]]
   val r = x.tail.head.asInstanceOf[Rep[Double]]
   Vector(double_times(l,r))
  }
  Op("double_times", Vector(Tag(manifest[Double]),Tag(manifest[Double])),Vector(Tag(manifest[Double])),f,sf, None)
 }

 val double_divide: Op = {
  val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
   val l = x.head.asInstanceOf[Double]
   val r = x.tail.head.asInstanceOf[Double]
   Vector(l/r)
  }
  val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
   val l = x.head.asInstanceOf[Rep[Double]]
   val r = x.tail.head.asInstanceOf[Rep[Double]]
   Vector(double_divide(l,r))
  }
  Op("double_divide", Vector(Tag(manifest[Double]),Tag(manifest[Double])),Vector(Tag(manifest[Double])),f,sf, None)
 }
 


 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes +
    (Set(Tag(manifest[Int]))) +
    (Set(Tag(manifest[Long]))) +
    (Set(Tag(manifest[Double]))) +
    (Set(Tag(manifest[Float])))
  )
 }


 override def ops(map: AvailOps) = {
  val int = registerOp(int_minus,
   registerOp(int_plus,
     registerOp(int_times, map
      //registerOp(int_divide, map)
     )
   )
  )
  val long = registerOp(long_minus,
   registerOp(long_plus,
    registerOp(long_times, int
     //registerOp(long_divide, map)
    )
   )
  )
  val float = registerOp(float_minus,
   registerOp(float_plus,
    registerOp(float_times, long
     //registerOp(float_divide, map)
    )
   )
  )
  val double = registerOp(double_minus,
   registerOp(double_plus,
    registerOp(double_times, float
     //registerOp(double_divide, map)
    )
   )
  )
  super.ops(double)
 }


}
