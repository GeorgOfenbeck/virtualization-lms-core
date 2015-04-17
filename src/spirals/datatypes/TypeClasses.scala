package ch.ethz.spirals.datatypes

import scala.collection.IndexedSeqLike
import scala.virtualization.lms.common._



/**
 * =========================================== NumericOps ==================================================
 *
 * NumericOps represent the numeric computations of the primitives. NumericOps require the inner type to
 * hold a Numeric type class.
 * In this context we define 3 distinct cases of numeric computations, on the primitives:
 *
 * 1. NumericNoRepOps  similar to Numeric, performs numeric computation on non-staged code. There is no
 * translation of this computations to any IR, and they are executed directly in Scala.
 * 2. NumericRepOps    represent SISD numeric computations that are translated in C-IR. The execution of this
 * code is delayed, and LMS kicks in.
 * 3. PackedNumericOps represent SIMD numeric computations that are translated into Intrinsics-IR. Similar to
 * NumericRepOps, code execution is staged and LMS kicks in. Note that it might make more
 * sense to move PackedNumericOps into VectorElementOps, however we like to have seamless
 * interface for the standard Numeric operations in both SIMD / SISD case.
 *
 * =========================================================================================================
 */

import scala.reflect.runtime.universe._


trait IdendityTypes{
  type NoRep[T] = T
  type Single[T] = T
}

object NumericOpsUnstaged extends IdendityTypes{

  class NumericNoRepOps[T: TypeTag](implicit val numeric: Numeric[T]) extends NumericOps[NoRep[Single[T]]] {
    //def cast[U: Manifest](x: T) = convert[T, U](x)

    def plus(x: T, y: T) = numeric.plus(x,y)

    def minus(x: T, y: T) = numeric.minus(x,y)

    def times(x: T, y: T) = numeric.times(x,y)

    def fromInt(x: Int): T = numeric.fromInt(x)
  }
}

trait TypeClassesStagedNumericOps extends NumericOpsExp with IdendityTypes{
  object StagedNumericOps {
    class NumericRepOps[T: Numeric : TypeTag] extends NumericOps[Rep[Single[T]]] {
      def plus(x: Rep[T], y: Rep[T]) = numeric_plus[T](x, y)

      def minus(x: Rep[T], y: Rep[T]) = numeric_minus[T](x, y)

      def times(x: Rep[T], y: Rep[T]) = numeric_times[T](x, y)

      def fromInt(x: Int): Rep[T] = ???
    }
  }
}

/**
 * =========================================== ElementOps ==================================================
 *
 * ElementOps represent higher order of computation that correspond to the mathematical notion of elements
 * represented in the IR. ElementOps require a Manifest, so it can be propagated to infer the Manifest of the
 * whole ElementOps type class. Furthermore they require a NumericOps case class, to handle the underlying
 * numerical operations on the primitives. In this case we deal with two different cases of Elements:
 *
 * 1. RealOps    that represent the operations of Real numbers, and
 * 2. ComplexOps that represent the numerical operation of Complex numbers
 *
 * =========================================================================================================
 */
object ElementOpsUnstaged {

  case class Real    [+T] (_re: T)
  case class Complex [+T] (_re: T, _im: T)

  class RealOps[T: TypeTag : NumericOps] extends ElementOps[Real, T] {
    import numeric._
    def plus(x: Real[T], y: Real[T]) = Real(x._re + y._re)

    def minus(x: Real[T], y: Real[T]) = Real(x._re - y._re)

    def times(x: Real[T], y: Real[T]) = Real(x._re * y._re)

    /*def create(v: List[T]) = v.size match {
      //case 0 => Real(implicitly[NumericOps[T]].fromDouble(0))
      case _ => Real(v(0))
    }*/

    //def extract(elem: Real[T]) = List(elem._re)
  }

  class ComplexOps[T: TypeTag : NumericOps] extends ElementOps[Complex, T] {
    import numeric._

    def plus(x: Complex[T], y: Complex[T]) = Complex(x._re + y._re, x._im + y._im)

    def minus(x: Complex[T], y: Complex[T]) = Complex(x._re - y._re, x._im - y._im)

    def times(x: Complex[T], y: Complex[T]) = {
      val m1 = x._re * y._re
      val m2 = x._im * y._im
      val m3 = x._re * y._im
      val m4 = x._im * y._re
      Complex(m1 - m2, m3 + m4)
    }

    /*def create(v: List[T]) = v.size match {
      case 0 => val z = nops.fromDouble(0); Complex(z, z)
      case 1 => val z = nops.fromDouble(0); Complex(v(0), z)
      case _ => Complex(v(0), v(1))
    }*/

    //def extract(elem: Complex[T]) = List(elem._re, elem._im)
  }
}

/* ========================================================================================================= */
/* ========================================= VectorElementOps ============================================== */
/* ========================================================================================================= */
/*

object VectorElementOps {
  class VectorElementOpsSingle[E[_], T: Manifest](implicit eops: ElementOps[E, Rep[Single[T]]])
    extends VectorElementOps[E, Rep, Single, T]() {

    def length() = 1

    def vset1(x: E[Rep[T]])(implicit e: ElementOps[E, Rep[T]]) = x

    def hadd(a: E[Rep[Single[T]]], b: E[Rep[Single[T]]]) = eops.plus(a, b)

    def permute2(a: E[Rep[Single[T]]], b: E[Rep[Single[T]]], mask: Int) = if (mask == 1) a else b
  }

  class VectorElementOpsPacked[E[_], T: Manifest](implicit eops: ElementOps[E, Rep[Packed[T]]])
    extends VectorElementOps[E, Rep, Packed, T]() {

    def length() = codegen.getInstructionSetVectorSize[T]

    def vset1(x: E[Rep[T]])(implicit e: ElementOps[E, Rep[T]]) = eops.create(
      e.extract(x).map(t => infix_vset1[T](t, codegen.getInstructionSetVectorSize[T]))
    )

    def hadd(a: E[Rep[Packed[T]]], b: E[Rep[Packed[T]]]) = eops.create(
      (eops.extract(a) zip eops.extract(b)).map(z => infix_hadd[T](z._1, z._2))
    )

    def permute2(a: E[Rep[Packed[T]]], b: E[Rep[Packed[T]]], mask: Int) = eops.create(
      (eops.extract(a) zip eops.extract(b)).map(z => infix_permute2[T](z._1, z._2, mask))
    )
  }
}
*/

/**
 * ============================================ ArrayOps ===================================================
 *
 * ArrayOps define the data layout of the elements inside the higher-order CVector. The array operations are
 * closely related to the datatype of the CVector, and must have the knowledge of the type primitives of the
 * elements, as well as whether we are dealing with SIMD / SISD instructions. Each array operation is
 * translated into corresponding C-IR call, depending on whether the array is staged, scalar, SIMD or SISD
 * array. The ArrayOps carry Manifest of the primitive, to be able to propagate that information into the
 * underlying C-IR calls. ArrayOps come in 5 flavors:
 *
 * 1. StagedSingleArrayOps Underlying representation of the array, is a staged array. Apply / update methods
 * operate with SISD elements, which are staged. Ops are translated into pure C-IR.
 * 2. ScalarSingleArrayOps Underlying representation of the array is array of SISD elements which are staged.
 * Apply / update methods modify the array direcyly, without delay in code execution.
 * 3. StagedPackedArrayOps Underlying representation of the array is a staged array. Apply / update methods
 * create SIMD instructions that are represented into Intrinsics-IR.
 * 4. PackedScalarArrayOps Underlying representation of the array is a scalar array consisted of staged SIMD
 * data. Apply / update methods operate directly on the array, with no code delay.
 * 5. ScalarPackedArrayOps Underlying representation of the array is a scalar array consited of staged SISD
 * data. Apply / update methods translate to Intrinsics-IR instructions that convert
 * the staged SISD data into staged SIMD data.
 *
 * =========================================================================================================
 *
 */
/*object ArrayOpsUnstaged extends IdendityTypes{
  class UnstagedIndexedSequence[T] (implicit val is: IS[T]) extends ArrayOps[NoRep,IS,NoRep,T] {
    def alloc(s: NoRep[Int]): NoRep[IS[T]] = is.companion.
    def apply(x: NoRep[Array[T]], i: NoRep[Int]): NoRep[T] = IS.
  }

}*/
/*


trait TypeClassesStagedNumericOps extends VectorOpsExp{

  class StagedSingleArrayOps[T] extends ArrayOps[Rep, Rep, Single, NoRep, T] {
    def alloc(s: Rep[Int]): Rep[Array[T]] = array_obj_new(s)

    def apply(x: Rep[Array[T]], i: Rep[Int]): Rep[T] = array_apply(x, i)

    def update(x: Rep[Array[T]], i: Rep[Int], y: Rep[T]) = array_update(x, i, y)
  }

  class ScalarSingleArrayOps[T: Manifest] extends ArrayOps[NoRep, Rep, Single, Rep, T] {
    def alloc(s: NoRep[Int]): Array[Rep[T]] = new Array[Rep[T]](s)

    def apply(x: Array[Rep[T]], i: NoRep[Int]): Rep[T] = x(i)

    def update(x: Array[Rep[T]], i: NoRep[Int], y: Rep[T]) = x.update(i, y)
  }

  class StagedPackedArrayOps[T: Manifest] extends ArrayOps[Rep, Rep, Packed, NoRep, T] {
    def alloc(s: Rep[Int]): Rep[Array[T]] = array_obj_new(s)

    def apply(x: Rep[Array[T]], i: Rep[Int]): Rep[Packed[T]] = infix_vload[T](x, i, codegen.getInstructionSetVectorSize[T])

    def update(x: Rep[Array[T]], i: Rep[Int], y: Rep[Packed[T]]) = infix_vstore(x, i, y)
  }

  class PackedScalarArrayOps[T: Manifest] extends ArrayOps[NoRep, Rep, Packed, RepPacked, T] {
    def alloc(s: NoRep[Int]): Array[RepPacked[T]] = new Array[RepPacked[T]](s)

    def apply(x: Array[RepPacked[T]], i: NoRep[Int]): Rep[Packed[T]] = x(i)

    def update(x: Array[RepPacked[T]], i: NoRep[Int], y: Rep[Packed[T]]) = x.update(i, y)
  }

  class ScalarPackedArrayOps[T: Manifest] extends ArrayOps[NoRep, Rep, Packed, Rep, T] {
    def alloc(s: NoRep[Int]): Array[Rep[T]] = new Array[Rep[T]](s)

    def apply(x: Array[Rep[T]], i: NoRep[Int]): Rep[Packed[T]] = {
      val r: Rep[Packed[T]] = null
      // TODO (Alen Stojanov): Write the implementation using infix_vset (or smth)
      r
    }

    def update(x: Array[Rep[T]], i: NoRep[Int], y: Rep[Packed[T]]) = {
      // TODO (Alen Stojanov): Write the implementation using infix_vget (or smth)
    }
  }

}
*/


/* ========================================================================================================= */
/* ============================================ LiftOps ==================================================== */
/* ========================================================================================================= */
/*
object LiftOps {

  class LifOpsRep extends LiftOps[Rep] {
    def apply[T: Numeric : Manifest](x: T) = numericToNumericRep(x)

    def apply(x: Unit) = fresh[Unit]

    def apply[X[_], T: Numeric : Manifest](exp: X[T])(implicit mE: Manifest[X[Any]]): Rep[T] = exp match {
      case c: Const[_] => apply(c.x.asInstanceOf[T])
      case _ if (mA == mE) => exp.asInstanceOf[Rep[T]]
      case _ => throw new LiftOpsException(exp.toString + " can not be lifted")
    }

    def staged() = true
  }

  class LifOpsNoRep extends LiftOps[NoRep] {
    def apply[T: Numeric : Manifest](x: T) = x

    def apply(x: Unit) = Unit

    def apply[X[_], T: Numeric : Manifest](exp: X[T])(implicit mE: Manifest[X[Any]]): NoRep[T] = exp match {
      case c: Const[_] => apply(c.x.asInstanceOf[T])
      case _ if (mA == mE) => exp.asInstanceOf[NoRep[T]]
      case _ => throw new LiftOpsException(exp.toString + " can not be lifted")
    }

    def staged() = false
  }

}*/
/*

object ImplicitOps {
  /* =========================================== NumericOps ================================================== */

  implicit def numericNoRepOps[T: Numeric : Manifest]: NumericOps[NoRep[Single[T]]] = new NumericOps.NumericNoRepOps[T]

  implicit def numericRepOps[T: Numeric : Manifest]: NumericOps[Rep[Single[T]]] = new NumericOps.NumericRepOps[T]

  implicit def packedNumericOps[T: Numeric : Manifest]: NumericOps[Rep[Packed[T]]] = new NumericOps.PackedNumericOps[T]

  /* =========================================== ElementOps ================================================== */

  implicit def complexOps[T: Manifest](implicit nops: NumericOps[T]): ElementOps[Complex, T] = new ElementOps.ComplexOps[T]()

  implicit def realOps[T: Manifest](implicit nops: NumericOps[T]): ElementOps[Real, T] = new ElementOps.RealOps[T]()

  /* ============================================ ArrayOps =================================================== */

  implicit def stagedPackedArrayOps[T: Manifest]: ArrayOps[Rep, Rep, Packed, NoRep, T] = new ArrayOps.StagedPackedArrayOps[T]

  implicit def scalarPackedArrayOps[T: Manifest]: ArrayOps[NoRep, Rep, Packed, Rep, T] = new ArrayOps.ScalarPackedArrayOps[T]

  implicit def packedScalarArrayOps[T: Manifest]: ArrayOps[NoRep, Rep, Packed, RepPacked, T] = new ArrayOps.PackedScalarArrayOps[T]

  implicit def scalarSingleArrayOps[T: Manifest]: ArrayOps[NoRep, Rep, Single, Rep, T] = new ArrayOps.ScalarSingleArrayOps[T]

  implicit def stagedSingleArrayOps[T: Manifest]: ArrayOps[Rep, Rep, Single, NoRep, T] = new ArrayOps.StagedSingleArrayOps[T]

  /* ========================================= VectorElementOps ============================================== */

  implicit def vectorElementOpsSingle[E[_], T: Manifest]
  (implicit eops: ElementOps[E, Rep[Single[T]]]): VectorElementOps[E, Rep, Single, T] =
    new VectorElementOps.VectorElementOpsSingle[E, T]()

  implicit def vectorElementOpsPacked[E[_], T: Manifest]
  (implicit eops: ElementOps[E, Rep[Packed[T]]]): VectorElementOps[E, Rep, Packed, T] =
    new VectorElementOps.VectorElementOpsPacked[E, T]()

  /* ============================================ LiftOps ==================================================== */

  implicit object RepObject extends LiftOps.LifOpsRep

  implicit object NoRepObject extends LiftOps.LifOpsNoRep

  /* ========================================================================================================= */
}*/
