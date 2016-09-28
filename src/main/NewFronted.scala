import scala.annotation.implicitNotFound
import scala.annotation.unchecked.uncheckedVariance

trait Base {
  @implicitNotFound("${T} is not a DSL type")
  type Typ[T] //type class to tag front-end types as staged

  @implicitNotFound("${A} cannot be implicitly lifted to ${B}")
  type Lift[A,B]

  implicit def identLift[T:Typ]: Lift[T,T]
  implicit def lift[A,B](x: A)(implicit e: Lift[A,B]): B
}

//TODO: refactor implicits into separate traits from types

//we currently have a nice symmetry: for all front-end types we have T:Typ; for all back-end types T:Manifest; we never have both

//this closely mirrors existing Expressions definitions
//may want Exp to eventually hold some kind of type tag other than Manifest, but this should not be Typ!
trait BaseExp extends Base {

  abstract class Exp[+T:Manifest] {
    def tp: Manifest[T @uncheckedVariance] = manifest[T]
  }

  case class Sym[+T:Manifest](id: Int) extends Exp[T] {
    override def toString = "x"+id
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  abstract class Def[+T]

  var nVars = 0

  def fresh[T:Manifest] = { nVars += 1; Sym(nVars-1) }

  //note: the error messages are better when this is added explicitly, might be worth it since auto-generated anyway
  implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    val e = fresh
    println(s"val $e = $d (${e.tp})")
    e
  }

  def unit[T:Manifest](x: T): Exp[T] = Const(x)

  trait Typ[T] {
    type Internal
    def from(e: Exp[Internal]): T
    def to(x: T): Exp[Internal]
    def m: Manifest[Internal]
  }

  trait Lift[A,B] {
    def to(x: A): B
  }

  def identLift[T:Typ]: Lift[T,T] = new Lift[T,T]{ def to(x: T) = x }
  def lift[A,B](x: A)(implicit e: Lift[A,B]): B = e.to(x)

  def typ[T:Typ] = implicitly[Typ[T]]

}

trait Ints extends Base {
  protected trait IntOps {
    def +(y: Int): Int
    def -(y: Int): Int
  }

  type Int <: IntOps
  implicit val intTyp: Typ[Int]
  implicit val intLift: Lift[scala.Int, Int]
  implicit val intNumeric: Numeric[Int] //TODO: the required methods of Numeric make it a poor choice for IR.Int... need a new typeclass instead (a la Arith)?
  //      or just hijack the scala.Int Numeric since we only need its impl for Const operations anyway
}

trait IntsImpl extends BaseExp with Ints {
  //type CInt = scala.Int //could declare alias, but doesn't save much effort; can't universally use name 'Internal' b/c traits get mixed together

  case class Int(e: Exp[scala.Int]) extends IntOps {
    def +(y: Int) = Int(int_plus(e, y.e))
    def -(y: Int) = Int(IntMinus(e, y.e))
  }

  case class IntPlus(x: Exp[scala.Int], y: Exp[scala.Int]) extends Def[scala.Int]
  case class IntMinus(x: Exp[scala.Int], y: Exp[scala.Int]) extends Def[scala.Int]

  val intTyp: Typ[Int] = new Typ[Int] { //note: abstract def -> concrete val, and not implicit anymore!
  type Internal = scala.Int
    def from(e: Exp[scala.Int]) = Int(e)
    def to(x: Int) = x.e
    def m = manifest[scala.Int]
  }

  val intLift: Lift[scala.Int, Int] = new Lift[scala.Int, Int] {
    def to(x: scala.Int) = Int(unit(x))
  }

  val intNumeric: Numeric[Int] = implicitly[Numeric[scala.Int]].asInstanceOf[Numeric[Int]]

  def int_plus(lhs: Exp[scala.Int], rhs: Exp[scala.Int]): Exp[scala.Int] = (lhs, rhs) match {
    case (Const(0), r) => r
    case (l, Const(0)) => l
    case (Const(x), Const(y)) => Const(x+y)
    case _ => IntPlus(lhs,rhs)
  }

}

trait Units extends Base {
  type Unit
  implicit def unitTyp: Typ[Unit]
  implicit val unitLift: Lift[scala.Unit, Unit]
}

trait UnitsImpl extends BaseExp {
  case class Unit(e: Exp[scala.Unit])
  val unitTyp = new Typ[Unit] {
    type Internal = scala.Unit
    def from(e: Exp[scala.Unit]) = Unit(e)
    def to(x: Unit) = x.e
    def m = manifest[scala.Unit]
  }
  val unitLift: Lift[scala.Unit, Unit] = new Lift[scala.Unit, Unit] {
    def to(x: scala.Unit) = Unit(unit(()))
  }
}

trait Primitives extends Ints with Units //with Booleans
trait PrimitivesImpl extends Primitives with IntsImpl with UnitsImpl

//note: unlike Reps all other types we need to name must be included so that type shadowing works
trait Arrays extends Base { this: Primitives =>
  protected trait ArrayOps[T] { //traits cannot have type bounds, but abstract classes can; is it important for :Typ to be here?
  def length: Int
    def apply(i: Int): T
    def update(i: Int, x: T): Unit
    def map[B:Typ](f: T => B): Array[B]
  }
  def NewArray[T](length: Int)(implicit tp: Typ[T]): Array[T]

  type Array[T] <: ArrayOps[T]
  implicit def arrayTyp[T:Typ]: Typ[Array[T]]
  //note: while we need Typ defined for every type, lift is optional
}

trait ArraysImpl extends BaseExp with Arrays { this: PrimitivesImpl => //note: if we just import the interface Int.e doesn't work
object Array {
  //this ordering trick works for methods but not classes :(
  //we could just make the class arg sufficiently generic to always work without casts and remove this, but using the method provides better static checking
  def apply[T](tp: Typ[T])(e: Exp[scala.Array[tp.Internal]]): Array[T] = Array(e.asInstanceOf[Exp[scala.Array[Any]]])(tp)
}
  case class Array[T:Typ](private val bleh: Exp[scala.Array[Any]]) extends ArrayOps[T] {
    val tp = typ[T]
    implicit val tpm = tp.m //Manifest[tp.Internal] isn't implicit by default; so declare here or add explicitly below
    val e = bleh.asInstanceOf[Exp[scala.Array[tp.Internal]]]

    def length = Int(ArrayLength(e))
    def apply(i: Int) = tp.from(toAtom(ArrayApply(e, i.e)))
    def update(i: Int, x: T) = Unit(ArrayUpdate(e, i.e, tp.to(x)))
    def map[B:Typ](f: T => B) = {
      val tpB = typ[B]
      implicit val tpBm = tpB.m
      val g: Exp[tp.Internal] => Exp[tpB.Internal] = e => tpB.to(f(tp.from(e)))
      Array(tpB)(toAtom(ArrayMap(e, g)))
    }
  }

  def NewArray[T](length: Int)(implicit tp: Typ[T]): Array[T] = {
    implicit val tpm = tp.m //passing this explicitly is hard b/c we need type Array[tpm]
    Array(tp)(toAtom(ArrayNew[tp.Internal](length.e)))
  }

  case class ArrayLength[T](a: Exp[scala.Array[T]]) extends Def[scala.Int]
  case class ArrayApply[T](a: Exp[scala.Array[T]], i: Exp[scala.Int]) extends Def[T]
  case class ArrayUpdate[T](a: Exp[scala.Array[T]], i: Exp[scala.Int], x: Exp[T]) extends Def[scala.Unit]
  case class ArrayMap[A,B](a: Exp[scala.Array[A]], f: Exp[A] => Exp[B]) extends Def[scala.Array[B]]
  case class ArrayNew[T](length: Exp[scala.Int]) extends Def[scala.Array[T]]

  def arrayTyp[T:Typ]: Typ[Array[T]] = new Typ[Array[T]] { //abstract def -> concrete def when we need type class
  val tp = typ[T]
    private implicit val tpm = tp.m
    type Internal = scala.Array[tp.Internal]
    def from(e: Exp[scala.Array[tp.Internal]]) = Array(tp)(e)
    def to(x: Array[T]) = x.e.asInstanceOf[Exp[scala.Array[tp.Internal]]] //x.tp.Internal /= this.tp.Internal :(
    def m = manifest[scala.Array[tp.Internal]]
  }
}

// // trait Conditionals extends Base { this: Booleans =>
// //   def __ifThenElse[C:Typ,A,B](c: Boolean, a: A, b: B)(implicit mA: Lift[A,C], mB: Lift[B,C]): C
// // }

trait DSLAPI extends Primitives with Arrays //with Numerics

trait DSLImpl extends DSLAPI with PrimitivesImpl with ArraysImpl //with NumericsImpl

trait DSLApp {
  private val IR = new DSLImpl{ }
  val API: DSLAPI = IR //explicit type important so we get the api, not the impl in the app!
  import API._ //note: without this all our useful implicits aren't in scope so just using long type names isn't enough!

  def main(args: Array[Int]): Unit //don't have Strings, close enough

  final def main(args: scala.Array[String]): scala.Unit = {
    implicit val t = IR.intTyp //Typs aren't implicit in the IR scope, only API scope... should they be?
    implicit val tm = t.m
    val sym = IR.Array(t)(IR.fresh[scala.Array[t.Internal]]).asInstanceOf[Array[Int]] //IR.Int considered different from API.Int, needs cast
    val res = main(sym)
    val exp = res.asInstanceOf[IR.Unit].e //and another cast back to IR
    println(exp)
  }
}

object App extends DSLApp {
  import API._

  //TODO: Typ bleeds through whenever users write generic types
  //how do we automatically translate between shallow and deep in cases like this? make a dummy Typ available in shallow?
  //note: same issue with lift() if ever called explicitly by users
  //fortunately these are the only 2 things currently accessible to users via Base (along with their concrete impls for each type)
  //note: if we make IntOps protected it's not accessible as a type in the App (see below), only the abstract type Int
  def myMap[A:Typ,B:Typ](f: A => B) = {
    val x = NewArray[A](100)
    x.map(f)
  }

  //def myNumericPlus[A:Typ:Numeric, B:Typ:Numeric](x: A, y: B) = x + y

  //def myPlus[A<:IntOps](a: A, b: Int) = a + b //fails if IntOps trait protected, works if public

  def main(args: Array[Int]) = {
    val x: Int = 5 //IR.Int
    val y = x + 3 //3 is lifted implicitly; rewrite rule should eliminate +

    val xs = NewArray[Int](100)
    xs(y) = x
    println(xs(y))

    val xs2 = xs.map(e => e + 2)

    val ys = NewArray[Array[Int]](1)
    ys(0) = xs
    println(ys(0))
  }
}