package SpiralS2


trait Skeleton extends Spiral_DSL{

  type NoRep[T] = T

  // encode least upper bound relation as implicit
  trait Lub[A[_], B[_], C[_]] {
    implicit def fromA[T: TypeRep](x: A[T]): C[T]

    implicit def fromB[T: TypeRep](x: B[T]): C[T]
  }

  implicit def NoRepNoRep: Lub[NoRep, NoRep, NoRep] = new Lub[NoRep, NoRep, NoRep] {
    def fromA[T: TypeRep](x: T) = x;

    def fromB[T: TypeRep](x: T) = x
  }

  implicit def RepNoRep: Lub[Rep, NoRep, Rep] = new Lub[Rep, NoRep, Rep] {
    def fromA[T: TypeRep](x: Rep[T]) = x;

    def fromB[T: TypeRep](x: T) = Const(x)
  }

  implicit def NoRepRep: Lub[NoRep, Rep, Rep] = new Lub[NoRep, Rep, Rep] {
    def fromA[T: TypeRep](x: T) = Const(x);

    def fromB[T: TypeRep](x: Rep[T]) = x
  }

  implicit def RepRep: Lub[Rep, Rep, Rep] = new Lub[Rep, Rep, Rep] {
    def fromA[T: TypeRep](x: Rep[T]) = x;

    def fromB[T: TypeRep](x: Rep[T]) = x
  }

  trait IRep[T[_]] extends RepBase[T] //with Conditionals[T] with StagedNum[T] with Comparisons[T] with RangeFold[T] with ChooseStuff[T] with BooleanOps[T]


  implicit object cRep extends IRep[Rep] with isRepBase //with RepNum with RepConditionals with RepComparisons with RepRangeFold with RepChooseStuff with RepBooleanOps

  implicit object cNoRep extends IRep[NoRep] with noRepBase //with NoRepNum with NoRepConditionals with NoRepComparisons with NoRepRangeFold with NoRepChooseStuff with NoRepBooleanOps

  trait RepBase[T[_]] {
    def isRep(): Boolean

    def const[A: TypeRep](x: A): T[A]

    def toRep[A: TypeRep](x: T[A]): Rep[A]

    def getRep[A](x: T[A]): Option[Rep[A]]

    def getNoRep[A](x: T[A]): Option[A]

    def fresh[A: TypeRep](): Vector[Rep[_]]

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Option[T[A]])
  }

  trait isRepBase extends RepBase[Rep] {
    val isRep = true

    def const[A: TypeRep](x: A): Rep[A] = Const(x)

    def toRep[A: TypeRep](x: Rep[A]): Rep[A] = x

    def getRep[A](x: Rep[A]): Some[Rep[A]] = Some(x)

    def getNoRep[A](x: Rep[A]): Option[A] = None

    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector(Arg[A])

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Some[Rep[A]]) = (x.tail, Some(x.head.asInstanceOf[Rep[A]]))

  }

  trait noRepBase extends RepBase[NoRep] {
    val isRep = false

    def const[A: TypeRep](x: A): NoRep[A] = x

    def toRep[A: TypeRep](x: NoRep[A]): Rep[A] = Const(x)

    def getRep[A](x: NoRep[A]): Option[Rep[A]] = None

    def getNoRep[A](x: NoRep[A]): Some[A] = Some(x)

    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector.empty

    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]], Option[NoRep[A]]) = (x, None)

  }





}
