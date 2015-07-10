import scala.collection.mutable


case class Bla[T](val x: T)

trait immutablething {
  val IR: something
  val locallist: Vector[IR.Exp]
}

trait something {
  self =>
  val IR: something = self
  protected implicit def InttoBla(x: IR.Exp): Bla[Int] = ???

  case class Exp(x: Int)

  val locallist: Option[scala.collection.mutable.ArrayBuffer[IR.Exp]] = None

  def returnExp(in: Int) = IR.Exp(in)

  def reconstruct(in: Exp) = IR.Exp(in.x)

  def givemeBla(x: IR.Exp): immutablething = {


    val ret = new immutablething {
      override val IR: self.IR.type = self.IR
      override val locallist: Vector[IR.Exp] = Vector(x)
      //override val locallist = Vector()
    }

    ret
  }
  def f(x: IR.Exp): Bla[Int] = {
    x
  }
}
val test = new something {}
//val err = test.f(4)
val exp = test.returnExp(4)
val res = test.givemeBla(exp)

val value = res.locallist(0).x

/*
val fromarray = test.locallist.get.apply(0)
test.reconstruct(fromarray)
val num = res.x
println(num)
*/