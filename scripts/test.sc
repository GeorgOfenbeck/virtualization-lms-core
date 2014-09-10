import scala.collection.mutable


val bla = Vector(3,1)
val y = bla(4)

case class Bla[T](val x: T)

trait immutablething {
  val locallist: Vector[something#Exp]
}


trait something {
  self =>
  val IR: something = self
  protected implicit def InttoBla(x: something#Exp): Bla[Int] = ???

  case class Exp(x: Int)

  val locallist: Option[scala.collection.mutable.ArrayBuffer[something#Exp]] = None

  def returnExp(in: Int) = Exp(in)

  def reconstruct(in: Exp) = Exp(in.x)

  def givemeBla(x: something#Exp): immutablething = {
    val withdiffimplicit = new something {
      //override val IR: self.type = self
      override val locallist = Some(mutable.ArrayBuffer.empty[something#Exp])
      override protected implicit def InttoBla(exp: something#Exp): Bla[Int] = {
        val x = exp.x
        locallist.get += exp
        println("inner view of the list: " + locallist.get)
        Bla(x)

      }
    }
    println("my view of the list: " + locallist)
    withdiffimplicit.f(x)

    val ret = new immutablething {
      override val locallist: Vector[something#Exp] = withdiffimplicit.locallist.get.toVector
    }
    ret
  }
  def f(x: something#Exp): Bla[Int] = {
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