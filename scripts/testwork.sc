import scala.collection.mutable
case class Bla[T](val x: T)

trait something {
  self =>
  val IR: something = self
  protected implicit def InttoBla(x: Int): Bla[Int] = ???

  val locallist: Option[scala.collection.mutable.ArrayBuffer[Int]] = None

  def givemeBla(x: Int): Bla[Int] = {
    val withdiffimplicit = new something {
      override val IR: self.type = self
      override val locallist = Some(mutable.ArrayBuffer.empty[Int])
      override protected implicit def InttoBla(x: Int): Bla[Int] = {
        locallist.get += x
        println("inner view of the list: " + locallist.get)
        Bla(x)
      }
    }
    println("my view of the list: " + locallist)
    withdiffimplicit.f(x)
  }

  def f(x: Int): Bla[Int] = {
    x
  }
}





val test = new something {}
//val err = test.f(4)
val res = test.givemeBla(4)
val num = res.x
println(num)
