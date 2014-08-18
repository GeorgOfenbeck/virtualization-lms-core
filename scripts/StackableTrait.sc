abstract class Pr {
  def pr()
}

trait PrePostPr extends Pr {
  abstract override def pr() {
    println("prepr")
    super.pr()
    println("postpr")
  }
}

class ImplPr extends Pr {
  def pr() = println("Foo")
}

//object Foo extends ImplPr with PrePostPr

//Foo.pr()