val a = "abcdef"
val b = "abcdef"

a == b


case class check (val next: Option[check], val f: Int => Int) {
  def default: (Int => Int) = (in: Int) => in
}


val first = check(None, {
  val f: (Int => Int) = (in: Int) => in
  f
})

val second = check(Some(first), {
  val f: (Int => Int) = (in: Int) => first.f(in)
  f
})