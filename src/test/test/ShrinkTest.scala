package test
object GenFs{
 //def default() : (Int => Int) = (in: Int) => in
 case class FNest(
                   val next: Option[FNest],
                   val f: Int => Int
                   )



 def genOk(): Gen[FNest] = lzy {
  val f : (Int => Int) = (in: Int) => {
   println("+1")
   in + 1
  }
  FNest(None,f)
 }

 def genFail(): Gen[FNest] = lzy {
  val f: (Int => Int) = (in: Int) => {
   println("*-1")
   in * -1
  }
  FNest(None,f)
 }


 def genSequence(deepth: Int): Gen[FNest] = {
  if (deepth > 0)
   for {
    rand <- Gen.choose(0,100)
    randomF <- if (rand == 69) genFail() else genOk()
    recurse <- genSequence(deepth-1)
   } yield {

    val next = recurse

    val regular : (Int => Int) = (in: Int) => {
     next.f(randomF.f(in))
    }
    /*
    val regular : (Int => Int) = (in: Int) => {
      recurse.regular(randomF.regular(in))
    }
    val without:  (Int => Int) = (in: Int) => {
      recurse.regular(in)
    }*/
    //FNest(regular,without)
    FNest(Some(next),regular)
   }
  else
  {
   val f : (Int => Int) = (in: Int) => {
    println(in)
    in
   }
   Gen.const(FNest(None,f))
  }
 }

}



class ShrinkTest  extends Properties("bitstuff") {
 import org.scalacheck.Shrink.shrink
 implicit val shrinkExpr: Shrink[GenFs.FNest] = Shrink({
  case GenFs.FNest(Some(next),f) => Stream.concat(
   shrink(next) map (smaller => (GenFs.FNest(Some(smaller), f))),
   Stream(next)
   //without current element

  )
  case GenFs.FNest(None,f) => Stream()

 })

 property("always pos") = forAll(GenFs.genSequence(3)) {
  f => {
   val x = f.f(1)
   println(x)
   x > 0
  }
 }
}