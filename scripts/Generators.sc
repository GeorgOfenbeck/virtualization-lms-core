

import org.scalacheck._
import Gen._
def GenRecList(): Gen[List[Int]]= {
  val x: List[Int] = List(1)
  val bla = for {
    ini <- Gen.const(x)
  } yield ini
  GenRec(10,10,bla)
}




def GenRec(full: Int, remain: Int, sofar: Gen[List[Int]]): Gen[List[Int]] = {
  if (remain > 0)
  {
    val outer_res =   for {
      num <- Gen.choose(1,2)
      pos <- Gen.choose(0,full-remain)
      prev <- sofar

    } yield {
      val res= if (num == 1)
      {
        num :: prev

      }
      else
      {
        (prev(pos) * 2) :: prev

      }

      res
    }
    GenRec(full,remain-1,outer_res)
  }
  else
  {
    sofar
  }
}

val la = GenRecList()
println(la.sample.get)
println("done")