
import org.scalacheck._
import Gen._


def GenRandomArray(): Gen[Array[Int]] = {
  for {
    size <- Gen.choose(1,3)
  } yield
  {
      val x = new Array[Int](size)
      x

  }
}


def GenRandomAccess(random: Gen[Array[Int]]): Gen[Int] = {
  for {
    arr <- GenRandomArray()
    choice <- Gen.choose(1,10)
  } yield{
    arr(choice)
  }
}


/*
def GenRandomAccess2(randomarr: Gen[Unit => Array[Int]]): Gen[Unit => Int] = {

  val res = randomarr.flatMap(
    arrf => {
        val result = Gen.const( () ).flatMap {
          unit => {
            val arr = arrf()
            val f : Unit => Int = (u : Unit ) => {

              val appy = Gen.choose(0, arr.size).map(
                index => {
                  val re = arr(index)
                  re
                }
              )
              appy
            }
          }
      }
      result
    }
  )
  res
}


def GenRandomAccess1(randomarr: Gen[Unit => Array[Int]]): Gen[Unit => Int] = {

  val res = randomarr.flatMap(
    arrf => {
      val arr = arrf() //give me one array
      val appy = Gen.choose(0,arr.size).map(
        index => {
          val f: Unit => Int = (u: Unit ) =>  arr(index)
          f
        }
      )
      appy
    }
  )
  res
}
*/

/*def GenRandomAccess(randomarr: Gen[Unit => Array[Int]]): Gen[Int] = {
  for {
    arrf <- randomarr
  } yield {
    val arr = arrf()
    val res = for {
     index <- Gen.choose(0,arr.length-1)
    } yield {
      arr(index)
    }
    res
  }
}*/



def GenIntSquare(): Gen[Int => Int] = {
  for {
    sq <- Gen.choose(1,2)
  } yield {
    val f: (Int => Int) = (in: Int) => in * sq
    f
  }
}


def GenIntFunc(): Gen[Int => Int] = {
    val check = for {
      plus <- Gen.choose(1,2)
      sqf <- GenIntSquare()
    } yield {
      val f: (Int => Int) = (in: Int) => sqf(in) + plus
      f
    }
  check


    /*val f: (Int => Int) = (in: Int) => {
      val g = for {
        sq <- GenIntSquare()
      }yield {
        sq(in)
      }
      g

    }
    f */
}


def GenIntF(): Gen[(Unit => Int)] = {
  val genf = for {
    value <- Gen.choose(1,9)
  }
  yield {
    val f: (Unit => Int) = (bla: Unit) => {
      value
    }
    f
  }
  genf
}

def GenIntG(): Gen[(Unit => Int)] = {
  val genf = for {
    value <- Gen.choose(1, 9)
    g <- GenIntF()
  }
  yield {
    val f: (Unit => Int) = (bla: Unit) => {
      g() * value
    }
    f
  }
  genf
}

def GenIntGDesugar: Gen[(Unit => Int)] = {
  val genf: Gen[Unit => Int] =
    Gen.choose(1,9).flatMap(value => {
      GenIntF().map (g => {
        val f: (Unit => Int) = (bla: Unit) => {
          g() * value
        }
        f
      })
    })
  genf
}






for (i <- 0 until 10) {
  val test = GenIntG().sample.get
  println(test())
}