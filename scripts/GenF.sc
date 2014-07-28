
import org.scalacheck._
import Gen._





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