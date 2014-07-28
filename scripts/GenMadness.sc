
import org.scalacheck._
import Gen._


def targetGen() : Gen[Int] => Gen[Int] = {
  for {
    feed <- feedin()
    spit <- spitout()
  } yield {
    val f: (Int => Int) = (in: Int) => {
      val l = feed(in)
      val x = addnTimes(l)
      val i: Int = spit(x)
      i
    }
    f
  }
}


def spitout(): Gen[List[Int] => Int] = {
  val f: (List[Int] => Int) = (in : List[Int]) => in.head
  Gen.const(f)
}

def feedin(): Gen[Int => List[Int]] = {
    val f: (Int => List[Int]) = (in : Int) => List(in)
    Gen.const(f)
}




def addnTimes(sofar: Gen[List[Int]]): Gen[List[Int]] = {
  val ret = for {
    depth <- Gen.choose(2,10)
    res <- addrecurse(depth,sofar)
  } yield res
  ret
}

def addrecurse(n: Int, sofar: Gen[List[Int]]): Gen[List[Int]] = {
  if (n > 0)  {
    val x = addRandom(sofar)
    addrecurse(n-1,x)
  }
  else
  {
    sofar
  }
}

def addRandom(prev: Gen[List[Int]]): Gen[List[Int]] = {
  for {
    sofar <- prev
    a <- choose(0,sofar.size)
    b <- choose(0,sofar.size)
  } yield (sofar(a)+sofar(b)) :: sofar
}



val res = targetGen().sample.get
res(199)