package Filter2

/**
  * Created by rayda on 02-Nov-16.
  */
trait ForTiark extends sort.Skeleton {



  case class GEntry[T1: Numeric, A1[_]: IRep](a1: A1[T1]) extends OneEntry{
    type T = T1
    type A[X] = A1[X] //i assume this syntax is just not correct
    val a: A1[T1] = a1
    val ev: IRep[A1] = ??? //implicitly[IRep[A1]]
    val evnum = implicitly[Numeric[T1]]
  }

  abstract class Matrix {
    type R1
    type R2
    type R3
    val r1: R1
    val r2: R2
    val r3: R3

    type ThisM = Matrix { type R1 = this.R1;  }
  }

  abstract class Row {
    val c1: OneEntry
    val c2: OneEntry
    val c3: OneEntry
  }

  abstract class OneEntry {
    type T
    type A[_]
    val a: A[T]
    val ev: IRep[A]
    val evnum: Numeric[T]
  }


//  abstract class Mat {
//
//  }


  //i want to be able to write that they need to be of the same type (all fields)
  def identicalmatrix(a: Matrix, b: Matrix): Matrix = {
    ???
  }

  def copy(a: Matrix): Matrix = {
    new Matrix{
      val r1 = a.r1
      val r2 = a.r2
      val r3 = a.r3
    }
  }



  def copychange(a: Matrix): Matrix = {
    new Matrix{
      val r1 = a.r1
      val r2 = a.r2
      val r3 = new Row{
        val c1 = a.r3.c1
        val c2 = a.r3.c2
        val c3 = new OneEntry {
          override type A[X] = NoRep[X]
          override type T = Int
          override val evnum: Numeric[Int] = implicitly[Numeric[Int]]
          override val ev: IRep[NoRep] = cNoRep
          override val a: this.A[this.T] = ev.const(1)
        }
      }
    }
  }

/* def usecase[M1,M2](stat: Matrix): (stat.share. => Int) = {

    val f: (Matrix => Int) = (dyn: Matrix) => {
      mix(stat,dyn)
    }
    f
  }*/



}







