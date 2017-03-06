/*
/**
  * Created by rayda on 13-Jan-17.
  */
class Snippets {

  type Rep[T] = T

  def stagef(f: Rep[Int] => Rep[Int]): Rep[Int => Int] = ???
  def stagef2(f: (Rep[Int],Rep[Int]) => Rep[Int]): Rep[(Int,Int) => Int] = ???

  def generate_add_function(stat: Int): Rep[Int => Int] = {
    def addf(dyn: Rep[Int]): Rep[Int] = dyn + stat
    stagef(addf) //stage the function (Rep[Int] => Rep[Int]) =>  Rep[Int => Int]
  }

  def poly_generate_f (maybestat: Either[Rep[Int], Int]): Either[Rep[ (Int,Int) => Int], Rep[Int => Int]] = {
    val rf: Either[Rep[ (Int,Int) => Int], Rep[Int => Int]] = maybestat.fold(
      isdyn => {
        def addf(dyn: Rep[Int], dyn2: Rep[Int]): Rep[Int] = dyn + dyn2
        Left(stagef2(addf)) //stage the function (Rep[Int] => Rep[Int]) =>  Rep[Int => Int]
      },
      isstat => {
        def addf(dyn: Rep[Int]): Rep[Int] = dyn + isstat
        Right(stagef(addf)) //stage the function (Rep[Int] => Rep[Int]) =>  Rep[Int => Int]
      }
    )
    rf
  }



  def doLamda[R[_]](f: Dyn[R] => Int): StagedFunction[Dyn[R],Int] = ???


  abstract class Base[R[_]](i: R[Int] )
  class Dyn[R[_]](x: Rep[Int], i: R[Int]) extends Base(i)
  class Stat[R[_]](i: R[Int], const: Int) extends Base(i) {
    def get_poly(): Option[Rep[Int]] = {
      i match {
        case x: Rep[Int] => Some(x)
        case _ => None
      }
    }
  }
  case class Mix[R[_]](x: Rep[Int], i: R[Int], const: Int) extends Base(i)
  object Mix{
    def apply[R[_]](stat: Stat[R], dyn: Dyn[R]): Mix[R] = ???
  }

  def poly_generate_f[R[_]](stat: Stat[R]): StagedFunction[Dyn[R],Rep[Int]] = {
    def addf(dyn: Dyn[R]): Rep[Int] = {
      val mix = Mix(stat,dyn)
      mix.x + mix.i + mix.const
    }
    doLambda(addf)
  }


}
*/
