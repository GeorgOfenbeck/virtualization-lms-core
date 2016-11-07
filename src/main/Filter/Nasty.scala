package Filter

/**
  * Created by a codemonkey on 31-Oct-16.
  */
object Nasty1 extends App{

  /***
    *
    * @param a
    * @param b
    * @tparam A
    * @tparam B
    * @tparam AB
    * @return
    */
  //Simple Version
  def foo1[A: Numeric, B: Numeric, AB: Numeric](a: A, b: B): AB = {
    val eva = implicitly[Numeric[A]]
    val evb = implicitly[Numeric[B]]

    //a + b
    ???
  }












  // encode least upper bound relation as implicit
  trait Lub[A, B, C] {
    implicit def fromA(x: A): C

    implicit def fromB(x: B): C
  }

  implicit def IntInt: Lub[Int, Int, Int] = new Lub[Int, Int, Int] {
    def fromA(x: Int) = x

    def fromB(x: Int) = x
  }
  implicit def IntDouble: Lub[Int, Double,Double] = new Lub[Int, Double, Double] {
    def fromA(x: Int) = x.toDouble

    def fromB(x: Double) = x
  }
  implicit def DoubleInt: Lub[Double, Int, Double] = new Lub[Double, Int, Double] {
    def fromA(x: Double) = x.toDouble

    def fromB(x: Int) = x
  }
  implicit def DoubleDouble: Lub[Double, Double, Double] = new Lub[Double, Double, Double] {
    def fromA(x: Double) = x.toDouble

    def fromB(x: Double) = x.toDouble
  }



  //Simple Solution
  def foo2[A: Numeric, B: Numeric, AB: Numeric](a: A, b: B)(lub:Lub[A,B,AB]): AB = {
    val eva = implicitly[Numeric[A]]
    val evb = implicitly[Numeric[B]]
    val evab = implicitly[Numeric[AB]]

    val a_ab = lub.fromA(a)
    val b_ab = lub.fromB(b)

    evab.plus(a_ab,b_ab)
  }


  /***
    *
    * @param a
    * @param b
    * @param lub
    * @tparam A
    * @tparam B
    * @tparam AB
    * @return
    */
  def foo3[A: Numeric, B: Numeric, AB: Numeric](a: A, b: B)(implicit lub:Lub[A,B,AB]): AB = {
    val eva = implicitly[Numeric[A]]
    val evb = implicitly[Numeric[B]]
    val evab = implicitly[Numeric[AB]]

    val a_ab = lub.fromA(a)
    val b_ab = lub.fromB(b)

    val t = evab.plus(a_ab,b_ab)

    if (t == evab.zero)
      evab.zero
    else
      ???
      //foo3(a,t)
  }


  /***
    *
    * @param a
    * @param b
    * @param lub1
    * @param lub2
    * @tparam A
    * @tparam B
    * @tparam AB
    * @return
    */
  def foo4[A: Numeric, B: Numeric, AB: Numeric](a: A, b: B)(implicit lub1:Lub[A,B,AB], lub2: Lub[A,AB,AB]): AB = {
    val eva = implicitly[Numeric[A]]
    val evb = implicitly[Numeric[B]]
    val evab = implicitly[Numeric[AB]]

    val a_ab = lub1.fromA(a)
    val b_ab = lub1.fromB(b)

    val t = evab.plus(a_ab,b_ab)

    if (t != evab.zero)
      evab.zero
    else
      foo4(a,t)(eva,evab,evab,lub2,lub2)
  }

  usage()

  def usage(): Unit ={
    val x = foo4[Int,Int,Int](-1,1)
    println(x)
  }


  /***
    *
    * @param a
    * @param b
    * @param lub1
    * @param lub2
    * @param lub3
    * @param lub4
    * @tparam A
    * @tparam B
    * @tparam AB
    * @return
    */
  def foo5[A: Numeric, B: Numeric, AB: Numeric](a: A, b: B)(implicit lub1:Lub[A,B,AB], lub2: Lub[A,AB,AB], lub3: Lub[AB,B,AB], lub4: Lub[AB,AB,AB]): AB = {
    val eva = implicitly[Numeric[A]]
    val evb = implicitly[Numeric[B]]
    val evab = implicitly[Numeric[AB]]

    val a_ab = lub1.fromA(a)
    val b_ab = lub1.fromB(b)

    val t = evab.plus(a_ab,b_ab)

    if (t == evab.zero)
      evab.zero
    else
      ???
    //foo5(a,t)(eva,evab,evab,lub2,lub2)
  }




  def foo[A: Numeric, B: Numeric, C: Numeric, D: Numeric](a: A, b: B, c: C): D = {
    val eva = implicitly[Numeric[A]]
    val evb = implicitly[Numeric[B]]
    val evc = implicitly[Numeric[C]]

   /* if (a == eva.zero)
      foo(a+b,b,c)
    else if (b == evb.zero)
      foo(a,b+c,c)
    else if (c == evc.zero)
      foo(a,b,c+a)
    else
      a + b + c*/
  ???
  }





}
