

/*
/*****************************************
  Emitting Generated Code
  *******************************************/
/*****************************************
  Emitting Generated Code
  *******************************************/
class tupler$3 extends (Vector[Any] =>(Long,Float)) {
 def apply( v: Vector[Any]): ((Long,Float)) = {
  val x: ((Long,Float)) = ((1L).asInstanceOf[Long],(-5.1013167E-6f).asInstanceOf[Float])
  x
 }
}
/*****************************************
  Emitting Generated Code
  *******************************************/
class staged$2 extends (((Long,Float))=> ((Long,Float,Float,Float))) {
 def apply( helper: ((Long,Float))): ((Long,Float,Float,Float)) = {
  val x0 : Long = helper._1
  val x1 : Float = helper._2
  val x2 = x1 * x1
  val x3 = x1 + x2

  (x0,x1,x2,x3)
 }
}
/*****************************************
  Emitting Generated Code
  *******************************************/
class detupler$4 extends (((Long,Float,Float,Float)) => Vector[Any]) {
 def apply(helper: (Long,Float,Float,Float)): (Vector[Any]) = {
  val x: (Vector[Any]) = Vector(helper._1,helper._2,helper._3,helper._4)
  x
 }}
/*****************************************
  End of Generated Code
  *******************************************/

object Srsly extends App{
 val tupler = new tupler$3()
 val code = new staged$2()
 val detupler = new detupler$4
 val a = tupler.apply(Vector.empty)
 val b = code(a)
 val c = detupler(b)
 println(c)


}*/
/*
/**
 * Georg Ofenbeck
 First created:
 * Date: 28/10/2015
 * Time: 10:16 
 */
object Srsly extends App{
 val iter = 10000

 {
  val start = System.nanoTime()
  val t = (0 until iter).foldLeft(0.0)(
   (acc1, ele1) => {
    val x = (0 until iter).foldLeft(0.0)(
     (acc, ele) => acc + Math.sin(ele)
    )
    acc1 + ele1 + x
   }
  )
  val stop = System.nanoTime()
  val time = (stop - start) / 1000000000.0;
  println(t, time)
 }

 {
  val start = System.nanoTime()
  val x = (0 until iter).foldLeft(0.0)(
   (acc, ele) => acc + Math.sin(ele)
  )
  val t = (0 until iter).foldLeft(0.0)(
   (acc1, ele1) => {
    acc1 + ele1 + x
   }
  )
  val stop = System.nanoTime()
  val time = (stop - start) / 1000000000.0;
  println(t, time)
 }



}
*/
