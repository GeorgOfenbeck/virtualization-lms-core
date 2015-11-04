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
