/**
  * Georg Ofenbeck
  * First created:
  * Date: 10/05/2016
  * Time: 17:30 
  */
object IM_Test extends App{

  val ksize = 4
  //val isize = 2
  val msize = 2

  val base = 0


  val in = Array(1,2,3,4,5)
  val out = Array(0,0,0,0,0)


  val foldout = (0 until in.size).par.foldLeft(out){
    (acc,ele) => {
      acc(ele) = in(ele)
      acc
    }
  }
  foldout.map(println(_))


  def fuse(r: IMx, s: IMx): IMx = {
    val newbase = r.base + s.base * r.strides.head
    val x5 = r.strides
    val x58 = s.strides
    val x62 = Vector(x5.headOption.getOrElse(0) * x58.headOption.getOrElse(0)) ++ x5.tail.zipAll(x58.tail,0,0).map(p => p._1 + x5.headOption.getOrElse(0) * p._2)
    IMx(newbase,x62)
  }

  case class IMx(base: Int, strides: Vector[Int]){
    def up(): IMx = IMx(base, Vector(strides.head,0) ++ strides.tail)
  }


  val a = IMx(0,Vector(1,1))
  val b= IMx(0,Vector(1,2))

  println(a.up)
  println(b.up)

  println(fuse(a,b))
  //-----------------------
  println(fuse(a.up,b))

  println(fuse(a,b.up))

  println(fuse(b.up,a))

  println(fuse(b,a.up))

  /*for (k <- 0 until ksize)
    for (i <- 0 until msize)
    {
      val strides = Vector(ksize,1)
      val loopvars = Vector(k,i) // (i,k)

      val tres = loopvars.reverse.zip(strides).foldLeft(0)({(acc,ele) => acc + (ele._1 * ele._2)})
      val index = base + tres
      println(index)
    }
  println("------------------------")
  for (k <- 0 until ksize)
    for (i <- 0 until msize)
      {
        val strides = Vector(1,msize)
        val loopvars = Vector(k,i) // (i,k)

        val tres = loopvars.reverse.zip(strides).foldLeft(0)({(acc,ele) => acc + (ele._1 * ele._2)})
        val index = base + tres
        println(index)
      }

*/
}
