package SpiralS2

/**
  * Created by rayda on 24-Dec-16.
  */
object FuseTest extends App {

  //we always "uprank" r
  case class IMHBase(base: Int, s0: Int, s1: Int)

  def toOE(x: Int) = x

  def fuseIM(r: IMHBase, s: IMHBase, lv: Int): IMHBase = {
    val ss0 = r.s0
    val fbase = r.base + (ss0 * s.base)
    val ns0 = r.s0 * s.s0
    val ns1 = toOE(0) + (r.s0 * s.s1)
    val ns2 = r.s1
    val baseres = fbase + ns2 * lv
    IMHBase(baseres, ns0, ns1)
  }
  val r = IMHBase(0,1,0)
  val s = IMHBase(0,4,1)
  val v = 0
  val t = fuseIM(r,s,v)
  t
}
