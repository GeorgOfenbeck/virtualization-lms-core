package ch.ethz.spirals.dsls


import virtualization.lms.common._


trait SigmaSPLBase extends Base with NumericOps {
  abstract class SigmaSPLVector
  abstract class IndexMapping
  abstract class Tag
  case class Unroll() extends Tag

  def im_h(fragsize: Rep[Int], stride: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
  def im_l(fragsize: Rep[Int], stride: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
  def im_compose(x: Rep[IndexMapping], y: Rep[IndexMapping], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
  
  def gather (im: Rep[IndexMapping], x: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
  def scatter (im: Rep[IndexMapping], x: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]


  //def tag(x: Rep[SigmaSPLVector], tag: Rep[Tag]) : Rep[SigmaSPLVector]
  def sigma (n: Rep[Int], start: Rep[Int], end: Rep[Int], f: Rep[Int] => Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
  def directsum (x: Rep[SigmaSPLVector], y: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
}


trait SigmaSPLExp extends SigmaSPLBase with NumericOpsExp { self =>

  class DefIM (val range: Exp[Int], val domain : Exp[Int]) extends Def[IndexMapping]
  case class IM_H       (fragsize: Exp[Int], stride: Exp[Int],  override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
  case class IM_L       (fragsize: Exp[Int], stride: Exp[Int],  override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
  case class IM_Compose (x: Exp[IndexMapping], y: Exp[IndexMapping],  override val range: Exp[Int], override val domain: Exp[Int]) extends DefIM(range, domain)

  def im_h(fragsize: Exp[Int], stride: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_H(fragsize, stride, range, domain)
  def im_l(fragsize: Exp[Int], stride: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_L(fragsize, stride, range, domain)
  def im_compose(x: Exp[IndexMapping], y: Exp[IndexMapping], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_Compose(x, y, range, domain)

  case class Tag   (body: Exp[SigmaSPLVector], tagType: Exp[Tag]) extends Def[SigmaSPLVector]
  case class Gather     (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
  case class Scatter    (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]  
  case class DirectSum  (x: Exp[SigmaSPLVector], y: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
  case class Sigma      (n: Exp[Int], start: Exp[Int], end: Exp[Int], i: Exp[Int], body: Block)  extends Def[SigmaSPLVector]

  def tag(x: Exp[SigmaSPLVector], tag: Exp[Tag]): Exp[SigmaSPLVector] = Tag(x,tag)
  def directsum (x: Exp[SigmaSPLVector], y: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = DirectSum(x, y)
  def gather (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = Gather (im, x)
  def scatter (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = Scatter (im, x)

  def sigma(n: Exp[Int], start: Exp[Int], end: Exp[Int], f: Exp[Int] => Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = {
    ???
  }
}
