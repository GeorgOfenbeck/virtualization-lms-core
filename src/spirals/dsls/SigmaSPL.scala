package ch.ethz.spirals.dsls


import virtualization.lms.common._


trait SigmaSPLMetaData extends BaseExp{
  import scala.reflect.runtime.universe._
  abstract class SigmaSPLVector
  case class SigmaSPLMeta(val size: Int, override val tag: TypeTag[SigmaSPLVector])  extends TypeExp[SigmaSPLVector](tag)
}


trait SigmaSPLBase extends Base with NumericOps with SigmaSPLMetaData{

  abstract class IndexMapping
  abstract class Tag
  case class Unroll() extends Tag

  def im_h(fragsize: Rep[Int], stride: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
  def im_l(fragsize: Rep[Int], stride: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
  def im_compose(x: Rep[IndexMapping], y: Rep[IndexMapping], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
  
  def gather (im: Rep[IndexMapping], x: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
  def scatter (im: Rep[IndexMapping], x: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]

  def imh_h(fragsize: Rep[Int], base: Rep[Int], strides: Vector[Rep[Int]], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]

  def gt(
         x: Rep[SigmaSPLVector],
         A: Rep[SigmaSPLVector],
         g: Rep[IndexMapping],
         s: Rep[IndexMapping],
         v: Vector[Int]): Rep[SigmaSPLVector]
  //def tag(x: Rep[SigmaSPLVector], tag: Rep[Tag]) : Rep[SigmaSPLVector]
  def sigma (n: Rep[Int], start: Rep[Int], end: Rep[Int], f: Rep[Int] => Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
  def directsum (x: Rep[SigmaSPLVector], y: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
}


trait SigmaSPLExp extends SigmaSPLBase with NumericOpsExp { self =>

  case class DefwithMeta(d: Def[SigmaSPLVector], sigmasplmeta: SigmaSPLMeta)
  protected implicit def toAtomMeta(d: DefwithMeta): Exp[SigmaSPLVector] = {
    toAtom(d.d)(d.sigmasplmeta)
  }



  def getSigmaSPLMeta(x: Exp[SigmaSPLVector]): SigmaSPLMeta = {
    val t = getTP(x).map(tp => tp.tag)
    t match {
      case Some(meta: SigmaSPLMeta) => meta
      case Some(meta: TypeExp[_]) => {
        assert(false, "Seems we've lost the SigmaSPL Meta Info on the way - check the DSL and make sure every note propagates the meta data")
        ???
      }
      case _ => {
        assert(false, "apparently the Exp is not part of the internal IR - how did you manage this?")
        ???
      }
    }
  }


  class DefIM (val range: Exp[Int], val domain : Exp[Int]) extends Def[IndexMapping]
  case class IM_H       (fragsize: Exp[Int], stride: Exp[Int],  override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
  case class IM_L       (fragsize: Exp[Int], stride: Exp[Int],  override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
  case class IM_Compose (x: Exp[IndexMapping], y: Exp[IndexMapping],  override val range: Exp[Int], override val domain: Exp[Int]) extends DefIM(range, domain)

  def im_h(fragsize: Exp[Int], stride: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_H(fragsize, stride, range, domain)
  def im_l(fragsize: Exp[Int], stride: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_L(fragsize, stride, range, domain)
  def im_compose(x: Exp[IndexMapping], y: Exp[IndexMapping], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_Compose(x, y, range, domain)


  case class IMH_H  (fragsize: Exp[Int], base: Exp[Int], strides: Vector[Exp[Int]], override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range,domain)

  def imh_h(fragsize: Exp[Int], base: Exp[Int], strides: Vector[Exp[Int]], range: Exp[Int], domain: Exp[Int]) = IMH_H(fragsize,base,strides,range,domain)
  

  case class Tag   (body: Exp[SigmaSPLVector], tagType: Exp[Tag]) extends Def[SigmaSPLVector]
  case class Gather     (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
  case class Scatter    (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]  
  case class DirectSum  (x: Exp[SigmaSPLVector], y: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
  case class Sigma      (n: Exp[Int], start: Exp[Int], end: Exp[Int], i: Exp[Int], body: Block)  extends Def[SigmaSPLVector]
  
  case class GT (
                 x: Exp[SigmaSPLVector],
                 A: Exp[SigmaSPLVector],
                 g: Exp[IndexMapping],
                 s: Exp[IndexMapping],
                 v: Vector[Int]
                  ) extends Def[SigmaSPLVector]

  def tag(x: Exp[SigmaSPLVector], tag: Exp[Tag]): Exp[SigmaSPLVector] = Tag(x,tag)
  def directsum (x: Exp[SigmaSPLVector], y: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = DirectSum(x, y)
  def gather (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = Gather (im, x)
  def scatter (im: Exp[IndexMapping], x: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = Scatter (im, x)

  def gt (
           x: Exp[SigmaSPLVector],
           A: Exp[SigmaSPLVector],
          g: Exp[IndexMapping],
          s: Exp[IndexMapping],
          v: Vector[Int]) = DefwithMeta(GT(x,A,g,s,v),getSigmaSPLMeta(x))

  def sigma(n: Exp[Int], start: Exp[Int], end: Exp[Int], f: Exp[Int] => Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = {
    ???
  }
}
