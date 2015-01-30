package scala.virtualization.lms
package common


trait PureLoops extends Base { // no surface constructs for now

}

trait PureLoopsExp extends PureLoops with BaseExp  {

  abstract class AbstractLoop[A] extends Def[A]  {
    val size: Exp[Int]
    val v: Exp[Int]
    val body: Def[A]
  }
  
  case class SimpleLoop[A](val size: Exp[Int], val v: Exp[Int], val body: Def[A]) extends AbstractLoop[A]
  
  def simpleLoop[A:Manifest](size: Exp[Int], v: Exp[Int], body: Def[A]): Exp[A] = SimpleLoop(size, v, body)

  override def syms(e: Any): List[Exp[Any]] = e match {
    case e: AbstractLoop[_] => syms(e.size) ::: syms(e.body) // should add super.syms(e) ?? not without a flag ...
    case _ => super.syms(e)
  }

/*  override def readSyms(e: Any): List[Exp[Any]] = e match {
    case e: AbstractLoop[_] => readSyms(e.size) ::: readSyms(e.body)
    case _ => super.readSyms(e)
  }*/

  override def boundSyms(e: Any): List[Exp[Any]] = e match {
    case e: AbstractLoop[_] => e.v :: boundSyms(e.body)
    case _ => super.boundExps(e)
  }

  /*override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case e: AbstractLoop[_] => freqNormal(e.size) ::: freqHot(e.body) // should add super.syms(e) ?? not without a flag ...
    case _ => super.symsFreq(e)
  }*/


  //////////////
  // mirroring

  /*override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s,v,body: Def[A]) => simpleLoopCopy(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f), e)
    case Reflect(SimpleLoop(s,v,body: Def[A]), u, es) if u == Control() => reflectMirrored(Reflect(SimpleLoop(f(s),f(v).asInstanceOf[Sym[Int]],mirrorFatDef(body,f)).copyCanBeFused(e), mapOver(f,u), f(es)))(mtype(manifest[A]), pos) 
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??*/

  /////////////////////
  // aliases and sharing

/*  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => aliasSyms(e.body)
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => containSyms(e.body)
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => extractSyms(e.body)
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case e: AbstractLoop[_] => copySyms(e.body)
    case _ => super.copySyms(e)
  }*/
}


/*

trait BaseGenLoops extends GenericNestedCodegen {
  val IR: LoopsExp
  import IR._

}

trait BaseGenLoopsFat extends BaseGenLoops with BaseLoopsTraversalFat with GenericFatCodegen {
  val IR: LoopsFatExp
  import IR._

}

trait ScalaGenLoops extends ScalaGenBase with BaseGenLoops {
  import IR._

  //TODO

}

trait ScalaGenLoopsFat extends ScalaGenLoops with ScalaGenFat with BaseGenLoopsFat {
  import IR._

  //TODO

}

trait CLikeGenLoops extends CLikeGenBase with BaseGenLoops
trait CLikeGenLoopsFat extends CLikeGenLoops with CLikeGenFat with BaseGenLoopsFat

trait CGenLoops extends CGenBase with CLikeGenLoops
trait CGenLoopsFat extends CGenLoops with CGenFat with CLikeGenLoopsFat

trait GPUGenLoops extends GPUGenBase with CLikeGenLoops
trait GPUGenLoopsFat extends GPUGenLoops with GPUGenFat with CLikeGenLoopsFat 

trait CudaGenLoops extends CudaGenBase with GPUGenLoops
trait CudaGenLoopsFat extends CudaGenLoops with CudaGenFat with GPUGenLoopsFat

trait OpenCLGenLoops extends OpenCLGenBase with GPUGenLoops
trait OpenCLGenLoopsFat extends OpenCLGenLoops with OpenCLGenFat with GPUGenLoopsFat
*/
