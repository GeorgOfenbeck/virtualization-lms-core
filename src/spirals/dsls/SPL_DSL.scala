package ch.ethz.spirals.dsls



import virtualization.lms.common._



trait SPLMetaData extends BaseExp{
  import scala.reflect.runtime.universe._

  case class SPLMeta(val size: Int, override val tag: TypeTag[SPL])  extends TypeExp[SPL](tag)
}

/**
 * This is the conversion of the original SPL to the non-pointfree version
 */
trait SPL_Base extends Base with SPLMetaData{

  //=============================================================================
  // SPL Operators
  //=============================================================================
    def SPLtoRep(i: SPL): Rep[SPL] = {
      import scala.reflect.runtime.universe._

      def bla[T](x:T)(implicit tag: TypeTag[T]): TypeTag[T]= tag //RF!
      val tag: TypeTag[SPL] = bla(i)
      val meta: SPLMeta =SPLMeta(i.size, tag)
      unit(i)(meta)
    }
  implicit def mkSPLRepOps(lhs: Rep[SPL]): SPLOps = new SPLOps(lhs)


  class SPLOps(x: Rep[SPL]) {
    def tensor(y: Rep[SPL]) = infix_tensor(x, y)
    def compose(y: Rep[SPL]) = infix_compose(x, y)
  }
  def infix_tensor(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
  def infix_compose(x: Rep[SPL], y: Rep[SPL]): Rep[SPL]
}

trait SPL_Exp extends SPL_Base with BaseExp with PureFunctionsExp{
  case class DefwithMeta(d: Def[SPL], splmeta: SPLMeta)

  protected implicit def toAtomMeta(d: DefwithMeta): Exp[SPL] = {
    toAtom(d.d)(d.splmeta)
  }

  def getSPLMeta(x: Exp[SPL]): SPLMeta = {
    val t = getTP(x).map(tp => tp.tag)
    t match {
      case Some(meta: SPLMeta) => meta
      case Some(meta: TypeExp[_]) => {
        assert(false, "Seems we've lost the SPL Meta Info on the way - check the DSL and make sure every note propagates the meta data")
        ???
      }
      case _ => {
        assert(false, "apparently the Exp is not part of the internal IR - how did you manage this?")
        ???
      }
    }
  }

  def createTensorMeta(x: Exp[SPL], y: Exp[SPL]): SPLMeta = {
    val xmeta = getSPLMeta(x)
    val ymeta = getSPLMeta(y)
    val meta: SPLMeta = SPLMeta(xmeta.size * ymeta.size, xmeta.tag)
    meta
  }

  def createComposeMeta(x: Exp[SPL], y: Exp[SPL]): SPLMeta = {
    val xmeta = getSPLMeta(x)
    SPLMeta(xmeta.size, xmeta.tag)
  }

  case class Tensor(x: Exp[SPL], y: Exp[SPL]) extends Def[SPL]
  case class Compose(x: Exp[SPL], y: Exp[SPL]) extends Def[SPL]

  def infix_tensor(x: Exp[SPL], y: Exp[SPL]):Exp[SPL] = DefwithMeta(Tensor(x, y), createTensorMeta(x,y))
  def infix_compose(x: Exp[SPL], y: Exp[SPL]): Exp[SPL] = DefwithMeta(Compose(x, y), createComposeMeta(x,y))
}

class SPL_DSL extends SPL_Exp