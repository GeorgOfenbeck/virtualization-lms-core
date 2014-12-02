package scala.virtualization.lms
package internal


import scala.collection.mutable.ListBuffer
import scala.virtualization.lms.common._

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils with TypeRepBase{
  //abstract class Exp[+T:TypeRep]
  case class Exp[+T:TypeRep](val id: Int)

  /*
  case class Const[+T:TypeRep](x: T) extends Exp[T]
  case class Sym[+T:TypeRep](val id: Int) extends Exp[T]
  */

  case class ConstDef[T:TypeRep](x: T) extends Def[T]
  def Const[T:TypeRep](x: T) = toAtom(ConstDef(x))


  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  abstract class Def[+T] { // operations (composite)
  override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }

  case class TP[T](val exp: Exp[T], val rhs: Def[T], val tag: TypeRep[T])

  var nVars = 0
  def fresh[T:TypeRep]: Exp[T] = Exp[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }

  var exp2tp: Map[Exp[_], TP[_]] = Map.empty
  var def2tp: Map[Def[_], TP[_]] = Map.empty

  def reifySubGraph[T](b: =>T): (T, Vector[TP[Any]]) = {
    val r = b
    ???
  }


  def storeTP[T: TypeRep](tp: TP[T]): Unit = {
    def2tp = def2tp + (tp.rhs -> tp)
    exp2tp = exp2tp + (tp.exp -> tp)
  }

  def getTP[T: TypeRep](d: Def[T]): Option[TP[T]] = {
    def2tp.get(d).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
  }

  def getTP[T: TypeRep](exp: Exp[T]): Option[TP[T]] = {
    exp2tp.get(exp).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
  }

  def reflectSubGraph[T: TypeRep](tp: TP[T]): Unit = {
    assert(getTP(tp.rhs).isEmpty, "already defined" + tp)
    storeTP(tp)
  }

  def findDefinition[T: TypeRep](s: Exp[T]): Option[TP[T]] = getTP(s)

  def findDefinition[T: TypeRep](d: Def[T]): Option[TP[T]] = getTP(d)

  def findOrCreateDefinition[T:TypeRep](d: Def[T]): TP[T] = getTP(d).getOrElse(createDefinition(fresh[T], d))

  def findOrCreateDefinitionExp[T:TypeRep](d: Def[T]): Exp[T] = findOrCreateDefinition(d).exp

  def createDefinition[T](s: Exp[T], d: Def[T]) (implicit tag: TypeRep[T]): TP[T] = {
    val f = TP(s, d, tag)
    reflectSubGraph(f)
    f
  }

  protected implicit def toAtom[T:TypeRep](d: Def[T]): Exp[T] = {
    findOrCreateDefinitionExp(d)
  }


  // symbols which are bound in a definition
  def boundExps(e: Any): List[Exp[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundExps(_))
    case p: Product => p.productIterator.toList.flatMap(boundExps(_))
    case _ => Nil
  }

  // regular data (and effect) dependencies
  def syms(e: Any): List[Exp[Any]] = e match {
    case s: Exp[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product =>
      //return p.productIterator.toList.flatMap(syms(_))
      /* performance hotspot */
      val iter = p.productIterator
      val out = new ListBuffer[Exp[Any]]
      while (iter.hasNext) {
        val e = iter.next()
        out ++= syms(e)
      }
      out.result
    case _ => Nil
  }



  // graph construction state
  object Def {
    def unapply[T: TypeRep](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Exp(_) =>
        //findDefinition(s).flatMap(_.defines(s))
        //findDefinition(s).map( x => x.rhs.asInstanceOf[Def[T]]) //get rid of the cast
        ??? //TODO
      case _ =>
        None
    }
  }

  def reset() =  {
    nVars = 0
    exp2tp = Map.empty
    def2tp = Map.empty
  }

}
