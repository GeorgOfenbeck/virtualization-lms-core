package scala.virtualization.lms
package internal






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

  /*case class Const[+T:TypeRep](x: T) extends Exp[T]

  case class Sym[+T:TypeRep](val id: Int) extends Exp[T]*/

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  abstract class Def[+T] { // operations (composite)
  override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }

  case class TP[T](val sym: Sym[T], val rhs: Def[T], val tag: TypeRep[T])

  var nVars = 0
  def fresh[T:TypeRep]: Sym[T] = Sym[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }

  var sym2tp: Map[Sym[_], TP[_]] = Map.empty
  var def2tp: Map[Def[_], TP[_]] = Map.empty

  def reifySubGraph[T](b: =>T): (T, Vector[TP[Any]]) = {
    val r = b
    ???
  }


  def storeTP[T: TypeRep](tp: TP[T]): Unit = {
    def2tp = def2tp + (tp.rhs -> tp)
    sym2tp = sym2tp + (tp.sym -> tp)
  }

  def getTP[T: TypeRep](d: Def[T]): Option[TP[T]] = {
    def2tp.get(d).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
  }

  def getTP[T: TypeRep](sym: Sym[T]): Option[TP[T]] = {
    sym2tp.get(sym).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
  }

  def reflectSubGraph[T: TypeRep](tp: TP[T]): Unit = {
    assert(getTP(tp.rhs).isEmpty, "already defined" + tp)
    storeTP(tp)
  }

  def findDefinition[T: TypeRep](s: Sym[T]): Option[TP[T]] = getTP(s)

  def findDefinition[T: TypeRep](d: Def[T]): Option[TP[T]] = getTP(d)

  def findOrCreateDefinition[T:TypeRep](d: Def[T]): TP[T] = getTP(d).getOrElse(createDefinition(fresh[T], d))

  def findOrCreateDefinitionExp[T:TypeRep](d: Def[T]): Exp[T] = findOrCreateDefinition(d).sym

  def createDefinition[T](s: Sym[T], d: Def[T]) (implicit tag: TypeRep[T]): TP[T] = {
    val f = TP(s, d, tag)
    reflectSubGraph(f)
    f
  }

  protected implicit def toAtom[T:TypeRep](d: Def[T]): Exp[T] = {
    findOrCreateDefinitionExp(d)
  }


  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }



  // graph construction state
  object Def {
    def unapply[T: TypeRep](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        //findDefinition(s).flatMap(_.defines(s))
        //findDefinition(s).map( x => x.rhs.asInstanceOf[Def[T]]) //get rid of the cast
        ??? //TODO
      case _ =>
        None
    }
  }

  def reset() =  {
    nVars = 0
    sym2tp = Map.empty
    def2tp = Map.empty
  }

}
