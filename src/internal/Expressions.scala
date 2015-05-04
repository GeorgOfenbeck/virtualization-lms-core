package scala.virtualization.lms
package internal

import org.scala_lang.virtualized.SourceContext
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer
import java.lang.{StackTraceElement,Thread}


/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils {

  case class Exp[+T](id: Int) { // constants/symbols (atomic)
    var sourceContexts: Vector[SourceContext] = Vector.empty
    def pos = sourceContexts
    def withPos(pos: Vector[SourceContext]) = { sourceContexts :+ pos; this }
    def tp[T] : Manifest[T] = {
      getTP(this) match {
        case Some(ftp) => ftp.tag
        case None => assert(false, "tried to get Manifest of a symbol which apparently does not have an assoziated TP"); ???
      }
    }

  }

  type Sym[T] = Exp[T]

  case class ConstDef[T:Manifest](x: T) extends Def[T]

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  var nVars = 0
  def fresh[T:Manifest]: Exp[T] = Exp[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }
  def fresh[T:Manifest](pos: Vector[SourceContext]): Exp[T] = fresh[T].withPos(pos)

  var exp2tp: Map[Exp[_], TP[_]] = Map.empty
  var def2tp: Map[Def[_], TP[_]] = Map.empty
  var id2tp: Map[Int, TP[_]] = Map.empty


  def quotePos(e: Exp[Any]): String = {
    if(e.pos.isEmpty)
      "<unknown>"
    else
    {
      def all(cs: SourceContext): Vector[SourceContext] = cs.parent match {
        case None => Vector(cs)
        case Some(p) => cs +: all(p)
      }
      e.pos.reverse.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
    }
  }

  abstract class Def[+T] { // operations (composite)
    override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }

  // statement (links syms and definitions)
/*  abstract class Stm {
    def lhs: List[Sym[Any]] = infix_lhs(this)
    def rhs: Any = infix_rhs(this)
    def defines[A](sym: Sym[A]): Option[Def[A]] = infix_defines(this, sym)
    def defines[A](rhs: Def[A]): Option[Sym[A]] = infix_defines(this, rhs)
  }*/



  case class TP[T](sym: Exp[T], rhs: Def[T], val tag: Manifest[T]){
    def lhs: Vector[Exp[_]] = infix_lhs(this)
    def defines(sym: Exp[T]): Option[Def[T]] = infix_defines(this, sym)
    def defines(rhs: Def[T]): Option[Exp[T]] = infix_defines(this, rhs)
  }



  
  def infix_lhs[T](tp: TP[T]): Vector[Exp[_]] = Vector(tp.sym)
  def infix_rhs[T](tp: TP[T]): Any = tp.rhs


  def infix_defines[A](tp: TP[A], sym: Exp[A]): Option[Def[A]] = Some(tp.rhs)
  def infix_defines[A](tp: TP[A], rhs: Def[A]): Option[Exp[A]] = Some(tp.sym)


  def storeTP[T: Manifest](tp: TP[T]): Unit = {
    def2tp = def2tp + (tp.rhs -> tp)
    exp2tp = exp2tp + (tp.sym -> tp)
    id2tp = id2tp + (tp.sym.id -> tp)
  }

  def getTP[T: Manifest](d: Def[T]): Option[TP[T]] = {
    def2tp.get(d).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
  }

  def getTP[T: Manifest](exp: Exp[T]): Option[TP[T]] = {
    exp2tp.get(exp).asInstanceOf[Option[TP[T]]] //TODO - get rid of the type cast
  }

  // graph construction state
  
  /*var globalDefs: List[Stm] = Nil
  var localDefs: List[Stm] = Nil
  var globalDefsCache: Map[Sym[Any],Stm] = Map.empty*/

  def reflectSubGraph(tp: TP[_]): Unit = {
    assert(getTP(tp.rhs).isEmpty, "already defined" + tp)
    storeTP(tp)
  }


  /*def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    // equivalent to: globalDefs filter (_.lhs exists (lhs contains _))
    val existing = lhs flatMap (globalDefsCache get _)
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
    for (stm <- ds; s <- stm.lhs) {      
      globalDefsCache += (s->stm)
    }
  }*/



  def findDefinition[T: Manifest](s: Exp[T]): Option[TP[T]] = getTP(s)

  def findDefinition[T: Manifest](d: Def[T]): Option[TP[T]] = getTP(d)

  def findOrCreateDefinition[T:Manifest](d: Def[T], pos: Vector[SourceContext]): TP[T] = getTP(d).getOrElse(createDefinition(fresh[T](pos), d))

  def findOrCreateDefinitionExp[T:Manifest](d: Def[T], pos: Vector[SourceContext]): Exp[T] = findOrCreateDefinition(d,pos).sym

  def createDefinition[T](s: Exp[T], d: Def[T]) (implicit tag: Manifest[T]): TP[T] = {
    val f = TP(s, d, tag)
    reflectSubGraph(f)
    f
  }

  protected implicit def toAtom[T:Manifest](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
    findOrCreateDefinitionExp(d, Vector(pos))
  }


  object Def {
    def unapply[T: Manifest](e: Exp[T]): Option[Def[T]] = getTP(e) match {
      case Some(x) => Some(x.rhs)
      case None => scala.None
    }
  }

  object Const{
    def apply[T:Manifest](x: T): Exp[T] = {
      toAtom(ConstDef(x))
    }
    def unapply[T](exp: Exp[_]) = getTP(exp) match {
      case Some(x) => x.rhs match {
        case ConstDef(y) => Some(y)
        case _ => scala.None
      }
      case None => scala.None
    }
    def unapply[T](rhs: Def[_]) = getTP(rhs) match {
      case Some(x) => x.rhs match {
        case ConstDef(y) => Some(y)
        case _ => scala.None
      }
      case None => scala.None
    }
  }


  // dependencies

  // regular data (and effect) dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product =>
      // performance hotspot: this is the same as
      // p.productIterator.toList.flatMap(syms(_))
      // but faster
      val iter = p.productIterator
      val out = new ListBuffer[Sym[Any]]
      while (iter.hasNext) {
        val e = iter.next()
        out ++= syms(e)
      }
      out.result
    case _ => Nil
  }

  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }

  // symbols which are bound in a definition, but also defined elsewhere
  def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(tunnelSyms(_))
    case p: Product => p.productIterator.toList.flatMap(tunnelSyms(_))
    case _ => Nil
  }

  // symbols of effectful components of a definition
  def effectSyms(x: Any): List[Sym[Any]] = x match {
    case ss: Iterable[Any] => ss.toList.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }

  // soft dependencies: they are not required but if they occur, 
  // they must be scheduled before
  def softSyms(e: Any): List[Sym[Any]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(softSyms(_))
    case p: Product => p.productIterator.toList.flatMap(softSyms(_))
    case _ => Nil
  }

  // generic symbol traversal: f is expected to call rsyms again
  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case ss: Iterable[Any] => ss.toList.flatMap(f)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case ss: Iterable[Any] => ss.toList.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    //case _ => rsyms(e)(symsFreq)
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))


  // bookkeeping

  def reset { // used by delite?
    nVars = 0
    exp2tp = Map.empty
    def2tp = Map.empty
    id2tp = Map.empty
  }

}
