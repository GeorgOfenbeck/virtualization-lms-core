package scala.virtualization.lms
package internal


import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.IntMap
import java.lang.{StackTraceElement,Thread}
import scala.reflect.runtime.universe._
import scala.virtualization.lms.common.TypeRepBase

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils with TypeRepBase{


  abstract class Exp[+T:TypeRep] { // constants/symbols (atomic)
  //def tp: TypeRep[T @uncheckedVariance] = typeRep[T] //invariant position! but hey...
  }

  case class Const[+T:TypeRep](x: T) extends Exp[T]

  case class Sym[+T:TypeRep](val id: Int) extends Exp[T]

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  abstract class Def[+T] { // operations (composite)
  override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }
  /*abstract class Stm // statement (links syms and definitions)
  {
    def lhs(): List[Sym[Any]] = this match {
      case TP(sym, rhs) => sym::Nil
    }

    def xrhs(): Any = this match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
      case TP(sym, rhs) => rhs
    }

    def defines[A]( sym: Sym[A]): Option[Def[A]] = this match {
      case TP(`sym`, rhs: Def[A]) => Some(rhs)
      case _ => None
    }

    def defines[A](rhs: Def[A]): Option[Sym[A]] = this match {
      case TP(sym: Sym[A], `rhs`) => Some(sym)
      case _ => None
    }
  }*/

  case class TP[+T](val sym: Sym[T], val rhs: Def[T]) //, val tag: TypeRep[T])



  var nVars = 0
  def fresh[T:TypeRep]: Sym[T] = Sym[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }
  //def fresh[T:TypeTag]: Sym[T] = fresh[T]


  var globalDefs: Vector[TP[Any]] = Vector()
  var localDefs: Vector[TP[Any]] = Vector()

  var sym2tp: Map[Sym[Any], TP[Any]] = Map.empty
  var def2tp: Map[Def[Any], TP[Any]] = Map.empty


  def reifySubGraph[T](b: =>T): (T, Vector[TP[Any]]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    //val saveGlobalCache = globalDefsCache
    localDefs = Vector()
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    //globalDefsCache = saveGlobalCache
    (r, defs)
  }

  def reflectSubGraph[T: TypeRep](ds: Vector[TP[Any]]): Unit = {
    val lhs = ds.map(_.sym)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    lhs.map( p => assert(sym2tp.contains(p), "already defined: " + sym2tp(p) + " for " + ds))
    val (tsym2tp, tdef2tp) = ds.foldLeft((sym2tp,def2tp))((acc,ele) => {
      val (lsym2tp,ldef2tp) = acc               //take the existing Hashmaps and add all new Symbols to them
      val nsym2tp = lsym2tp + (ele.sym -> ele)  //in the end replace the existing HashMaps
      val ndef2tp = ldef2tp + (ele.rhs -> ele)
      (nsym2tp,ndef2tp)
    })
    sym2tp = tsym2tp
    def2tp = tdef2tp

    localDefs = localDefs ++ ds
    globalDefs = globalDefs ++ ds
  }




  def findDefinition[T](s: Sym[T]): Option[TP[Any]] = sym2tp.get(s)

  def findDefinition[T](d: Def[T]): Option[TP[Any]] = def2tp.get(d)

  def findOrCreateDefinition[T:TypeRep](d: Def[T]): TP[Any] = def2tp.get(d).getOrElse(createDefinition(fresh[T], d))

  def findOrCreateDefinitionExp[T:TypeRep](d: Def[T]): Exp[T] = {
    val t = findOrCreateDefinition(d).sym
    t.asInstanceOf[Sym[T]]
    //TODO:
    //this is safe since a Sym[T] always has the same type then the according Def[T]
    //still would be nice to not have to cast here
    //put typetag as part of the TP - rewrite this to use the tag for the conversion
  }


  def createDefinition[T](s: Sym[T], d: Def[T]) /*(implicit tag: TypeRep[T])*/: TP[T] = {
    val f = TP(s, d) //, tag)
    reflectSubGraph(Vector(f))
    //reflectSubGraph(f)
    f
  }



  protected implicit def toAtom[T:TypeRep](d: Def[T]): Exp[T] = {
    findOrCreateDefinitionExp(d) // TBD: return Const(()) if type is Unit??
  }


  // dependencies

  // regular data (and effect) dependencies
  def syms(e: Any): Vector[Sym[Any]] = e match {
    case s: Sym[Any] => Vector(s)
    case ss: Iterable[Any] => ss.toVector.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product =>
      //return p.productIterator.toList.flatMap(syms(_))
      /* performance hotspot */
      val iter = p.productIterator
      val out = new ListBuffer[Sym[Any]]
      while (iter.hasNext) {
        val e = iter.next()
        out ++= syms(e)
      }
      out.result().toVector
    case _ => Vector()
  }

  // symbols which are bound in a definition
  def boundSyms(e: Any): Vector[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toVector.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(boundSyms(_))
    case _ => Vector()
  }

  // symbols which are bound in a definition, but also defined elsewhere
  def tunnelSyms(e: Any): Vector[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toVector.flatMap(tunnelSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(tunnelSyms(_))
    case _ => Vector()
  }

  // symbols of effectful components of a definition
  def effectSyms(x: Any): Vector[Sym[Any]] = x match {
    case ss: Iterable[Any] => ss.toVector.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(effectSyms(_))
    case _ => Vector()
  }

  // soft dependencies: they are not required but if they occur, 
  // they must be scheduled before
  def softSyms(e: Any): Vector[Sym[Any]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toVector.flatMap(softSyms(_))
    case p: Product => p.productIterator.toVector.flatMap(softSyms(_))
    case _ => Vector()
  }


  def rsyms[T](e: Any)(f: Any=>Vector[T]): Vector[T] = e match {
    case s: Sym[Any] => f(s)
    case ss: Iterable[Any] => ss.toVector.flatMap(f)
    case p: Product => p.productIterator.toVector.flatMap(f)
    case _ => Vector()
  }

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): Vector[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => Vector((s,1.0))
    case ss: Iterable[Any] => ss.toVector.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toVector.flatMap(symsFreq(_))
    //case _ => rsyms(e)(symsFreq)
    case _ => Vector()
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))





  // graph construction state
  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        //findDefinition(s).flatMap(_.defines(s))
        findDefinition(s).map( x => x.rhs.asInstanceOf[Def[T]]) //get rid of the cast
      case _ =>
        None
    }
  }



  def reset { // used by delite?
    nVars = 0
    globalDefs = Vector()
    localDefs = Vector()
    //globalDefsCache = IntMap.empty
  }

}
