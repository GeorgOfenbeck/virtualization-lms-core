package scala.virtualization.lms
package common

import java.io.PrintWriter

trait PureRangeOps extends Base {
  // workaround for infix not working with manifests
  implicit def repRangeToRangeOps(r: Rep[Range]) = new rangeOpsCls(r)
  class rangeOpsCls(r: Rep[Range]){
    //def foreach(f: Rep[Int] => Rep[Unit]) = range_foreach(r, f) //this does not make sense in the pure context
    //def map2[B](f: Rep[Int] => B)(implicit returns: ExposeRep[B]) = range_map(r,f)
    def map[B](f: Rep[Int] => Rep[B])(implicit b: TypeRep[B], v: TypeRep[Vector[B]]): Rep[Vector[B]] = range_map(r,f)
  }

  def infix_until(start: Rep[Int], end: Rep[Int]) = range_until(start,end)
  def infix_start(r: Rep[Range]) = range_start(r)
  def infix_step(r: Rep[Range]) = range_step(r)
  def infix_end(r: Rep[Range]) = range_end(r)
  //def infix_foreach(r: Rep[Range], f: Rep[Int] => Rep[Unit]) = range_foreach(r, f)

  def range_until(start: Rep[Int], end: Rep[Int]): Rep[Range]
  def range_start(r: Rep[Range]) : Rep[Int]
  def range_step(r: Rep[Range]) : Rep[Int]
  def range_end(r: Rep[Range]) : Rep[Int]
  //def range_map2[B](r: Rep[Range],f: Rep[Int] => B )(implicit returns: ExposeRep[B]): Vector[B]
  def range_map[B](r: Rep[Range],f: Rep[Int] => Rep[B] )(implicit b: TypeRep[B], v: TypeRep[Vector[B]]): Rep[Vector[B]]
  //def range_foreach(r: Rep[Range], f: (Rep[Int]) => Rep[Unit]): Rep[Unit]
}

trait PureRangeOpsExp extends PureRangeOps with PureFunctionsExp {
  case class Until(start: Exp[Int], end: Exp[Int]) extends Def[Range]
  case class RangeStart(r: Exp[Range]) extends Def[Int]
  case class RangeStep(r: Exp[Range]) extends Def[Int]
  case class RangeEnd(r: Exp[Range]) extends Def[Int]
  //case class RangeMap2[CR](r: Exp[Range], lam: Exp[_ => _]) extends Def[Vector[CR]]
  case class RangeMap[B: TypeRep](r: Exp[Range], lam: Exp[_ => _]) extends Def[Vector[B]]
  //case class RangeForeach(r: Exp[Range], i: Exp[Int], body: Exp[Unit]) extends Def[Unit]
  //case class RangeForeach(start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Unit]) extends Def[Unit]

  def range_until(start: Exp[Int], end: Exp[Int]) : Exp[Range] = Until(start, end)
  def range_start(r: Exp[Range]) : Exp[Int] = r match { 
    case Def(Until(start, end)) => start
    case _ => RangeStart(r)
  }
  def range_step(r: Exp[Range]) : Exp[Int] = RangeStep(r)
  def range_end(r: Exp[Range]) : Exp[Int] = r match { 
    case Def(Until(start, end)) => end
    case _ => RangeEnd(r)
  }

  def range_map[B](r: Exp[Range], f: Exp[Int] => Exp[B])(implicit b: TypeRep[B], v: TypeRep[Vector[B]]): Exp[Vector[B]] = {
    val args = exposeRepFromRep[Int]
    //val returns = exposeRepFromRep[Exp[B]](tag.tag)
    val returns = exposeRepFromRep[B](b.tag)
    val lam = fun(f)(args,returns)
    val c: Def[Vector[B]] = RangeMap[B](r,lam)
    toAtom(c)(v.tag)
  }

  /*def range_map2[B](r: Exp[Range], f: Exp[Int] => B)(implicit returnit: ExposeRep[B]): Exp[Vector[B]] = {
    val args = exposeRepFromRep[Int]
    val lam = fun(f)(args,returnit)
    val c: Def[Vector[B]] = RangeMap[B](r,lam)
    toAtom(c)
  }*/


  override def syms(e: Any): List[Exp[Any]] = e match {
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Exp[Any]] = e match {

    case _ => super.boundSyms(e)
  }
/*
  override def symsFreq(e: Any): List[(Exp[Any], Double)] = e match {
    case _ => super.symsFreq(e)
  }*/


}
