package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack

import internal._
import scala.reflect.runtime.universe._

trait OrderingOps extends Base with ImplicitOps/*with Variables*/ with OverloadHack {
  // workaround for infix not working with implicits in PrimitiveOps
  implicit def orderingToOrderingOps[T:Ordering:TypeTag](n: T) = new OrderingOpsCls(unit(n))
  implicit def repOrderingToOrderingOps[T:Ordering:TypeTag](n: Rep[T]) = new OrderingOpsCls(n)
  //implicit def varOrderingToOrderingOps[T:Ordering:TypeTag](n: Var[T]) = new OrderingOpsCls(readVar(n)) //RF!

  class OrderingOpsCls[T:Ordering:TypeTag](lhs: Rep[T]){
    def <(rhs: Rep[T]) = ordering_lt(lhs, rhs)
    def <=(rhs: Rep[T]) = ordering_lteq(lhs, rhs)
    def >(rhs: Rep[T]) = ordering_gt(lhs, rhs)
    def >=(rhs: Rep[T]) = ordering_gteq(lhs, rhs)
    def equiv(rhs: Rep[T]) = ordering_equiv(lhs, rhs)
    def max(rhs: Rep[T]) = ordering_max(lhs, rhs)
    def min(rhs: Rep[T]) = ordering_min(lhs, rhs)
    def compare(rhs: Rep[T]) = ordering_compare(lhs, rhs)

    def <[B](rhs: B)(implicit c: B => Rep[T]) = ordering_lt(lhs, c(rhs))
    def <=[B](rhs: B)(implicit c: B => Rep[T]) = ordering_lteq(lhs, c(rhs))
    def >[B](rhs: B)(implicit c: B => Rep[T]) = ordering_gt(lhs, c(rhs))
    def >=[B](rhs: B)(implicit c: B => Rep[T]) = ordering_gteq(lhs, c(rhs))
    def equiv[B](rhs: B)(implicit c: B => Rep[T]) = ordering_equiv(lhs, c(rhs))
    def max[B](rhs: B)(implicit c: B => Rep[T]) = ordering_max(lhs, c(rhs))
    def min[B](rhs: B)(implicit c: B => Rep[T]) = ordering_min(lhs, c(rhs))
    def compare[B](rhs: B)(implicit c: B => Rep[T]) = ordering_compare(lhs, c(rhs))
  }

//  def infix_<[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: TypeTag[T]) = ordering_lt(lhs,c(rhs))
//  def infix_<=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: TypeTag[T]) = ordering_lteq(lhs,c(rhs))
//  def infix_>[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: TypeTag[T]) = ordering_gt(lhs,c(rhs))
//  def infix_>=[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: TypeTag[T]) = ordering_gteq(lhs,c(rhs))
//  def infix_equiv[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: TypeTag[T]) = ordering_equiv(lhs,c(rhs))
//  def infix_max[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: TypeTag[T]) = ordering_max(lhs,c(rhs))
//  def infix_min[T,B](lhs: Rep[T], rhs: B)(implicit o: Ordering[T], c: B => Rep[T], mT: TypeTag[T]) = ordering_min(lhs,c(rhs))

  def ordering_lt[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_lteq[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gt[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_gteq[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_equiv[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[Boolean]
  def ordering_max[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def ordering_min[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[T]
  def ordering_compare[T:Ordering:TypeTag](lhs: Rep[T], rhs: Rep[T]): Rep[Int]
}


trait OrderingOpsExp extends OrderingOps with ImplicitOpsExp/*with VariablesExp */ {
  abstract class DefMN[T:Ordering:TypeTag,A] extends Def[A] {
    def mev = typeTag[T]
    def aev = implicitly[Ordering[T]]
  }
  case class OrderingLT[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingLTEQ[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingGT[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingGTEQ[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingEquiv[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Boolean]
  case class OrderingMax[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]
  case class OrderingMin[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,T]
  case class OrderingCompare[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T,Int]

  def ordering_lt[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[Boolean] = OrderingLT(lhs,rhs)
  def ordering_lteq[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[Boolean] = OrderingLTEQ(lhs,rhs)
  def ordering_gt[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[Boolean] = OrderingGT(lhs,rhs)
  def ordering_gteq[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[Boolean] = OrderingGTEQ(lhs,rhs)
  def ordering_equiv[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[Boolean] = OrderingEquiv(lhs,rhs)
  def ordering_max[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[T] = OrderingMax(lhs,rhs)
  def ordering_min[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[T] = OrderingMin(lhs,rhs)
  def ordering_compare[T:Ordering:TypeTag](lhs: Exp[T], rhs: Exp[T]): Exp[Int] = OrderingCompare(lhs,rhs)

  /*
  override def mirror[A:TypeTag](e: Def[A], f: Transformer): Exp[A] = {
    (e match {
    case e@OrderingLT(a,b) => ordering_lt(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingLTEQ(a,b) => ordering_lteq(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingGT(a,b) => ordering_gt(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingGTEQ(a,b) => ordering_gteq(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingEquiv(a,b) => ordering_equiv(f(a),f(b))(e.aev,e.mev,pos)
    case e@OrderingMax(a,b) => ordering_max(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev),pos)
    case e@OrderingMin(a,b) => ordering_min(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev),pos)
    case e@OrderingCompare(a,b) => ordering_compare(f(a),f(b))(e.aev,e.mev,pos)
    case Reflect(e@OrderingLT(a,b), u, es) => reflectMirrored(Reflect(OrderingLT(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case Reflect(e@OrderingLTEQ(a,b), u, es) => reflectMirrored(Reflect(OrderingLTEQ(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case Reflect(e@OrderingGT(a,b), u, es) => reflectMirrored(Reflect(OrderingGT(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case Reflect(e@OrderingGTEQ(a,b), u, es) => reflectMirrored(Reflect(OrderingGTEQ(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case Reflect(e@OrderingEquiv(a,b), u, es) => reflectMirrored(Reflect(OrderingEquiv(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case Reflect(e@OrderingMax(a,b), u, es) => reflectMirrored(Reflect(OrderingMax(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev)), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case Reflect(e@OrderingMin(a,b), u, es) => reflectMirrored(Reflect(OrderingMin(f(a),f(b))(e.aev.asInstanceOf[Ordering[A]],mtype(e.mev)), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case Reflect(e@OrderingCompare(a,b), u, es) => reflectMirrored(Reflect(OrderingCompare(f(a),f(b))(e.aev,e.mev), mapOver(f,u), f(es)))(mtype(typeTag[A]), pos)
    case _ => super.mirror(e, f)
    }).asInstanceOf[Exp[A]]
  } */
}
//RF
/*
trait ScalaGenOrderingOps extends ScalaGenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingLT(a,b) => emitValDef(sym, src"$a < $b")
    case OrderingLTEQ(a,b) => emitValDef(sym, src"$a <= $b")
    case OrderingGT(a,b) => emitValDef(sym, src"$a > $b")
    case OrderingGTEQ(a,b) => emitValDef(sym, src"$a >= $b")
    case OrderingEquiv(a,b) => emitValDef(sym, src"$a equiv $b")
    case OrderingMax(a,b) => emitValDef(sym, src"$a max $b")
    case OrderingMin(a,b) => emitValDef(sym, src"$a min $b")
    case c@OrderingCompare(a,b) => c.mev match {
      case m if m == TypeTag.Int => emitValDef(sym, "java.lang.Integer.compare("+quote(a)+","+quote(b)+")")
      case m if m == TypeTag.Long => emitValDef(sym, "java.lang.Long.compare("+quote(a)+","+quote(b)+")")
      case m if m == TypeTag.Double => emitValDef(sym, "java.lang.Double.compare("+quote(a)+","+quote(b)+")")
      case m if m == TypeTag.Float => emitValDef(sym, "java.lang.Float.compare("+quote(a)+","+quote(b)+")")
      case m if m == TypeTag.Boolean => emitValDef(sym, "java.lang.Boolean.compare("+quote(a)+","+quote(b)+")")
      case m if m == TypeTag.Byte => emitValDef(sym, "java.lang.Byte.compare("+quote(a)+","+quote(b)+")")
      case m if m == TypeTag.Char => emitValDef(sym, "java.lang.Character.compare("+quote(a)+","+quote(b)+")")
      case m if m == TypeTag.Short => emitValDef(sym, "java.lang.Short.compare("+quote(a)+","+quote(b)+")")
      case _ => emitValDef(sym, quote(a) + " compare " + quote(b))
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenOrderingOps extends CLikeGenBase {
  val IR: OrderingOpsExp
  import IR._
  
  // TODO: Add MIN/MAX macro needs to C-like header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case OrderingLT(a,b) =>
          emitValDef(sym, src"$a < $b")
        case OrderingLTEQ(a,b) =>
          emitValDef(sym, src"$a <= $b")
        case OrderingGT(a,b) =>
          emitValDef(sym, src"$a > $b")
        case OrderingGTEQ(a,b) =>
          emitValDef(sym, src"$a >= $b")
        case OrderingEquiv(a,b) =>
          emitValDef(sym, src"$a == $b")
        case OrderingMax(a,b) =>
          //emitValDef(sym, quote(a) + ">" + quote(b) + "?" + quote(a) + ":" + quote(b))
          emitValDef(sym, src"MAX($a, $b)")
        case OrderingMin(a,b) =>
          //emitValDef(sym, quote(a) + "<" + quote(b) + "?" + quote(a) + ":" + quote(b))
          emitValDef(sym, src"MIN($a, $b)")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenOrderingOps extends CudaGenBase with CLikeGenOrderingOps
trait OpenCLGenOrderingOps extends OpenCLGenBase with CLikeGenOrderingOps
trait CGenOrderingOps extends CGenBase with CLikeGenOrderingOps
*/
