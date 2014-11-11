/*
package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.util.OverloadHack

trait ImmutableArrayStaticSizedOps extends ImplicitOps with OverloadHack{
  this: ImplicitOps =>

  object ImmutableArrayStaticSized {
    def apply[A:TypeTag](xs: Rep[A]*) = immutablearraystaticsized_new(xs)
  }

  implicit def repToImmutableArrayStaticSizedOps[T:TypeTag](a: Rep[Vector[T]]) = new ImmutableArrayStaticSizedOpsCls(a)

  class ImmutableArrayStaticSizedOpsCls[A:TypeTag](l: Rep[Vector[A]]) {
  /*  def map[B:TypeTag](f: Rep[A] => Rep[B]) = immutablearraystaticsized_map(l,f)
    def flatMap[B : TypeTag](f: Rep[A] => Rep[Vector[B]]) = immutablearraystaticsized_flatMap(l,f)
    def filter(f: Rep[A] => Rep[Boolean]) = immutablearraystaticsized_filter(l, f)
    def sortBy[B:TypeTag:Ordering](f: Rep[A] => Rep[B]) = immutablearraystaticsized_sortby(l,f)
    def ::(e: Rep[A]) = immutablearraystaticsized_prepend(l,e)
    def ++ (l2: Rep[Vector[A]]) = immutablearraystaticsized_concat(l, l2)*/
    def apply(idx: Rep[Int]) = immutablearraystaticsized_apply(l,idx)
  }


  def immutablearraystaticsized_new[A:TypeTag](xs: Seq[Rep[A]]): Rep[Vector[A]]
  def immutablearraystaticsized_apply[T:TypeTag](x: Rep[Vector[T]], idx: Rep[Int]): Rep[T]

  //def immutablearraystaticsized_fromseq[A:TypeTag](xs: Rep[Seq[A]]): Rep[Vector[A]]
  /*
  def immutablearraystaticsized_map[A:TypeTag,B:TypeTag](l: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def immutablearraystaticsized_flatMap[A : TypeTag, B : TypeTag](xs: Rep[Vector[A]], f: Rep[A] => Rep[Vector[B]]): Rep[Vector[B]]
  def immutablearraystaticsized_filter[A : TypeTag](l: Rep[Vector[A]], f: Rep[A] => Rep[Boolean]): Rep[Vector[A]]
  def immutablearraystaticsized_sortby[A:TypeTag,B:TypeTag:Ordering](l: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[A]]
  def immutablearraystaticsized_prepend[A:TypeTag](l: Rep[Vector[A]], e: Rep[A]): Rep[Vector[A]]
  def immutablearraystaticsized_toseq[A:TypeTag](l: Rep[Vector[A]]): Rep[Seq[A]]
  def immutablearraystaticsized_concat[A:TypeTag](xs: Rep[Vector[A]], ys: Rep[Vector[A]]): Rep[Vector[A]] */

}


trait ImmutableArrayStaticSizedOpsExp extends ImmutableArrayStaticSizedOps with ImplicitOpsExp {
  case class ImmutableArrayStaticSizedNew[A:TypeTag](xs: Seq[Rep[A]], length: Int) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedApply[A:TypeTag](a: Exp[Array[A]], n: Exp[Int]) extends Def[A]
  /*case class ImmutableArrayStaticSizedFromSeq[A:TypeTag](xs: Rep[Seq[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedMap[A:TypeTag,B:TypeTag](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[Vector[B]]
  case class ImmutableArrayStaticSizedFlatMap[A:TypeTag, B:TypeTag](l: Exp[List[A]], x: Sym[A], block: Block[List[B]]) extends Def[Vector[B]]
  case class ImmutableArrayStaticSizedFilter[A : TypeTag](l: Exp[List[A]], x: Sym[A], block: Block[Boolean]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedSortBy[A:TypeTag,B:TypeTag:Ordering](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedPrepend[A:TypeTag](x: Exp[List[A]], e: Exp[A]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedToArray[A:TypeTag](x: Exp[List[A]]) extends Def[Array[A]]
  case class ImmutableArrayStaticSizedToSeq[A:TypeTag](x: Exp[List[A]]) extends Def[Seq[A]]
  case class ImmutableArrayStaticSizedConcat[A:TypeTag](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedCons[A:TypeTag](x: Rep[A], xs: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedMkString[A:TypeTag](l: Exp[List[A]]) extends Def[String]
  case class ImmutableArrayStaticSizedMkString2[A:TypeTag](l: Exp[List[A]], s: Exp[String]) extends Def[String]
  case class ImmutableArrayStaticSizedHead[A:TypeTag](xs: Rep[List[A]]) extends Def[A]
  case class ImmutableArrayStaticSizedTail[A:TypeTag](xs: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedIsEmpty[A:TypeTag](xs: Rep[List[A]]) extends Def[Boolean]*/

  def immutablearraystaticsized_new[A:TypeTag](xs: Seq[Rep[A]]) = ImmutableArrayStaticSizedNew(xs, xs.length)
  //def immutablearraystaticsized_fromseq[A:TypeTag](xs: Rep[Seq[A]]) = ImmutableArrayStaticSizedFromSeq(xs, xs)
  def immutablearraystaticsized_apply[T:TypeTag](x: Exp[Array[T]], n: Exp[Int]): Exp[T] = ImmutableArrayStaticSizedApply(x, n)

  def immutablearraystaticsized_map[A:TypeTag,B:TypeTag](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    ??? //RF!
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListMap(l, a, b), summarizeEffects(b).star)*/
  }


  def immutablearraystaticsized_flatMap[A:TypeTag, B:TypeTag](l: Exp[List[A]], f: Exp[A] => Exp[List[B]]) = {
    ??? //RF!
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFlatMap(l, a, b), summarizeEffects(b).star)*/
  }
  def immutablearraystaticsized_filter[A : TypeTag](l: Exp[List[A]], f: Exp[A] => Exp[Boolean]) = {
    ???
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFilter(l, a, b), summarizeEffects(b).star)*/
  }
  def immutablearraystaticsized_sortby[A:TypeTag,B:TypeTag:Ordering](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    ???
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListSortBy(l, a, b), summarizeEffects(b).star)*/
  }
/*  def immutablearraystaticsized_toarray[A:TypeTag](l: Exp[List[A]]) = ImmutableArrayStaticSizedToArray(l)
  def immutablearraystaticsized_toseq[A:TypeTag](l: Exp[List[A]]) = ImmutableArrayStaticSizedToSeq(l)
  def immutablearraystaticsized_prepend[A:TypeTag](l: Exp[List[A]], e: Exp[A]) = ImmutableArrayStaticSizedPrepend(l,e)
  def immutablearraystaticsized_concat[A:TypeTag](xs: Rep[List[A]], ys: Rep[List[A]]) = ImmutableArrayStaticSizedConcat(xs,ys)*/


  /*
  override def syms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => syms(a):::syms(body)
    case ListFlatMap(a, _, body) => syms(a) ::: syms(body)
    case ListFilter(a, _, body) => syms(a) ::: syms(body)
    case ListSortBy(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ListMap(a, x, body) => x :: effectSyms(body)
    case ListFlatMap(_, x, body) => x :: effectSyms(body)
    case ListFilter(_, x, body) => x :: effectSyms(body)
    case ListSortBy(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ListMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ListFlatMap(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListFilter(a, _, body) => freqNormal(a) ::: freqHot(body)
    case ListSortBy(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }
  */

}
*/
