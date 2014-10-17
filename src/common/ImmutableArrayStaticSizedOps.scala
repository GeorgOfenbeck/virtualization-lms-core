package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.util.OverloadHack

trait ImmutableArrayStaticSizedOps extends ImplicitOps with OverloadHack{
  this: ImplicitOps =>

  object ImmutableArrayStaticSized {
    def apply[A:Manifest](xs: Rep[A]*) = immutablearraystaticsized_new(xs)
  }

  implicit def repToImmutableArrayStaticSizedOps[T:Manifest](a: Rep[Vector[T]]) = new ImmutableArrayStaticSizedOpsCls(a)

  class ImmutableArrayStaticSizedOpsCls[A:Manifest](l: Rep[Vector[A]]) {
  /*  def map[B:Manifest](f: Rep[A] => Rep[B]) = immutablearraystaticsized_map(l,f)
    def flatMap[B : Manifest](f: Rep[A] => Rep[Vector[B]]) = immutablearraystaticsized_flatMap(l,f)
    def filter(f: Rep[A] => Rep[Boolean]) = immutablearraystaticsized_filter(l, f)
    def sortBy[B:Manifest:Ordering](f: Rep[A] => Rep[B]) = immutablearraystaticsized_sortby(l,f)
    def ::(e: Rep[A]) = immutablearraystaticsized_prepend(l,e)
    def ++ (l2: Rep[Vector[A]]) = immutablearraystaticsized_concat(l, l2)*/
    def apply(idx: Rep[Int]) = immutablearraystaticsized_apply(l,idx)
  }


  def immutablearraystaticsized_new[A:Manifest](xs: Seq[Rep[A]]): Rep[Vector[A]]
  def immutablearraystaticsized_apply[T:Manifest](x: Rep[Vector[T]], idx: Rep[Int]): Rep[T]

  //def immutablearraystaticsized_fromseq[A:Manifest](xs: Rep[Seq[A]]): Rep[Vector[A]]
  /*
  def immutablearraystaticsized_map[A:Manifest,B:Manifest](l: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def immutablearraystaticsized_flatMap[A : Manifest, B : Manifest](xs: Rep[Vector[A]], f: Rep[A] => Rep[Vector[B]]): Rep[Vector[B]]
  def immutablearraystaticsized_filter[A : Manifest](l: Rep[Vector[A]], f: Rep[A] => Rep[Boolean]): Rep[Vector[A]]
  def immutablearraystaticsized_sortby[A:Manifest,B:Manifest:Ordering](l: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[A]]
  def immutablearraystaticsized_prepend[A:Manifest](l: Rep[Vector[A]], e: Rep[A]): Rep[Vector[A]]
  def immutablearraystaticsized_toseq[A:Manifest](l: Rep[Vector[A]]): Rep[Seq[A]]
  def immutablearraystaticsized_concat[A:Manifest](xs: Rep[Vector[A]], ys: Rep[Vector[A]]): Rep[Vector[A]] */

}


trait ImmutableArrayStaticSizedOpsExp extends ImmutableArrayStaticSizedOps with ImplicitOpsExp {
  case class ImmutableArrayStaticSizedNew[A:Manifest](xs: Seq[Rep[A]], length: Int) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedApply[A:Manifest](a: Exp[Array[A]], n: Exp[Int]) extends Def[A]
  /*case class ImmutableArrayStaticSizedFromSeq[A:Manifest](xs: Rep[Seq[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedMap[A:Manifest,B:Manifest](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[Vector[B]]
  case class ImmutableArrayStaticSizedFlatMap[A:Manifest, B:Manifest](l: Exp[List[A]], x: Sym[A], block: Block[List[B]]) extends Def[Vector[B]]
  case class ImmutableArrayStaticSizedFilter[A : Manifest](l: Exp[List[A]], x: Sym[A], block: Block[Boolean]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedSortBy[A:Manifest,B:Manifest:Ordering](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedPrepend[A:Manifest](x: Exp[List[A]], e: Exp[A]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedToArray[A:Manifest](x: Exp[List[A]]) extends Def[Array[A]]
  case class ImmutableArrayStaticSizedToSeq[A:Manifest](x: Exp[List[A]]) extends Def[Seq[A]]
  case class ImmutableArrayStaticSizedConcat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedCons[A:Manifest](x: Rep[A], xs: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedMkString[A:Manifest](l: Exp[List[A]]) extends Def[String]
  case class ImmutableArrayStaticSizedMkString2[A:Manifest](l: Exp[List[A]], s: Exp[String]) extends Def[String]
  case class ImmutableArrayStaticSizedHead[A:Manifest](xs: Rep[List[A]]) extends Def[A]
  case class ImmutableArrayStaticSizedTail[A:Manifest](xs: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayStaticSizedIsEmpty[A:Manifest](xs: Rep[List[A]]) extends Def[Boolean]*/

  def immutablearraystaticsized_new[A:Manifest](xs: Seq[Rep[A]]) = ImmutableArrayStaticSizedNew(xs, xs.length)
  //def immutablearraystaticsized_fromseq[A:Manifest](xs: Rep[Seq[A]]) = ImmutableArrayStaticSizedFromSeq(xs, xs)
  def immutablearraystaticsized_apply[T:Manifest](x: Exp[Array[T]], n: Exp[Int]): Exp[T] = ImmutableArrayStaticSizedApply(x, n)

  def immutablearraystaticsized_map[A:Manifest,B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    ??? //RF!
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListMap(l, a, b), summarizeEffects(b).star)*/
  }


  def immutablearraystaticsized_flatMap[A:Manifest, B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[List[B]]) = {
    ??? //RF!
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFlatMap(l, a, b), summarizeEffects(b).star)*/
  }
  def immutablearraystaticsized_filter[A : Manifest](l: Exp[List[A]], f: Exp[A] => Exp[Boolean]) = {
    ???
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFilter(l, a, b), summarizeEffects(b).star)*/
  }
  def immutablearraystaticsized_sortby[A:Manifest,B:Manifest:Ordering](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    ???
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListSortBy(l, a, b), summarizeEffects(b).star)*/
  }
/*  def immutablearraystaticsized_toarray[A:Manifest](l: Exp[List[A]]) = ImmutableArrayStaticSizedToArray(l)
  def immutablearraystaticsized_toseq[A:Manifest](l: Exp[List[A]]) = ImmutableArrayStaticSizedToSeq(l)
  def immutablearraystaticsized_prepend[A:Manifest](l: Exp[List[A]], e: Exp[A]) = ImmutableArrayStaticSizedPrepend(l,e)
  def immutablearraystaticsized_concat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]]) = ImmutableArrayStaticSizedConcat(xs,ys)*/


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
