package scala.virtualization.lms
package common

import java.io.PrintWriter

import scala.virtualization.lms.util.OverloadHack

trait ImmutableArrayOps extends ImplicitOps with OverloadHack{
  this: ImplicitOps =>

  object ImmutableArray {
    def apply[A:Manifest](xs: Rep[A]*) = immutablearray_new(xs)
  }
  
  implicit def repToListOps[T:Manifest](a: Rep[Vector[T]]) = new ImmutableArrayOpsCls(a)

  class ImmutableArrayOpsCls[A:Manifest](l: Rep[Vector[A]]) {
    def map[B:Manifest](f: Rep[A] => Rep[B]) = immutablearray_map(l,f)
    def flatMap[B : Manifest](f: Rep[A] => Rep[Vector[B]]) = immutablearray_flatMap(l,f)
    def filter(f: Rep[A] => Rep[Boolean]) = immutablearray_filter(l, f)
    def sortBy[B:Manifest:Ordering](f: Rep[A] => Rep[B]) = immutablearray_sortby(l,f)
    def ::(e: Rep[A]) = immutablearray_prepend(l,e)
    def ++ (l2: Rep[Vector[A]]) = immutablearray_concat(l, l2)    
  }
  
  
  def immutablearray_new[A:Manifest](xs: Seq[Rep[A]]): Rep[Vector[A]]
  def immutablearray_fromseq[A:Manifest](xs: Rep[Seq[A]]): Rep[Vector[A]]
  def immutablearray_map[A:Manifest,B:Manifest](l: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[B]]
  def immutablearray_flatMap[A : Manifest, B : Manifest](xs: Rep[Vector[A]], f: Rep[A] => Rep[Vector[B]]): Rep[Vector[B]]
  def immutablearray_filter[A : Manifest](l: Rep[Vector[A]], f: Rep[A] => Rep[Boolean]): Rep[Vector[A]]
  def immutablearray_sortby[A:Manifest,B:Manifest:Ordering](l: Rep[Vector[A]], f: Rep[A] => Rep[B]): Rep[Vector[A]]
  def immutablearray_prepend[A:Manifest](l: Rep[Vector[A]], e: Rep[A]): Rep[Vector[A]]
  def immutablearray_toseq[A:Manifest](l: Rep[Vector[A]]): Rep[Seq[A]]
  def immutablearray_concat[A:Manifest](xs: Rep[Vector[A]], ys: Rep[Vector[A]]): Rep[Vector[A]]
}


trait ImmutableArrayOpsExp extends ImmutableArrayOps with ImplicitOpsExp {
  case class ImmutableArrayNew[A:Manifest](xs: Seq[Rep[A]]) extends Def[Vector[A]]
  case class ImmutableArrayFromSeq[A:Manifest](xs: Rep[Seq[A]]) extends Def[Vector[A]]
  case class ImmutableArrayMap[A:Manifest,B:Manifest](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[Vector[B]]
  case class ImmutableArrayFlatMap[A:Manifest, B:Manifest](l: Exp[List[A]], x: Sym[A], block: Block[List[B]]) extends Def[Vector[B]]
  case class ImmutableArrayFilter[A : Manifest](l: Exp[List[A]], x: Sym[A], block: Block[Boolean]) extends Def[Vector[A]]
  case class ImmutableArraySortBy[A:Manifest,B:Manifest:Ordering](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[Vector[A]]
  case class ImmutableArrayPrepend[A:Manifest](x: Exp[List[A]], e: Exp[A]) extends Def[Vector[A]]
  case class ImmutableArrayToArray[A:Manifest](x: Exp[List[A]]) extends Def[Array[A]]
  case class ImmutableArrayToSeq[A:Manifest](x: Exp[List[A]]) extends Def[Seq[A]]
  case class ImmutableArrayConcat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayCons[A:Manifest](x: Rep[A], xs: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayMkString[A:Manifest](l: Exp[List[A]]) extends Def[String]
  case class ImmutableArrayMkString2[A:Manifest](l: Exp[List[A]], s: Exp[String]) extends Def[String]
  case class ImmutableArrayHead[A:Manifest](xs: Rep[List[A]]) extends Def[A]
  case class ImmutableArrayTail[A:Manifest](xs: Rep[List[A]]) extends Def[Vector[A]]
  case class ImmutableArrayIsEmpty[A:Manifest](xs: Rep[List[A]]) extends Def[Boolean]

  def immutablearray_new[A:Manifest](xs: Seq[Rep[A]]) = ImmutableArrayNew(xs)
  def immutablearray_fromseq[A:Manifest](xs: Rep[Seq[A]]) = ImmutableArrayFromSeq(xs)


  def immutablearray_map[A:Manifest,B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    ??? //RF!
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListMap(l, a, b), summarizeEffects(b).star)*/
  }


  def immutablearray_flatMap[A:Manifest, B:Manifest](l: Exp[List[A]], f: Exp[A] => Exp[List[B]]) = {
    ??? //RF!
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFlatMap(l, a, b), summarizeEffects(b).star)*/
  }
  def immutablearray_filter[A : Manifest](l: Exp[List[A]], f: Exp[A] => Exp[Boolean]) = {
    ???
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListFilter(l, a, b), summarizeEffects(b).star)*/
  }
  def immutablearray_sortby[A:Manifest,B:Manifest:Ordering](l: Exp[List[A]], f: Exp[A] => Exp[B]) = {
    ???
    /*val a = fresh[A]
    val b = reifyEffects(f(a))
    reflectEffect(ListSortBy(l, a, b), summarizeEffects(b).star)*/
  }
  def immutablearray_toarray[A:Manifest](l: Exp[List[A]]) = ImmutableArrayToArray(l)
  def immutablearray_toseq[A:Manifest](l: Exp[List[A]]) = ImmutableArrayToSeq(l)
  def immutablearray_prepend[A:Manifest](l: Exp[List[A]], e: Exp[A]) = ImmutableArrayPrepend(l,e)
  def immutablearray_concat[A:Manifest](xs: Rep[List[A]], ys: Rep[List[A]]) = ImmutableArrayConcat(xs,ys)


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
