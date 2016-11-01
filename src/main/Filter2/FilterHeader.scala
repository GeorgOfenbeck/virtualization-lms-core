package Filter2

trait FilterHeader extends sort.Skeleton {

  abstract class Matrix{
    val r1: Row
    val r2: Row
    val r3: Row
  }

  abstract class Row{
    val c1: OneEntry
    val c2: OneEntry
    val c3: OneEntry
  }

  abstract class OneEntry {
    type T
    type A[_]
    val a: A[T]
    val ev: IRep[A]
    val evnum: Numeric[T]
  }

  abstract class MInt{
    type A[_]
    val i: A[Int]
    val ev: IRep[A]
    val evnum: Numeric[Int]
  }

  abstract class Image{
    val xsize: MInt
    val ysize: MInt
  }


  trait RepSelector2 {
    val rrep: Boolean

    def repselect(one: OneEntry): Option[one.A[one.T]] =
    if (rrep) {
      if (one.ev.isRep()) Some(one.a) else None
    } else if (one.ev.isRep()) None else Some(one.a)

    def repselect(one: MInt): Option[one.A[Int]] =
      if (rrep) {
        if (one.ev.isRep()) Some(one.i) else None
      } else if (one.ev.isRep()) None else Some(one.i)
  }

  trait DynSelector2 extends RepSelector2{
    val rrep: Boolean = true
  }

  trait StatSelector2 extends RepSelector2{
    val rrep: Boolean = false
  }

  abstract class Base(image: Image, matrix: Matrix)
  abstract class Header(image: Image, matrix: Matrix) extends Base(image,matrix)

  case class InlineInfo(inline: Boolean, maxfunctions: Int, compareinline: Boolean, consider_inline: Boolean, specialize: Boolean, spezialize_done: Int)

  class StatFilterHeader(image: Image, matrix: Matrix, val inline: InlineInfo) extends Header(image,matrix) with StatSelector2{
    private def help(oneEntry: OneEntry): String = {
      repselect(oneEntry) match {
        case Some(x: NoRep[oneEntry.T]) => oneEntry.evnum.toLong(x).toString
        case _ => ""
      }
    }
    private def help(mInt: MInt): String = {
      repselect(mInt) match {
        case Some(x: NoRep[Int]) => x.toString
        case _ => ""
      }
    }
    def genSig(): String = {
      val ix = help(image.xsize)
      val iy = help(image.ysize)
      val as = help(matrix.r1.c1)
      val bs = help(matrix.r1.c2)
      val cs = help(matrix.r1.c3)
      val ds = help(matrix.r2.c1)
      val es = help(matrix.r2.c2)
      val fs = help(matrix.r2.c3)
      val gs = help(matrix.r3.c1)
      val hs = help(matrix.r3.c2)
      val is = help(matrix.r3.c3)
      s"_ix${ix}iy${iy}a${as}b${bs}c${cs}d${ds}e${es}f${fs}g${gs}h${hs}i${is}inline${inline.specialize}${inline.spezialize_done}"
    }
  }

  class DynFilterHeader(image_in: Rep[Image], image_out: Rep[Image], image: Image, matrix: Matrix )


  object StatFilterHeader{
    def apply(image: Image, matrix: Matrix, inlineInfo: InlineInfo) = new StatFilterHeader(image,matrix,inlineInfo)
  }

  case class MixFilterHeader(image_in: Rep[Image], image_out: Rep[Image], image: Image, matrix: Matrix, inlineInfo: InlineInfo) extends Base(image,matrix){
    def getDynHeader(): DynFilterHeader = new DynFilterHeader(image_in, image_out, image,matrix)
    def getStatHeader(): StatFilterHeader = new StatFilterHeader(image,matrix,inlineInfo)
    def split(): (StatFilterHeader,DynFilterHeader) = (getStatHeader(),getDynHeader())
  }

  object MixFilterHeader {
//    private def choose(a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get
/*
    private def choose[T1, A1[_]](a: OneEntry{ type T = T1; type A[_] = A1[_]}, b: OneEntry{ type T = T1; type A[_] = A1[_]}): A1[T1] = {
      val y: b.A[T1] = b.a
      val z: A1[b.T] = b.a
      var x: A1[T1] = b.a
      if (a.ev.isRep()) a.a else b.a
    }
    def apply(hs: StatFilterHeader)(hd: DynFilterHeader { type A = hs.A }): MixFilterHeader = {
      ???
    }*/
  }


}
