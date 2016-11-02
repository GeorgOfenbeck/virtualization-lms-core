
package Filter2

trait FilterHeader extends sort.Skeleton {

  abstract class Matrix {
    val r1: Row
    val r2: Row
    val r3: Row

    def cpy(m: Matrix, a: Row = null, b: Row = null, c: Row = null): Matrix = {
      val a1: Row = if (a == null) m.r1 else a
      val a2: Row = if (b == null) m.r2 else b
      val a3: Row = if (c == null) m.r3 else c
      new Matrix {
        val r1 = a1
        val r2 = a2
        val r3 = a3
      }
    }
  }

  abstract class Row {
    val c1: OneEntry
    val c2: OneEntry
    val c3: OneEntry

    def cpy(r: Row, a: OneEntry = null, b: OneEntry = null, c: OneEntry = null): Row = {
      val a1 = if (a == null) r.c1 else a
      val a2 = if (b == null) r.c2 else b
      val a3 = if (c == null) r.c3 else c
      new Row {
        val c1 = a1
        val c2 = a2
        val c3 = a3
      }
    }
  }


  abstract class OneEntry {
    self =>
    type T
    type A[_]
    val a: A[T]
    val ev: IRep[A]
    val evnum: Numeric[T]
    val evtyp: TypeRep[T]

    def cpy[T1: Numeric, A1[_] : IRep](n: A1[T1]) = {
      new OneEntry {
        override type A[X] = A1[X]
        override type T = T1
        override val evnum: Numeric[T1] = ???
        override val ev: IRep[A] = ???
        override val evtyp: TypeRep[T] = ???
        override val a: this.A[this.T] = n
      }
    }

    def makeSome(me: OneEntry): OptionalEntry = {
      new OptionalEntry {
        override type A[X] = me.A[X]
        override type T = me.T
        override val evnum: Numeric[T] = me.evnum
        override val ev: IRep[A] = me.ev
        override val a: Option[A[T]] = Some(me.a)
        override val evtyp = me.evtyp
      }
    }

    def makeNone(me: OneEntry): OptionalEntry = {
      new OptionalEntry {
        override type A[X] = me.A[X]
        override type T = me.T
        override val evnum: Numeric[T] = me.evnum
        override val ev: IRep[A] = me.ev
        override val a: Option[A[T]] = None
        override val evtyp = me.evtyp
      }
    }
  }

  abstract class OptionalEntry {
    self =>
    type T
    type A[_]
    val a: Option[A[T]]
    val ev: IRep[A]
    val evnum: Numeric[T]
    val evtyp: TypeRep[T]

    def toOneEntry(): Option[OneEntry] = {
      if (a.isDefined)
        Some(new OneEntry {
          override type A[X] = self.A[X]
          override type T = self.T
          override val evnum: Numeric[T] = self.evnum
          override val ev: IRep[A] = self.ev
          override val a: A[T] = self.a.get
          override val evtyp = self.evtyp
        })
      else None
    }
  }


  abstract class MInt {
    type A[_]
    val i: A[Int]
    val ev: IRep[A]
    val evnum: Numeric[Int]
  }

  abstract class Image {
    val xsize: MInt
    val ysize: MInt
  }


  trait RepSelector2 {
    val rrep: Boolean

    def repselect(one: OneEntry): OptionalEntry =
      if (rrep) {
        if (one.ev.isRep()) one.makeSome(one) else one.makeNone(one)
      } else if (one.ev.isRep()) one.makeNone(one) else one.makeSome(one)

    def repselect2(one: MInt): Option[one.A[Int]] =
      if (rrep) {
        if (one.ev.isRep()) Some(one.i) else None
      } else if (one.ev.isRep()) None else Some(one.i)
  }

  trait DynSelector2 extends RepSelector2 {
    val rrep: Boolean = true
  }

  trait StatSelector2 extends RepSelector2 {
    val rrep: Boolean = false
  }

  abstract class Base(image: Image, matrix: Matrix)

  abstract class Header(val image: Image, val matrix: Matrix) extends Base(image, matrix) with RepSelector2 {
    def a() = repselect(matrix.r1.c1)

    def b() = repselect(matrix.r1.c2)

    def c() = repselect(matrix.r1.c3)

    def d() = repselect(matrix.r2.c1)

    def e() = repselect(matrix.r2.c2)

    def f() = repselect(matrix.r2.c3)

    def g() = repselect(matrix.r3.c1)

    def h() = repselect(matrix.r3.c2)

    def i() = repselect(matrix.r3.c3)

  }

  case class InlineInfo(inline: Boolean, maxfunctions: Int, compareinline: Boolean, consider_inline: Boolean, specialize: Boolean, spezialize_done: Int)

  class StatFilterHeader(image: Image, matrix: Matrix, val inline: InlineInfo) extends Header(image, matrix) with StatSelector2 {
    private def help(oneEntry: OneEntry): String = {
      val t = repselect(oneEntry)
      t.a match {
        case Some(x: NoRep[t.T]) => t.evnum.toLong(x).toString
        case _ => ""
      }
    }

    private def help(mInt: MInt): String = {
      repselect2(mInt) match {
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

  class DynFilterHeader(val image_in: Rep[Image], val image_out: Rep[Image], image: Image, matrix: Matrix) extends Header(image, matrix) with DynSelector2


  object StatFilterHeader {
    def apply(image: Image, matrix: Matrix, inlineInfo: InlineInfo) = new StatFilterHeader(image, matrix, inlineInfo)
  }

  case class MixFilterHeader(image_in: Rep[Image], image_out: Rep[Image], image: Image, matrix: Matrix, inlineInfo: InlineInfo) extends Base(image, matrix) {
    def getDynHeader(): DynFilterHeader = new DynFilterHeader(image_in, image_out, image, matrix)

    def getStatHeader(): StatFilterHeader = new StatFilterHeader(image, matrix, inlineInfo)

    def split(): (StatFilterHeader, DynFilterHeader) = (getStatHeader(), getDynHeader())
  }

  object MixFilterHeader {

    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]], ev: IRep[A]): A[T] = if (ev.isRep()) b.get else a.get

    private def choose[A[_], T](a: Option[A[T]], b: Option[A[T]]) = {
      if (a.isDefined) a.get else b.get
    }

    private def choose[T1, A1[_]](a: OneEntry, b: OneEntry): A1[T1] = {
      if (a.ev.isRep()) a.a else b.a

      ???
    }


    def apply(hs: StatFilterHeader, hd: DynFilterHeader): MixFilterHeader = {
      val a = hs.a().toOneEntry().getOrElse(hd.a().toOneEntry().get)
      val b = hs.b().toOneEntry().getOrElse(hd.b().toOneEntry().get)
      val c = hs.c().toOneEntry().getOrElse(hd.c().toOneEntry().get)

      val d = hs.d().toOneEntry().getOrElse(hd.d().toOneEntry().get)
      val e = hs.e().toOneEntry().getOrElse(hd.e().toOneEntry().get)
      val f = hs.f().toOneEntry().getOrElse(hd.f().toOneEntry().get)

      val g = hs.g().toOneEntry().getOrElse(hd.g().toOneEntry().get)
      val h = hs.h().toOneEntry().getOrElse(hd.h().toOneEntry().get)
      val i = hs.i().toOneEntry().getOrElse(hd.i().toOneEntry().get)

      val nm = new Matrix {
        val r1 = new Row {
          val c1 = a
          val c2 = b
          val c3 = c
        }
        val r2 = new Row {
          val c1 = d
          val c2 = e
          val c3 = f
        }
        val r3 = new Row {
          val c1 = g
          val c2 = h
          val c3 = i
        }
      }

      new MixFilterHeader(hd.image_in, hd.image_out, hd.image, nm, hs.inline)
    }

  }

  implicit def exposeDynHeader(stat: StatFilterHeader): ExposeRep[DynFilterHeader] = new ExposeRep[DynFilterHeader] {
    val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
      Vector(Arg[Image]) ++ Vector(Arg[Image]) ++
        stat.matrix.r1.c1.ev.fresh[stat.matrix.r1.c1.T]()(stat.matrix.r1.c1.evtyp) ++
        stat.matrix.r1.c2.ev.fresh[stat.matrix.r1.c2.T]()(stat.matrix.r1.c2.evtyp) ++
        stat.matrix.r1.c3.ev.fresh[stat.matrix.r1.c3.T]()(stat.matrix.r1.c3.evtyp) ++
        stat.matrix.r2.c1.ev.fresh[stat.matrix.r2.c1.T]()(stat.matrix.r2.c1.evtyp) ++
        stat.matrix.r2.c2.ev.fresh[stat.matrix.r2.c2.T]()(stat.matrix.r2.c2.evtyp) ++
        stat.matrix.r2.c3.ev.fresh[stat.matrix.r2.c3.T]()(stat.matrix.r2.c3.evtyp) ++
        stat.matrix.r3.c1.ev.fresh[stat.matrix.r3.c1.T]()(stat.matrix.r3.c1.evtyp) ++
        stat.matrix.r3.c2.ev.fresh[stat.matrix.r3.c2.T]()(stat.matrix.r3.c2.evtyp) ++
        stat.matrix.r3.c3.ev.fresh[stat.matrix.r3.c3.T]()(stat.matrix.r3.c3.evtyp)

      //need to add image dimensions
    }
    val t2vec: DynFilterHeader => Vector[Exp[_]] = (in: DynFilterHeader) => {
      def help(ele: Option[OneEntry]): Vector[Exp[_]] = {
        ele.map(p => p.ev.getRep(p.a).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
        //ele.map(p => ev.getRep(p).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
      }
      Vector(in.image_in) ++ Vector(in.image_out) ++
        help(in.a().toOneEntry()) ++
        help(in.b().toOneEntry()) ++
        help(in.c().toOneEntry()) ++
        help(in.d().toOneEntry()) ++
        help(in.e().toOneEntry()) ++
        help(in.f().toOneEntry()) ++
        help(in.g().toOneEntry()) ++
        help(in.h().toOneEntry()) ++
        help(in.i().toOneEntry())

      ??? //still need to add image dimensions
    }

    val vec2t: Vector[Exp[_]] => DynFilterHeader = (in: Vector[Exp[_]]) => {
      def help[T[_], A: TypeRep](in: Vector[Rep[_]], statele: OptionalEntry): (Vector[Rep[_]], OneEntry) = {


        val (vecafter, ele) = statele.ev.fetch[statele.T](in)

        val res = ele.getOrElse(statele.a.get)
        val or = new OneEntry {
          override type A[X] = statele.A[X]
          override type T = statele.T
          override val evnum: Numeric[T] = statele.evnum
          override val ev: IRep[A] = statele.ev
          override val a: A[T] = statele.a.get
          override val evtyp = statele.evtyp
        }
        (vecafter, or)

      }
      val image_in = in.head.asInstanceOf[Rep[Image]]
      val image_out = in.tail.head.asInstanceOf[Rep[Image]]
      val (oa, a) = help(in.tail.tail, stat.matrix.r1.c1)
      val (ob, b) = help(oa, stat.b, implicitly[IRep[B]])
      val (oc, c) = help(ob, stat.c, implicitly[IRep[C]])
      val (od, d) = help(oc, stat.d, implicitly[IRep[D]])
      val (oe, e) = help(od, stat.e, implicitly[IRep[E]])
      val (of, f) = help(oe, stat.f, implicitly[IRep[F]])
      val (og, g) = help(of, stat.g, implicitly[IRep[G]])
      val (oh, h) = help(og, stat.h, implicitly[IRep[H]])
      val (oi, i) = help(oh, stat.i, implicitly[IRep[I]])

      val t: DynFilterHeader[T, A, B, C, D, E, F, G, H, I] = new DynFilterHeader[T, A, B, C, D, E, F, G, H, I](image_in, image_out, a, b, c, d, e, f, g, h, i)
      t

    }

  }


}

