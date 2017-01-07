package SpiralS2

trait Header extends Skeleton {

  implicit def mkDataEleOops(lhs: DataEle) = new DataEleOps(lhs)

  class DataEleOps(lhs: DataEle) {
    def +(rhs: DataEle) = lhs.dplus(lhs, rhs)

    def -(rhs: DataEle) = lhs.dminus(lhs, rhs)

    def *(rhs: DataEle) = lhs.dtimes(lhs, rhs)

    def /(rhs: DataEle) = lhs.ddiv(lhs, rhs)
  }


  val exposeComplexVector = new ExposeRep[Data]() {
    val freshExps = (u: Unit) => Vector(Arg[ComplexVector])
    val vec2t: Vector[Exp[_]] => SComplexVector = (in: Vector[Exp[_]]) => SComplexVector(in.head.asInstanceOf[Rep[ComplexVector]])
    val t2vec: Data => Vector[Exp[_]] = (in: Data) => in match {
      case v: SComplexVector => Vector(v.d)
      case _ => ???
    }
  }
  val exposeInterleavedComplexVector = new ExposeRep[Data]() {
    val freshExps = (u: Unit) => Vector(Arg[Array[Double]])
    val vec2t: Vector[Exp[_]] => InterleavedComplexVector = (in: Vector[Exp[_]]) => InterleavedComplexVector(in.head.asInstanceOf[Rep[Array[Double]]])
    val t2vec: Data => Vector[Exp[_]] = (in: Data) => in match {
      case v: InterleavedComplexVector => Vector(v.d)
      case _ => ???
    }
  }

  abstract class DataEle {

    def create(re: Exp[Double], im: Exp[Double]): DataEle

    def dplus(x1: DataEle, y1: DataEle): DataEle = (x1,y1) match {
      case (x: SComplex, y: SComplex) => x.dplus(x,y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.dplus(x,y)
      case _ => ???
    }

    def dminus(x1: DataEle, y1: DataEle): DataEle = (x1,y1) match {
      case (x: SComplex, y: SComplex) => x.dminus(x,y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.dminus(x,y)
      case _ => ???
    }

    def dtimes(x1: DataEle, y1: DataEle): DataEle = (x1,y1) match {
      case (x: SComplex, y: SComplex) => x.dtimes(x,y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.dtimes(x,y)
      case _ => ???
    }

    def ddiv(x1: DataEle, y1: DataEle): DataEle = (x1,y1) match {
      case (x: SComplex, y: SComplex) => x.ddiv(x,y)
      case (x: InterleavedComplex, y: InterleavedComplex) => x.ddiv(x,y)
      case _ => ???
    }
  }

  abstract class Data {
    def getdata(): Exp[_]

    def create(n: AInt): Data = ???

    def apply(i: AInt): DataEle

    def update(i: AInt, y: DataEle): Data = (this,y) match{
      case (me: SComplexVector,e: SComplex) => me.updatex(i,e)
      case (me: InterleavedComplexVector,e: InterleavedComplex) => me.updatex(i,e)
      case (me: ScalarVector,e: InterleavedComplex) => me.updatex(i,e)
      case _ => ???
    }

    def t2vec(): Vector[Exp[_]]



  }


  case class SComplex(d: Exp[Complex]) extends DataEle {

    def create(re: Exp[Double], im: Exp[Double]): SComplex = new SComplex(compcreate(re,im))
    def dplus(x: SComplex, y: SComplex): SComplex = SComplex(plus(x.d, y.d))

    def dminus(x: SComplex, y: SComplex): SComplex = SComplex(minus(x.d, y.d))

    def dtimes(x: SComplex, y: SComplex): SComplex = SComplex(times(x.d, y.d))
  }

  case class SComplexVector(d: Exp[ComplexVector]) extends Data {


    override def create(n: AInt): SComplexVector = SComplexVector(veccreate( n.ev.toRep(n.a)))

    override def getdata() = d
    def apply(i: AInt): SComplex = {
      val t = i.ev.toRep(i.a)
      SComplex(vecapply(d, t))
    }

    def updatex(i: AInt, y: SComplex): Data = {
      val t = i.ev.toRep(i.a)
      SComplexVector(vecupdate(d, t, y.d))
    }

    def t2vec(): Vector[Exp[_]] = Vector(d)
  }


  case class InterleavedComplex(re: Exp[Double], im: Exp[Double]) extends DataEle {

    def create(re: Exp[Double], im: Exp[Double]): InterleavedComplex = InterleavedComplex(re,im)

    def dplus(x: InterleavedComplex, y: InterleavedComplex): InterleavedComplex = InterleavedComplex(x.re + y.re,x.im + y.im)

    def dminus(x: InterleavedComplex, y: InterleavedComplex): InterleavedComplex = InterleavedComplex(x.re - y.re,x.im - y.im)

    def dtimes(x: InterleavedComplex, y: InterleavedComplex): InterleavedComplex = InterleavedComplex(x.re * y.re - x.im * y.im, x.re * y.im + x.im * y.re)
  }

  case class InterleavedComplexVector( d: Exp[Array[Double]]) extends Data {

    override def create(n: AInt): InterleavedComplexVector = InterleavedComplexVector( dveccreate( n.ev.toRep(n.a)))

    override def getdata() = d
    def apply(i: AInt): InterleavedComplex = {
      val t = i.ev.toRep(i.a)
      val re = dvecapply(d,2*t)
      val im = dvecapply(d,(2*t)+1)
      InterleavedComplex(re,im)
    }

    def updatex(i: AInt, y: InterleavedComplex): Data = {
      val t = i.ev.toRep(i.a)
      val re = dvecupdate(d,(2*t),y.re)
      val im = dvecupdate(re,(2*t+1),y.im)
      InterleavedComplexVector(im)
    }

    def t2vec(): Vector[Exp[_]] = Vector(d)
  }


  case class ScalarVector( d: Array[Exp[Double]]) extends Data {


    override def create(n: AInt): ScalarVector = n.ev.fold[Int,ScalarVector](n.a,
      fa=> {      ??? /* this should not occur */   }, fb => {
        ScalarVector(new Array[Exp[Double]](fb * 2))
      })

    override def getdata() = ???
    def apply(n: AInt): InterleavedComplex =
      n.ev.fold[Int,InterleavedComplex ](n.a,
        fa=> {      ??? /* this should not occur */   }, i => {
          val t = i
          val re =d(2*t)
          val im = d((2*t)+1)
          InterleavedComplex(re,im)
        })

    def updatex(i: AInt, y: InterleavedComplex): Data =
      i.ev.fold[Int,Data ](i.a,
        fa=> {      ??? /* this should not occur */   }, fb => {
          val t = fb
          val re = d.update((2*t),y.re)
          val im = d.update((2*t+1),y.im)
          this
        })


    def t2vec(): Vector[Exp[_]] = d.toVector
  }




  case class MaybeSFunction(f: Either[StagedFunction[Dyn, Data], Dyn => Data]) {
    def apply(dyn: Dyn): Data = f.fold(fa => fa(dyn), fb => fb(dyn))
    def mkfun(stat: Stat, dyn: Dyn): Data = f.fold(fa => fa(dyn), fb => {
      val expose = exposeDyn(stat)
      val t = doGlobalLambda(fb, Some("Base" + stat.toSig()), Some("Base" + stat.toSig()))(expose, stat.expdata)
      t(dyn)
    })
  }

  object MaybeSFunction {
    def apply(f: StagedFunction[Dyn, Data]): MaybeSFunction = MaybeSFunction(Left(f))

    def apply(f: (Dyn => Data)): MaybeSFunction = MaybeSFunction(Right(f))
  }

  def OO2Exp(oo: OptionalEntry): Vector[Exp[_]] = {
    oo.toOneEntry().map(p => p.ev.getRep(p.a).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
  }

  implicit def toOE[X: Numeric : TypeRep](x: X): OneEntry {type T = X} = {
    new OneEntry {
      override type A[Y] = NoRep[Y]
      override type T = X
      override val evnum: scala.Numeric[X] = implicitly[Numeric[X]]
      override val ev: Header.this.IRep[A] = cNoRep
      override val a: this.A[this.T] = x
      override val evtyp: Header.this.TypeRep[this.T] = implicitly[TypeRep[X]]
    }
  }

  def toOEL[X: TypeRep](x: X): OneEntry {type T = X} = {
    new OneEntry {
      override type A[Y] = NoRep[Y]
      override type T = X
      override val evnum: scala.Numeric[X] = null //ugly!!!!
      override val ev: Header.this.IRep[A] = cNoRep
      override val a: this.A[this.T] = x
      override val evtyp: Header.this.TypeRep[this.T] = implicitly[TypeRep[X]]
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

    def makeSome(me: OneEntry) = {
      new OptionalEntry {
        override type A[X] = me.A[X]
        override type T = me.T
        override val evnum: Numeric[T] = me.evnum
        override val ev: IRep[A] = me.ev
        override val a: Option[A[T]] = Some(me.a)
        override val evtyp = me.evtyp
      }
    }

    def makeNone(me: OneEntry) = {
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

    def toSig(): String = a.fold("")(fb => fb match {
      case lis: Int => if (lis < 0) s"neg${Math.abs(lis)}" else lis.toString
      case _ => fb.toString
    }
    )


    def toOneEntry(): Option[OneEntry {type T = self.T}] = {
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

  def sph(): AInt = {
    new OneEntry {
      override type T = Int
      override type A[X] = Exp[X]
      override val evnum: Numeric[Int] = implicitly[Numeric[Int]]
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[Int]
      override val a: this.A[this.T] = fresh[Int]
    }
  }
  def sph2(): LInt = {
    new OneEntry {
      override type T = List[Int]
      override type A[X] = Exp[X]
      override val evnum: Numeric[T] = null //ugly!!!!
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[List[Int]]
      override val a: this.A[this.T] = fresh[List[Int]]
    }
  }

  def R2AInt(x: Exp[Int]): AInt = {
    new OneEntry {
      override type T = Int
      override type A[X] = Exp[X]
      override val evnum: Numeric[Int] = implicitly[Numeric[Int]]
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[Int]
      override val a: this.A[this.T] = x
    }
  }

  def R2LInt(x: Exp[List[Int]]): LInt = {
    new OneEntry {
      override type T = List[Int]
      override type A[X] = Exp[X]
      override val evnum: Numeric[T] = null //ugly!!!!!
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[List[Int]]
      override val a: this.A[this.T] = x
    }
  }
  type AInt = OneEntry {type T = Int}


  type LInt = OneEntry {type T = List[Int]}

  class AIntOps(lhs: AInt) {
    def +(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x + y)
          case _ => (R2AInt(int_plus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_plus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }

    def -(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x - y)
          case _ => (R2AInt(int_minus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_minus(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }

    def *(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x * y)
          case _ => (R2AInt(int_times(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_times(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }

    def /(rhs: AInt): AInt = {
      if (!(lhs.ev.isRep() && rhs.ev.isRep())) {
        (lhs.a, rhs.a) match {
          case (x: Int, y: Int) => toOE(x / y)
          case _ => (R2AInt(int_divide(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
        }
      } else {
        (R2AInt(int_divide(lhs.ev.toRep(lhs.a), rhs.ev.toRep(rhs.a))))
      }
    }
  }

  implicit def toAIntOps(lhs: AInt) = new AIntOps(lhs)


  trait RepSelector2 {
    val rrep: Boolean

    def repselect[X](one: OneEntry {type T = X}): OptionalEntry {type T = X} =
      if (rrep) {
        if (one.ev.isRep()) one.makeSome(one) else one.makeNone(one)
      } else if (one.ev.isRep()) one.makeNone(one) else one.makeSome(one)


  }

  trait DynSelector2 extends RepSelector2 {
    val rrep: Boolean = true
  }

  trait StatSelector2 extends RepSelector2 {
    val rrep: Boolean = false
  }

  abstract class IMHBase(val base: AInt, val s0: AInt, val s1: AInt)

  abstract class IMHHeader(base: AInt, s0: AInt, s1: AInt) extends IMHBase(base, s0, s1) with RepSelector2 {
    def getbase() = repselect(base)

    def gets0() = repselect(s0)

    def gets1() = repselect(s1)
  }

  class StatIMH(base: AInt, s0: AInt, s1: AInt) extends IMHHeader(base, s0, s1) with StatSelector2 {
    def freshExps(): Vector[Exp[_]] = base.ev.fresh()(base.evtyp) ++ s0.ev.fresh()(s0.evtyp) ++ s1.ev.fresh()(s1.evtyp)

    def vec2t(v: Vector[Exp[_]]): (DynIMH, Vector[Exp[_]]) = {
      def help(a: AInt, v: Vector[Exp[_]]): (AInt, Vector[Exp[_]]) = if (a.ev.isRep()) (R2AInt(v.head.asInstanceOf[Exp[Int]]), v.tail) else (toOE(-1), v)

      val (nb, ab) = help(base, v)
      val (ns0, as0) = help(s0, ab)
      val (ns1, as1) = help(s1, as0)
      (new DynIMH(nb, ns0, ns1), as1)
    }

    def genSig(): String = "b" + repselect(base).toSig() + "s0" + repselect(s0).toSig() + "s1" + repselect(s1).toSig()
  }

  class DynIMH(base: AInt, s0: AInt, s1: AInt) extends IMHHeader(base, s0, s1) with DynSelector2 {
    def t2vec(): Vector[Exp[_]] = OO2Exp(repselect(base)) ++ OO2Exp(repselect(s0)) ++ OO2Exp(repselect(s1))
  }

  object IMH {
    def apply(stat: StatIMH, dyn: DynIMH): IMH = {
      val b = stat.getbase().toOneEntry().getOrElse(dyn.getbase().toOneEntry().get)
      val s0 = stat.gets0().toOneEntry().getOrElse(dyn.gets0().toOneEntry().get)
      val s1 = stat.gets1().toOneEntry().getOrElse(dyn.gets1().toOneEntry().get)
      new IMH(b, s0, s1)
    }
  }

  case class IMH(override val base: AInt, override val s0: AInt, override val s1: AInt) extends IMHBase(base, s0, s1) {
    def getDynIMH(): DynIMH = new DynIMH(base, s0, s1)

    def getStatIMH(): StatIMH = new StatIMH(base, s0, s1)
  }

  object IM {
    def apply(statx: StatIM, dynx: DynIM): IMFull = {
      (statx,dynx) match{
        case (stat: Stat_GT_IM, dyn: Dyn_GT_IM) => apply(stat,dyn)
        case (stat: Stat_GTI_IM, dyn: Dyn_GTI_IM) => apply(stat,dyn)
        case (stat: Stat_GTT_IM, dyn: Dyn_GTT_IM) => apply(stat,dyn)
        case _ => ???
      }
    }

    def apply(stat: Stat_GT_IM, dyn: Dyn_GT_IM): GT_IM = {
      val g = IMH(stat.g, dyn.g)
      val s = IMH(stat.s, dyn.s)
      new GT_IM(g, s)
    }

    def apply(stat: Stat_GTI_IM, dyn: Dyn_GTI_IM): GTI_IM = {
      val im = IMH(stat.im, dyn.im)
      val tw = IMH(stat.twim, dyn.twim)
      new GTI_IM(im, tw)
    }
    def apply(stat: Stat_GTT_IM, dyn: Dyn_GTT_IM): GTT_IM = {
      val g = IMH(stat.g, dyn.g)
      val s = IMH(stat.s, dyn.s)
      val tw = IMH(stat.twim, dyn.twim)
      new GTT_IM(g,s, tw)
    }
  }


  abstract class IM {
    def gather(): IMHBase

    def scatter(): IMHBase
  }

  abstract class IMFull extends IM {
    def getDynIM(): DynIM

    def getStatIM(): StatIM
  }


  case class GT_IM(g: IMH, s: IMH) extends IMFull {
    def getDynIM(): Dyn_GT_IM = new Dyn_GT_IM(g.getDynIMH(), s.getDynIMH())

    def getStatIM(): Stat_GT_IM = new Stat_GT_IM(g.getStatIMH(), s.getStatIMH())

    def gather() = g

    def scatter() = s
  }

  case class GTI_IM(im: IMH, twim: IMH) extends IMFull {
    def getDynIM(): Dyn_GTI_IM = new Dyn_GTI_IM(im.getDynIMH(), twim.getDynIMH())

    def getStatIM(): Stat_GTI_IM = new Stat_GTI_IM(im.getStatIMH(), twim.getStatIMH())

    def gather() = im

    def scatter() = im
  }

  case class GTT_IM(g: IMH, s: IMH, twim: IMH) extends IMFull {
    def getDynIM(): Dyn_GTT_IM = new Dyn_GTT_IM(g.getDynIMH(), s.getDynIMH(), twim.getDynIMH())

    def getStatIM(): Stat_GTT_IM = new Stat_GTT_IM(g.getStatIMH(), s.getStatIMH(), twim.getStatIMH())

    def gather() = g

    def scatter() = s
  }

  abstract class StatIM extends IM {
    def freshExps(): Vector[Exp[_]]

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]])

    def toSig() = "g" + gather().genSig() + "s" + scatter().genSig()

    def gather(): StatIMH

    def scatter(): StatIMH
  }

  case class Stat_GT_IM(g: StatIMH, s: StatIMH) extends StatIM {
    def freshExps(): Vector[Exp[_]] = g.freshExps() ++ s.freshExps()

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a, b) = g.vec2t(v)
      val (c, d) = s.vec2t(b)
      (new Dyn_GT_IM(a, c), d)
    }

    def gather() = g

    def scatter() = s
  }

  case class Stat_GTI_IM(im: StatIMH, twim: StatIMH) extends StatIM {
    override def toSig() = "im" + im.genSig() + "tw" + twim.genSig()

    def freshExps(): Vector[Exp[_]] = im.freshExps() ++ twim.freshExps()

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a, b) = im.vec2t(v)
      val (c, d) = twim.vec2t(b)
      (new Dyn_GTI_IM(a, c), d)
    }

    def gather() = im

    def scatter() = im
  }

  case class Stat_GTT_IM(g: StatIMH, s: StatIMH, twim: StatIMH) extends StatIM {
    override def toSig() = "g" + g.genSig() + "s" + s.genSig() + "tw" + twim.genSig()

    def freshExps(): Vector[Exp[_]] = g.freshExps() ++ s.freshExps() ++ twim.freshExps()

    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a, b) = g.vec2t(v)
      val (c, d) = s.vec2t(b)
      val (e, f) = twim.vec2t(d)
      (new Dyn_GTT_IM(a, c,e), f)
    }

    def gather() = g

    def scatter() = s
  }


  abstract class DynIM extends IM {
    def t2vec(): Vector[Exp[_]]
  }

  case class Dyn_GT_IM(g: DynIMH, s: DynIMH) extends DynIM {
    def t2vec(): Vector[Exp[_]] = g.t2vec() ++ s.t2vec()

    def gather() = g

    def scatter() = s
  }

  case class Dyn_GTI_IM(im: DynIMH, twim: DynIMH) extends DynIM {
    def t2vec(): Vector[Exp[_]] = im.t2vec() ++ twim.t2vec()

    def gather() = im

    def scatter() = im
  }

  case class Dyn_GTT_IM(g: DynIMH, s: DynIMH, twim: DynIMH) extends DynIM {
    def t2vec(): Vector[Exp[_]] = g.t2vec() ++ s.t2vec() ++ twim.t2vec()

    def gather() = g

    def scatter() = s
  }


  abstract class Base(pos: AInt, n: AInt, lb: AInt, im: IM, v: AInt, tw: Option[TwidBase])

  abstract class Header(pos: AInt, n: AInt, lb: AInt, im: IM, v: AInt, tw: Option[TwidHeader]) extends Base(pos,n, lb, im, v, tw) with RepSelector2 {
    def getpos() = repselect(pos)

    def getn() = repselect(n)

    def getlb() = repselect(lb)

    def getim(): IM = im

    def getv() = repselect(v)

    def gettw(): Option[TwidHeader] = tw
  }

  class Stat(val pos: AInt, val n: AInt, val lb: AInt, val im: StatIM, val v: AInt, val tw: Option[StatTwiddleScaling], val par: Option[Int], val precompute: Boolean, val expdata: ExposeRep[Data]) extends Header(pos, n, lb, im, v, tw) with StatSelector2 {
    def toSig(): String = {
      val t = "n" + repselect(n).toSig() + "lb" + repselect(lb).toSig() + im.toSig() + "v" + repselect(v).toSig() + "tw" + tw.fold("")(t => t.genSig()) + "par" + par.fold("")(p => p.toString) + "pos" + repselect(pos).toSig()
      t
    }
    override def getim(): StatIM = im

    override def gettw(): Option[StatTwiddleScaling] = tw
  }

  class Dyn(val pos: AInt, val x: Data, val y: Data, n: AInt, lb: AInt, im: DynIM, v: AInt, tw: Option[DynTwiddleScaling]) extends Header(pos, n, lb, im, v, tw) with DynSelector2 {
    override def getim(): DynIM = im

    override def gettw(): Option[DynTwiddleScaling] = tw
  }


  object Mix {
    def apply(stat: Stat, dyn: Dyn): Mix = {
      val pos: AInt = stat.getpos().toOneEntry().getOrElse(dyn.getpos().toOneEntry().get)
      val n: AInt = stat.getn().toOneEntry().getOrElse(dyn.getn().toOneEntry().get)
      val lb = stat.getlb().toOneEntry().getOrElse(dyn.getlb().toOneEntry().get)
      val v = stat.getv().toOneEntry().getOrElse(dyn.getv().toOneEntry().get)
      val g = stat.getim()
      val s = dyn.getim()
      val im = IM(g, s)
      val tw = TwiddleScaling(stat.gettw(), dyn.gettw())
      Mix(pos,dyn.x, dyn.y, n, lb, im, v, tw, stat.par, stat.precompute, stat.expdata)
    }
  }


  case class Mix(pos: AInt, x: Data, y: Data, n: AInt, lb: AInt, im: IMFull, v: AInt, tw: Option[TwiddleScaling], par: Option[Int], precompute: Boolean, expdata: ExposeRep[Data]) extends Base(pos, n, lb, im, v, tw) {
    def getDyn(): Dyn = new Dyn(pos, x, y, n, lb, im.getDynIM(), v, tw.fold[Option[DynTwiddleScaling]](None)(fb => Some(fb.getDynTwiddleScaling())))

    def getStat(): Stat = new Stat(pos, n, lb, im.getStatIM(), v, tw.fold[Option[StatTwiddleScaling]](None)(fb => Some(fb.getStatTwiddleScaling())),par, precompute, expdata)
  }

  def OR2AInt[T](op: Option[T]): AInt = op match {
    case Some(x: Rep[Int]) => R2AInt(x)
    case _ => toOE(-1)
  }
  def OR2LInt[T](op: Option[T]): LInt = op match {
    case Some(x: Rep[List[Int]]) => R2LInt(x)
    case _ => toOEL(List.empty)
  }

  implicit def exposeDyn(stat: Stat): ExposeRep[Dyn] = {
    new ExposeRep[Dyn]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => {
        val t = stat.expdata.freshExps() ++ stat.expdata.freshExps() ++ stat.pos.ev.fresh()(stat.pos.evtyp) ++stat.n.ev.fresh()(stat.n.evtyp) ++ stat.lb.ev.fresh()(stat.lb.evtyp) ++
          stat.v.ev.fresh()(stat.v.evtyp) ++ stat.im.freshExps() ++  stat.tw.fold[Vector[Exp[_]]](Vector.empty)(fb => fb.freshExps())
        t
      }
      val vec2t: Vector[Exp[_]] => Dyn = (in: Vector[Exp[_]]) => {
        def removeData(v: Vector[Exp[_]]): (Data, Vector[Exp[_]]) = {
          val d = stat.expdata.vec2t(v)
          val me = stat.expdata.t2vec(d)
          (d, v.drop(me.size))
        }
        val (x, ax) = removeData(in)
        val (y, ay) = removeData(ax)
        val (apos, opos) = stat.pos.ev.fetch[List[Int]](ay)
        val (an, on) = stat.n.ev.fetch[Int](apos)
        val (alb, olb) = stat.lb.ev.fetch[Int](an)
        val (av, ov) = stat.v.ev.fetch[Int](alb)
        val (im, aim) = stat.im.vec2t(av)

        val (atw, otw) = stat.tw.fold[(Vector[Exp[_]], Option[DynTwiddleScaling])]((aim, None))(fb => {
          val (t0, t1) = fb.vec2t(aim)
          (t1, Some(t0))
        })
        val pos: AInt = OR2AInt(opos)
        val v: AInt = OR2AInt(ov)
        val n: AInt = OR2AInt(on)
        val lb: AInt = OR2AInt(olb)
        new Dyn(pos,x, y, n, lb, im, v, otw)
      }
      val t2vec: Dyn => Vector[Exp[_]] = (in: Dyn) => {
        in.x.t2vec() ++ in.y.t2vec() ++
          OO2Exp(in.getpos()) ++
          OO2Exp(in.getn()) ++
          OO2Exp(in.getlb()) ++
          OO2Exp(in.getv()) ++
          in.getim().t2vec() ++
          in.gettw().fold[Vector[Exp[_]]](Vector.empty)(fb => fb.t2vec(fb))
      }
    }
  }


  abstract class TwidBase(n: AInt, d: AInt, k: AInt)

  abstract class TwidHeader(n: AInt, d: AInt, k: AInt) extends TwidBase(n, d, k) with RepSelector2 {
    def getn() = repselect(n)

    def getd() = repselect(d)

    def getk() = repselect(k)

  }

  case class TwiddleScaling(n: AInt, d: AInt, k: AInt) extends TwidBase(n, d, k) {
    def getDynTwiddleScaling(): DynTwiddleScaling = new DynTwiddleScaling(n, d, k)

    def getStatTwiddleScaling(): StatTwiddleScaling = new StatTwiddleScaling(n, d, k)
  }

  class StatTwiddleScaling(val n: AInt, val d: AInt, val k: AInt) extends TwidHeader(n, d, k) with StatSelector2 {

    def genSig(): String = "n" + repselect(n).toSig() + "d" + repselect(d).toSig() + "k" + repselect(k).toSig()
    def freshExps(): Vector[Exp[_]] = n.ev.fresh()(n.evtyp) ++ d.ev.fresh()(d.evtyp) ++ k.ev.fresh()(k.evtyp)
//base.ev.fresh()(base.evtyp) ++ s0.ev.fresh()(s0.evtyp) ++ s1.ev.fresh()(s1.evtyp)
    def vec2t(v: Vector[Exp[_]]): (DynTwiddleScaling, Vector[Exp[_]]) = {
      val (an, on) = n.ev.fetch[Int](v)
      val (ad, od) = d.ev.fetch[Int](an)
      val (ak, ok) = k.ev.fetch[Int](ad)
      val nx = OR2AInt(on)
      val dx = OR2AInt(od)
      val kx = OR2AInt(ok)
      (new DynTwiddleScaling(nx, dx, kx), ad)
    }
  }

  case class DynTwiddleScaling(n: AInt, d: AInt, k: AInt) extends TwidHeader(n, d, k) with DynSelector2 {
    def t2vec(in: DynTwiddleScaling): Vector[Exp[_]] = OO2Exp(repselect(n)) ++ OO2Exp(repselect(d)) ++ OO2Exp(repselect(k))
  }

  object TwiddleScaling {
    def apply(statx: Option[StatTwiddleScaling], dynx: Option[DynTwiddleScaling]): Option[TwiddleScaling] = {
      statx.fold[Option[TwiddleScaling]](None)(stat => dynx.fold[Option[TwiddleScaling]](None)(dyn => {
        val n: AInt = stat.getn().toOneEntry().getOrElse(dyn.getn().toOneEntry().get)
        val d = stat.getd().toOneEntry().getOrElse(dyn.getd().toOneEntry().get)
        val k = stat.getk().toOneEntry().getOrElse(dyn.getk().toOneEntry().get)
        Some(TwiddleScaling(n, d, k))
      }))
    }
  }


}