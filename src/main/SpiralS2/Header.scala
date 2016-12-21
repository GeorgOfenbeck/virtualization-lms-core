 package SpiralS2

trait Header extends DFTData {

  implicit def mkDataEleOops(lhs: DataEle) = new DataEleOps(lhs)
  class DataEleOps(lhs: DataEle){
    def +(rhs: DataEle) = lhs.dplus(lhs,rhs)
    def -(rhs: DataEle) = lhs.dminus(lhs,rhs)
    def *(rhs: DataEle) = lhs.dtimes(lhs,rhs)
    def /(rhs: DataEle) = lhs.ddiv(lhs,rhs)
  }

  //this does not scale yet - hardcoded ComplexVector
  implicit val exposeData = new ExposeRep[Data]() {
    val freshExps = (u: Unit) => Vector(Arg[ComplexVector])
    val vec2t: Vector[Exp[_]] => SComplexVector = (in: Vector[Exp[_]]) => SComplexVector(in.head.asInstanceOf[Rep[ComplexVector]])
    val t2vec: Data => Vector[Exp[_]] = (in: Data) => in match {
      case v: SComplexVector => Vector(v.d)
      case _ => ???
    }
  }

  abstract class DataEle {
    def dplus(x: DataEle, y: DataEle): DataEle = ???
    def dminus(x: DataEle, y: DataEle): DataEle = ???
    def dtimes(x: DataEle, y: DataEle): DataEle = ???
    def ddiv(x: DataEle, y: DataEle): DataEle = ???
  }
  abstract class Data{
    def create(n: AInt): Data = ???
    def apply(i: AInt): DataEle
    def update(i: AInt, y: DataEle): Data = ???
    def t2vec(): Vector[Exp[_]]
  }



  case class SComplex(d: Exp[Complex]) extends DataEle {
    def dplus(x: SComplex, y: SComplex):SComplex = SComplex(plus(x.d,y.d))
    def dminus(x: SComplex, y: SComplex):SComplex = SComplex(minus(x.d,y.d))
    def dtimes(x: SComplex, y: SComplex):SComplex = SComplex(times(x.d,y.d))
  }
  
  case class SComplexVector(d: Exp[ComplexVector]) extends Data{
    def apply(i: AInt): SComplex = {
      val t = i.ev.toRep(i.a)
      SComplex(vecapply(d,t))
    }
    def update(i: AInt, y: SComplex): Data = {
      val t = i.ev.toRep(i.a)
      SComplexVector(vecupdate(d,t,y.d))
    }
    def t2vec(): Vector[Exp[_]] = Vector(d)
  }




  case class MaybeSFunction(f: Either[StagedFunction[Dyn, Data], Dyn => Data]) {
    def apply(dyn: Dyn): Data = f.fold(fa => fa(dyn), fb => fb(dyn))
  }
  object MaybeSFunction {
    def apply(f: StagedFunction[Dyn, Data]): MaybeSFunction = MaybeSFunction(Left(f))
    def apply(f: (Dyn => Data)): MaybeSFunction = MaybeSFunction(Right(f))
  }

  def OO2Exp(oo: OptionalEntry): Vector[Exp[_]] = {
    oo.toOneEntry().map(p => p.ev.getRep(p.a).map(o => Vector(o)).getOrElse(Vector.empty)).getOrElse(Vector.empty)
  }

  def toOE[X: Numeric : TypeRep](x: X): OneEntry{type T = X} = {
    new OneEntry {
      override type A[Y] = NoRep[Y]
      override type T = X
      override val evnum: scala.Numeric[X] = implicitly[Numeric[X]]
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

    def toSig(): String = a.fold("")(fb => fb.toString)

    def toOneEntry(): Option[OneEntry{ type T = self.T}] = {
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
  def R2AInt(x: Exp[Int]): AInt = {
    new AInt {
      override type A[X] = Exp[X]
      override val evnum: Numeric[Int] = Numeric[Int]
      override val ev: IRep[A] = cRep
      override val evtyp: TypeRep[T] = manifest[Int]
      override val a: this.A[this.T] = x
    }
  }
  type AInt = OneEntry{type T = Int  }

  class AIntOps(lhs: AInt){
    def +(rhs: AInt): AInt = ???
    def -(rhs: AInt): AInt = ???
    def *(rhs: AInt): AInt = ???
    def /(rhs: AInt): AInt = ???
  }

  implicit def toAIntOps(lhs: AInt) = new AIntOps(lhs)



  trait RepSelector2 {
    val rrep: Boolean

    def repselect[X](one: OneEntry{ type T = X}): OptionalEntry{ type T = X} =
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

  abstract class IMHBase(base: AInt, s0: AInt, s1: AInt)
  abstract class IMHHeader(base: AInt, s0: AInt, s1: AInt) extends IMHBase(base,s0, s1) with RepSelector2
  class StatIMH(base: AInt, s0: AInt, s1: AInt) extends IMHHeader(base,s0, s1) with StatSelector2{
    def freshExps(): Vector[Exp[_]] = base.ev.fresh() ++ s0.ev.fresh() ++ s1.ev.fresh()
    def vec2t(v: Vector[Exp[_]]): (DynIMH, Vector[Exp[_]]) = {
      def help(a: AInt, v: Vector[Exp[_]]): (AInt,Vector[Exp[_]]) = if (a.ev.isRep()) (R2AInt(v.head.asInstanceOf[Exp[Int]]), v.tail) else (toOE(-1),v)
      val(nb, ab) = help(base,v)
      val(ns0, as0) = help(s0,ab)
      val(ns1, as1) = help(s1,as0)
      (new DynIMH(nb,ns0,ns1), as1)
    }
    def genSig(): String = "b" + repselect(base).toSig() + "s0" + repselect(s0).toSig() + "s1" + repselect(s1).toSig()
  }
  class DynIMH(base: AInt, s0: AInt, s1: AInt) extends IMHHeader(base,s0,s1) with DynSelector2{
    def t2vec(): Vector[Exp[_]] = OO2Exp(repselect(base)) ++ OO2Exp(repselect(s0)) ++ OO2Exp(repselect(s1))
  }
  case class IMH(base: AInt, s0: AInt, s1: AInt) extends IMHBase(base,s0,s1){
    def getDynIMH(): DynIMH = ???
    def getStatIMH(): StatIMH = ???
  }

  abstract class IM {
    def getDynIM(): DynIM
    def getStatIM(): StatIM
    def gather(): IMH
    def scatter(): IMH
    def toSig() = gather().getStatIMH().genSig() + scatter().getStatIMH().genSig()
  }
  case class GT_IM (g: IMH, s: IMH) extends IM
  {
    def getDynIM(): Dyn_GT_IM = new Dyn_GT_IM(g.getDynIMH(),s.getDynIMH())
    def getStatIM(): Stat_GT_IM = new Stat_GT_IM(g.getStatIMH(),s.getStatIMH())
    def gather() = g
    def scatter() = s
  }
  case class GTI_IM(im: IMH, twim: IMH) extends IM
  {
    def getDynIM(): Dyn_GTI_IM = new Dyn_GTI_IM(im.getDynIMH(), twim.getDynIMH())
    def getStatIM(): Stat_GTI_IM = new Stat_GTI_IM(im.getStatIMH(), twim.getStatIMH())
    def gather() = im
    def scatter() = im
  }
  abstract class StatIM{
    def freshExps(): Vector[Exp[_]]
    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]])
  }
  case class Stat_GT_IM(g: StatIMH, s: StatIMH) extends StatIM
  {
    def freshExps(): Vector[Exp[_]] = g.freshExps() ++ s.freshExps()
    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a,b) = g.vec2t(v)
      val (c,d) = s.vec2t(b)
      (new Dyn_GT_IM(a,c),d)
    }
  }
  case class Stat_GTI_IM( im: StatIMH, twim: StatIMH) extends StatIM
  {
    def freshExps(): Vector[Exp[_]] = im.freshExps() ++ twim.freshExps()
    def vec2t(v: Vector[Exp[_]]): (DynIM, Vector[Exp[_]]) = {
      val (a,b) = im.vec2t(v)
      val (c,d) = twim.vec2t(b)
      (new Dyn_GTI_IM(a,c),d)
    }
  }


  abstract class DynIM
  {
    def t2vec(): Vector[Exp[_]]
  }
  case class Dyn_GT_IM(g: DynIMH, s: DynIMH) extends DynIM
  {
    def t2vec(): Vector[Exp[_]] = g.t2vec() ++ s.t2vec()
  }
  case class Dyn_GTI_IM(im: DynIMH, twim: DynIMH) extends DynIM
  {
    def t2vec(): Vector[Exp[_]] = im.t2vec() ++ twim.t2vec()
  }


  abstract class Base(n: AInt, lb: AInt, im: IM, v: AInt, tw: TwidBase)

  abstract class Header(n: AInt, lb: AInt, im: IM, v: AInt, tw: TwidHeader) extends Base(n,lb,im,v, tw) with RepSelector2{
    def getn() = repselect(n)
    def getlb() = repselect(lb)
    def getim() = im
    def getv() = repselect(v)
  }

  object Stat {

  }
  class Stat(n: AInt, lb: AInt, im: IM, v: AInt, tw: StatTwiddleScaling) extends Header(n,lb,im,v, tw) with StatSelector2{
    def toSig():String = {
      "n" + repselect(n).toSig() + "lb" + repselect(lb).toSig() + im.toSig() + "v" + repselect(v).toSig()
    }
  }

  class Dyn(val x: Data, val y: Data, n: AInt, lb: AInt, im: IM, v: AInt, tw: DynTwiddleScaling) extends Header(n,lb,im,v, tw) with DynSelector2


  object Mix{
    def apply(stat: Stat, dyn: Dyn): Mix = {
      val n: AInt = stat.getn().toOneEntry().getOrElse(dyn.getn().toOneEntry().get)
      val lb = stat.getlb().toOneEntry().getOrElse(dyn.getlb().toOneEntry().get)
      val v = stat.getv().toOneEntry().getOrElse(dyn.getv().toOneEntry().get)
      val im = ???
      val tw = ???
      Mix(dyn.x,dyn.y,n,lb,im,v, tw)
    }
  }
  case class Mix(x: Data, y: Data, n: AInt, lb: AInt, im: IM, v: AInt, tw: TwiddleScaling) extends Base(n,lb,im,v, tw) {
    def getDyn(): Dyn = ???
    def getStat(): Stat = ???
  }


  implicit def exposeDyn(stat: Stat): ExposeRep[Dyn] = {
    new ExposeRep[Dyn]() {
      val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => ???
      val vec2t: Vector[Exp[_]] => Dyn = (in: Vector[Exp[_]]) => ???
      val t2vec: Dyn => Vector[Exp[_]] = (in: Dyn) => {
        in.x.t2vec() ++ in.y.t2vec() ++
          OO2Exp(in.getn()) ++
          OO2Exp(in.getlb()) ++
          in.getim().getDynIM().t2vec() ++
          OO2Exp(in.getv())
      }
    }
  }


  abstract class TwidBase(n: AInt, d: AInt, k: AInt)
  abstract class TwidHeader(n: AInt, d: AInt, k: AInt) extends TwidBase(n,d,k) with RepSelector2

  case class TwiddleScaling(n: AInt, d: AInt, k: AInt) extends TwidBase(n,d,k) {
    def getDynTwiddleScaling(): DynTwiddleScaling = ???
    def getStatTwiddleScaling(): StatTwiddleScaling = ???
  }

  class StatTwiddleScaling(n: AInt, d: AInt, k: AInt) extends TwidHeader(n,d,k) with StatSelector2 {
    def freshExps(): Vector[Exp[_]] = ???
    def vec2t(v: Vector[Exp[_]]): (DynTwiddleScaling, Vector[Exp[_]]) = ???
  }
  case class DynTwiddleScaling(n: AInt, d: AInt, k: AInt) extends TwidHeader(n,d,k) with DynSelector2 {
    def t2vec(): Vector[Exp[_]] = ???
  }

  object TwiddleScaling {
    def apply(stat: StatTwiddleScaling, dyn: DynTwiddleScaling): TwiddleScaling = {
      ???
    }

  }


}