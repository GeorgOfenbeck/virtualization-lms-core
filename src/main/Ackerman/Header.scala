package Ackerman

import org.scala_lang.virtualized.SourceContext

trait Header extends Skeleton {

  var graphname: Boolean = false

  //just to rename
  def ifThenElse[A](cond: Rep[Boolean], thenp: => A, elsep: => A)(implicit pos: SourceContext, branch: ExposeRep[A]): A = myifThenElse[A](cond,thenp,elsep)




  case class MaybeSFunction(f: Either[StagedFunction[Dyn, Rep[Int]], Dyn => Rep[Int]]) {
    def apply(dyn: Dyn): Rep[Int] = f.fold(fa => fa(dyn), fb => fb(dyn))

    def mkfun(stat: Stat, dyn: Dyn): Rep[Int] = f.fold(fa => fa(dyn), fb => {
      val expose = exposeDyn(stat)
      val t = doGlobalLambda(fb, Some("Base" + stat.toSig()), Some("Base" + stat.toSig()))(expose, exposeRepFromRep[Int])
      t(dyn)
    })
  }

  object MaybeSFunction {
    def apply(f: StagedFunction[Dyn, Rep[Int]]): MaybeSFunction = MaybeSFunction(Left(f))

    def apply(f: (Dyn => Rep[Int])): MaybeSFunction = MaybeSFunction(Right(f))
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
      override val evnum: scala.Numeric[X] = null
      //ugly!!!!
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
      override val evnum: Numeric[T] = null
      //ugly!!!!
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
      override val evnum: Numeric[T] = null
      //ugly!!!!!
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
      (statx, dynx) match {
        case (stat: Stat_GT_IM, dyn: Dyn_GT_IM) => apply(stat, dyn)
        case (stat: Stat_GTI_IM, dyn: Dyn_GTI_IM) => apply(stat, dyn)
        case (stat: Stat_GTT_IM, dyn: Dyn_GTT_IM) => apply(stat, dyn)
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
      new GTT_IM(g, s, tw)
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
      (new Dyn_GTT_IM(a, c, e), f)
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


  abstract class Base(n: AInt, m: AInt)

  abstract class Header(n: AInt, m: AInt) extends Base(n, m) with RepSelector2 {
    def getn() = repselect(n)

    def getm() = repselect(m)
  }

  class Stat(val n: AInt, val m: AInt) extends Header(n, m) with StatSelector2 {
    //(x: Data, y: Data, n: AInt, lb: AInt, im: IMFull, v: AInt, tw: Option[TwiddleScaling], par: Option[Int], precompute: Boolean, expdata: ExposeRep[Data], scalars: Boolean)
    def toName(): String = {
      if (graphname){

        val ssize: (String,String) = n.ev.fold[Int, (String,String)](n.a, fa => (",n: Int",""), fb=> (("",s", n = ${n.a}")))
        val slb: (String,String) = m.ev.fold[Int, (String,String)](m.a, fa => (",m: Int",""), fb=> (("",s", m = ${m.a}")))


        val stats = "[" + s"${ssize._2} ${slb._2} ]".trim.stripPrefix(",") //.dropWhile(s => s != ',' && s != ']')

        //dyns + stats
        stats
      }
      else toSig()
    }
    def toSig(): String = "n" + repselect(n).toSig() + "m" + repselect(m).toSig()
  }

  class Dyn(n: AInt, m: AInt) extends Header(n, m) with DynSelector2 {  }


  object Mix {
    def apply(stat: Stat, dyn: Dyn): Mix = {
      val n: AInt = stat.getn().toOneEntry().getOrElse(dyn.getn().toOneEntry().get)
      val m = stat.getm().toOneEntry().getOrElse(dyn.getm().toOneEntry().get)

      Mix( n, m )
    }
  }


  case class Mix( n: AInt, m: AInt) extends Base(n, m) {
    def getDyn(): Dyn = new Dyn(n, m)
    def getStat(): Stat = new Stat(n, m )
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
        val t = stat.n.ev.fresh()(stat.n.evtyp) ++ stat.m.ev.fresh()(stat.n.evtyp)
        t
      }
      val vec2t: Vector[Exp[_]] => Dyn = (in: Vector[Exp[_]]) => {
        val (an, on) = stat.n.ev.fetch[Int](in)
        val (alb, olb) = stat.m.ev.fetch[Int](an)
        val n: AInt = OR2AInt(on)
        val m: AInt = OR2AInt(olb)
        new Dyn(n,m)
      }
      val t2vec: Dyn => Vector[Exp[_]] = (in: Dyn) => {
          OO2Exp(in.getn()) ++
          OO2Exp(in.getm())
      }
    }
  }
/*

  case class iData(in: Data, out: Data, i: AInt, scalars: Boolean)

  def exposeiData(expdata: ExposeRep[Data]) = new ExposeRep[iData]() {
    val freshExps = (u: Unit) => Vector(Arg[Int]) ++ expdata.freshExps() ++ expdata.freshExps()
    val vec2t: Vector[Exp[_]] => iData = (in: Vector[Exp[_]]) => {
      val t = in(0).asInstanceOf[Rep[Int]]
      val input = expdata.vec2t(in.tail)
      val vecs = expdata.t2vec(input)
      val rest = in.tail.drop(vecs.size)
      val output = expdata.vec2t(rest)
      iData(input, output, R2AInt(t),false)
    }
    val t2vec: iData => Vector[Exp[_]] = (in: iData) => {
      val t = Vector(in.i.ev.toRep(in.i.a))
      val input: Vector[Exp[_]] = expdata.t2vec(in.in)
      val output: Vector[Exp[_]] = expdata.t2vec(in.out)
      t ++ input ++ output
    }
  }*/


}