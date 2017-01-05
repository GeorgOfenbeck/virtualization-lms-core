package SpiralS2

import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

object Constants {
  val encode_right = 1
  val encode_left = -1
}

class Core(variant: BreakDown.Tree, val lookup: Map[List[Int], (Int, Boolean, Boolean)], val testsize: Int,
           val WHT: Boolean = true,
           val static_size: Option[Int] = None,
           val interleaved: Boolean = false,
           val thread: Boolean = false,
           val base_default: Int = 0,
           val twid_inline: Boolean = true,
           val twid_default_precomp: Boolean = true,
           val inplace: Boolean = false,
           val inline:Boolean = true
          ) extends Header {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenOrderingOps {
    val IR: self.type = self
  }



  val basecase_size: Option[Int] = if (base_default == 0) None else Some(base_default)

  def inline(oe: OptionalEntry {type T = Int}): Boolean = oe.a match {
    case Some(n: Int) => inline && basecase_size.fold(false)(fb => n <= fb)
    case _ => false
  }

  def resolveH(h: IMHBase, i: AInt, v: AInt): AInt = h.base + (h.s0 * i) + (h.s1 * v)

  def resolveTwid(sample: DataEle, mix: Mix, n: AInt, d: AInt, k: AInt, i: AInt): DataEle = {
    if (twid_inline && !n.ev.isRep() && !d.ev.isRep() && !k.ev.isRep() && !i.ev.isRep()) {
      (n.a, d.a, k.a, i.a) match {
        case (ni: Int, di: Int, ki: Int, ii: Int) => {
          val t = Twiddle(ni, di, ki, ii)
          //new SComplex(compcreate(Const(t.re), Const(t.im)))
          sample.create(Const(t.re), Const(t.im))
        }
        case _ => ???
      }
    } else {
      val (nr, dr, kr, ir): (Rep[Int], Rep[Int], Rep[Int], Rep[Int]) = (n.ev.toRep(n.a), d.ev.toRep(d.a), k.ev.toRep(k.a), i.ev.toRep(i.a))
      if (mix.precompute) {
        sample.create(dtwiddle_apply_index_store(nr, dr, kr, ir, true), dtwiddle_apply_index_store(nr, dr, kr, ir, false))
      } else {
        if (twid_default_precomp) {
          sample.create(dtwiddle_load(ir), dtwiddle_load(ir))
        }
        else {
          sample.create(dtwiddle_apply_index(nr, dr, kr, ir, true), dtwiddle_apply_index(nr, dr, kr, ir, false))
        }
      }
    }
  }

  def chooseRadix(n: AInt, l: LInt): AInt = if (l.ev.isRep()) {
    R2AInt(choose_radix(l.ev.toRep(l.a)))
  } else {
    toOE(l.a match {
      case ll: List[Int] => {
        lookup.getOrElse(ll, (2, false, false))._1
      }
      case _ => ???
    })
  }


  def chooseTwiddle(l: LInt): AInt = if (l.ev.isRep()) {
    R2AInt(choose_twid(l.ev.toRep(l.a)))
  } else {
    toOE(l.a match {
      case ll: List[Int] => {
        val t = lookup.getOrElse(ll, (2, false, true))._3
        if (t) 1 else 0
      }
      case _ => ???
    })
  }

  def unroll(mix: Mix): Boolean = {
    if (mix.lb.ev.isRep() || mix.n.ev.isRep()) false else {
      mix.n.ev.less(mix.n.a, mix.n.ev.const(basecase_size.getOrElse(0) + 1)) match {
        case b: Boolean => b
        case _ => false
      }
    }
  }

  def loop[A](mix: Mix, ini: Data, par: Option[Int], body: iData => Data): Data = {
    val till = mix.lb
    if (!unroll(mix)) sumFoldx(till.ev.toRep(till.a), par, ini.getdata(), mix.y.getdata(), body)(exposeiData(mix.expdata), mix.expdata)
    else {
      till.a match {
        case x: Int => {
          val partial = (0 until x).map(ele => {
            ini match {
              case sc: SComplexVector => body(iData(sc, ele))
              case ic: InterleavedComplexVector => body(iData(ic, ele))
              case _ => ???
            }
          })
          partial.reduce((x, y) => x.same(x, y))

        }
        case _ => ??? //this should not be possible
      }

    }
  }

  case class iData(d: Data, i: AInt)

  def exposeiData(expdata: ExposeRep[Data]) = new ExposeRep[iData]() {

    val freshExps = (u: Unit) => expdata.freshExps() ++ Vector(Arg[Int])
    val vec2t: Vector[Exp[_]] => iData = (in: Vector[Exp[_]]) => {
      val t = in(1).asInstanceOf[Rep[Int]]
      val s = expdata.vec2t(in)
      iData(s, R2AInt(t))
    }
    val t2vec: iData => Vector[Exp[_]] = (in: iData) => {
      val t: Vector[Exp[_]] = expdata.t2vec(in.d)
      val s = Vector(in.i.ev.toRep(in.i.a))
      t ++ s
    }
  }

  def F2(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      loop(mix, mix.x, None, { idata => {
        val t01 = dyn.x.apply(resolveH(mix.im.gather(), toOE(0), idata.i))
        val t02 = dyn.x.apply(resolveH(mix.im.gather(), toOE(1), idata.i))
        val (t1, t2): (DataEle, DataEle) = mix.im match {
          case im_git: GT_IM => (t01, t02)
          case im_gtt: GTT_IM => if (WHT) (t01, t02) else mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
            fb => (
              (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(0), idata.i)) * t01),
              (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(1), idata.i)) * t02)))
          case im_gti: GTI_IM => if (WHT) (t01, t02) else mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
            fb => (
              (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(0), idata.i)) * t01),
              (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(1), idata.i)) * t02)))
        }
        mix.y.update(resolveH(mix.im.scatter(), toOE(0), idata.i), (t1 + t2)).update(resolveH(mix.im.scatter(), toOE(1), idata.i), (t1 - t2))
      }
      })
    }
    if (inline(stat.getn())) MaybeSFunction(stageme) else
      MaybeSFunction(doGlobalLambda(stageme, Some("F2" + stat.toSig()), Some("F2" + stat.toSig()))(expose, stat.expdata))
  }

  //we always "uprank" r
  def fuseIM(r: IMHBase, s: IMHBase, lv: AInt): IMH = IMH((r.base + (r.s0 * s.base)) + r.s1 * lv, r.s0 * s.s0, (toOE(0) + (r.s0 * s.s1)))

  def DFT_CT(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix_b = Mix(stat, dyn)
      val (mix, parx): (Mix, Option[Int]) = mix_b.par.fold[(Mix, Option[Int])]((mix_b, None))(p => mix_b.lb.a match {
        case x: Int => if (x < 2) (mix_b, None) else (mix_b.copy(par = None), Some(p))
        case _ => (mix_b.copy(par = None), Some(p))
      })
      val m = chooseRadix(mix.n, mix.pos)
      val k = mix.n / m
      loop(mix, mix.x, parx, { idata => {
        val stage1mix: Mix = {
          val (s0, s1) = if (!WHT) (k, toOE(1)) else (toOE(1), m)
          val inner = IMH(toOE(0), s0, s1)
          val s1_gather: IMH = fuseIM(mix.im.gather(), inner, idata.i)
          val s1_scatter: IMH = if (inplace) {
            fuseIM(mix.im.scatter(), IMH(toOE(0), toOE(1), m), idata.i) //fuseIM(mix.im.scatter(), IMH(toOE(0), m, toOE(1)), idata.i)
          } else IMH(toOE(0), toOE(1), m)
          val nim = mix.im match {
            case gt: GT_IM => GT_IM(s1_gather, s1_scatter)
            case gti: GTI_IM => GTT_IM(s1_gather, s1_scatter, fuseIM(gti.twim, inner, idata.i))
            case gtt: GTT_IM => GTT_IM(s1_gather, s1_scatter, fuseIM(gtt.twim, inner, idata.i))
          }
          val stage1_target = if (inplace) mix.y else mix.y.create(mix.n)
          val npos = R2LInt(listadd(mix.pos.ev.toRep(mix.pos.a), Const(Constants.encode_right)))
          mix.copy(x = mix.x, y = stage1_target, n = m, lb = k, im = nim, v = idata.i, pos = npos)
        }
        val dataafterS1 = DFT(stage1mix.getStat())(stage1mix.getDyn())
        val stage2mix: Mix = {
          val twid = TwiddleScaling(mix.n, m, toOE(1))
          val s2_gather: IMH = IMH(toOE(0), m, toOE(1))
          val s2_scatter: IMH = fuseIM(mix.im.scatter(), s2_gather, idata.i)
          val nim = if (inplace) GTI_IM(s2_scatter, s2_gather) else if (WHT) GT_IM(s2_gather, s2_scatter) else {
            GTT_IM(s2_gather, s2_scatter, s2_gather)
          }
          val npos = R2LInt(listadd(mix.pos.ev.toRep(mix.pos.a), Const(Constants.encode_left)))
          mix.copy(x = dataafterS1, y = mix.y, n = k, lb = m, im = nim, v = idata.i, tw = Some(twid), pos = npos)
        }
        DFT(stage2mix.getStat())(stage2mix.getDyn())
      }
      })
    }
    if (inline(stat.getn())) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT_CT" + stat.toSig()), Some("DFT_CT" + stat.toSig()))(expose, stat.expdata))
  }


  def binsearch(mix: Mix, check: Rep[Int], low: Int, high: Int): Data = {
    val mid = low + (high - low) / 2
    implicit val expose = mix.expdata
    if ((high - low) <= 1) {
      myifThenElse(check < Const(high), {
        val nmix = mix.copy(n = toOE(low))
        DFT(nmix.getStat()).mkfun(nmix.getStat(), nmix.getDyn())
      }, {
        val nmix = mix.copy(n = toOE(high))
        DFT(nmix.getStat()).mkfun(nmix.getStat(), nmix.getDyn())
      })
    }
    else myifThenElse(check < Const(mid), {
      binsearch(mix, check, low, mid)
    }, {
      binsearch(mix, check, mid, high)
    })
  }

  def DFT_placeholder(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => dyn.y
    if (inline(stat.getn())) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT_uneven" + stat.toSig()), Some("DFT_uneven" + stat.toSig()))(expose, stat.expdata))
  }

  def DFT_call(mix: Mix, stat: Stat, dyn: Dyn): Data = {
    val dftct = mix.n.ev.equiv(mix.n.ev.mod(mix.n.a, mix.n.ev.const(2)), mix.n.ev.const(0))
    implicit val expose = mix.expdata
    mix.n.ev._if(dftct, {
      val bool = mix.n.ev.equiv(mix.n.a, mix.n.ev.const(2))
      mix.n.ev._if(bool, {
        F2(stat)(dyn)
      }, {
        DFT_CT(stat)(dyn)
      })
    }, {
      DFT_placeholder(stat)(dyn)
    })
  }

  def DFT(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix2 = Mix(stat, dyn)
      val nl = listadd(mix2.pos.ev.toRep(mix2.pos.a), mix2.n.ev.toRep(mix2.n.a))
      val mix = mix2.copy(pos = R2LInt(nl))
      implicit val exposedata = mix.expdata
      if (basecase_size.isDefined && mix.n.ev.isRep()) {
        val isbasecase = mix.n.ev.less(mix.n.a, mix.n.ev.const(basecase_size.get + 1))
        mix.n.ev._if(isbasecase, {
          binsearch(mix, mix.n.ev.toRep(mix.n.a), 2, basecase_size.get)
        }, {
          DFT_call(mix, mix.getStat(), mix.getDyn())
        })
      }
      else DFT_call(mix, mix.getStat(), mix.getDyn())
    }
    if (inline(stat.getn())) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT" + stat.toSig()), Some("DFT" + stat.toSig()))(expose, stat.expdata))
  }

  def ini(stat: Stat): (Dyn => Data) = {
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      DFT(stat)(dyn)
    }
    stageme
  }
}


