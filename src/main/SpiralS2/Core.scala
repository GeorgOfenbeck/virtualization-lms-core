package SpiralS2

import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

object Constants {
  val encode_right = 1
  val encode_left = -1
}

class Core(variant: BreakDown.Tree, val lookup: BRMaps, val testsize: Int,
           val WHT: Boolean = true,
           val static_size: Option[Int] = None,
           val interleaved: Boolean = false,
           val thread: Boolean = false,
           val base_default: Int = 0,
           val twid_inline: Boolean = true,
           val twid_default_precomp: Boolean = true,
           val inplace: Boolean = false,
           val inline: Boolean = true
          ) extends Header {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadNoTuples with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenOrderingOps {
    val IR: self.type = self
  }


  val basecase_size: Option[Int] = if (base_default == 0) None else Some(base_default)

  def inlinec(oe: OptionalEntry {type T = Int}): Boolean = oe.a match {
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


  def getmyID(size: AInt, idsofar: AInt): AInt = {
    idsofar.ev.fold[Int, AInt](idsofar.a, fa => {
      val t = myifThenElse(ordering_equiv(fa, Const(-99)), {
        val t: Rep[Int] = size.ev.toRep(size.a)
        lookupsize2id(t)
      }, {
        fa
      })
      R2AInt(t)
    }, fb => {
      if (fb == -99) {
        size.ev.fold[Int, AInt](size.a, fa => {
          R2AInt(lookupsize2id(fa))
        }, fb => {
          val id = lookup.size2id.getOrElse(fb, -99)
          toOE(id)
        })
      } else toOE(fb)
    })

  }

  def getChildIDs(id: AInt): (AInt, AInt) = {
    id.ev.fold[Int, (AInt, AInt)](id.a, fa => {
      val lid = lookupid2lid(fa)
      val rid = lookupid2rid(fa)
      (R2AInt(lid), R2AInt(rid))
    }, fb => {
      val (idl, idr) = lookup.id2ids.getOrElse(fb, (-99, -99))
      (toOE(idl), toOE(idr))
    })
  }


  def chooseRadix(n: AInt, id: AInt): AInt =
    id.ev.fold[Int, AInt](id.a, fa => {
      R2AInt(choose_radix(fa))
    }, fb => {
      toOE(lookup.id2radix.getOrElse(fb, 2))
    })


  def chooseTwiddle(l: AInt): AInt = if (l.ev.isRep()) {
    //R2AInt(choose_twid(l.ev.toRep(l.a)))
    ???
  } else {
    toOE(l.a match {
      case ll: List[Int] => {
        val t: Boolean = ??? // = lookup.getOrElse(ll, (2, false, true))._3
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


  var hack: Boolean = false

  def loop[A](mix: Mix, in: Data, out: Data, par: Option[Int], body: iData => Data): Data = {
    val till = mix.lb
    if (!unroll(mix) ) {
      sigmaLoop(till.ev.toRep(till.a), par, in.getdata(), out.getdata(), body)(exposeiData(mix.expdata), mix.expdata)
    }
    else {
      till.a match {
        case x: Int => {
          val beforehack = hack
          if (!hack) hack = true
          val finalresult: (Data, Data) = (0 until x).foldLeft(in, out) {
            (acc, ele) => {
              val (input, output) = acc
              val returndata: Data = body(iData(input, output, ele))
              (input, returndata)
            }
          }
          hack = beforehack
          finalresult._2
        }
        case _ => ??? //this should not be possible
      }
    }
  }

  case class iData(in: Data, out: Data, i: AInt)

  def exposeiData(expdata: ExposeRep[Data]) = new ExposeRep[iData]() {

    val freshExps = (u: Unit) => Vector(Arg[Int]) ++ expdata.freshExps() ++ expdata.freshExps()
    val vec2t: Vector[Exp[_]] => iData = (in: Vector[Exp[_]]) => {
      val t = in(0).asInstanceOf[Rep[Int]]
      val input = expdata.vec2t(in.tail)
      val vecs = expdata.t2vec(input)
      val rest = in.tail.drop(vecs.size)
      val output = expdata.vec2t(rest)
      iData(input, output, R2AInt(t))
    }
    val t2vec: iData => Vector[Exp[_]] = (in: iData) => {
      val t = Vector(in.i.ev.toRep(in.i.a))
      val input: Vector[Exp[_]] = expdata.t2vec(in.in)
      val output: Vector[Exp[_]] = expdata.t2vec(in.out)
      t ++ input ++ output
    }
  }

  def F2(stat: Stat, inline: Boolean): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      loop(mix, mix.x, mix.y, None, { idata => {
        val t01 = idata.in.apply(resolveH(mix.im.gather(), toOE(0), idata.i))
        val t02 = idata.in.apply(resolveH(mix.im.gather(), toOE(1), idata.i))
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
        val idx0 = resolveH(mix.im.scatter(), toOE(0), idata.i)
        val val1 = idata.out.update(idx0, (t1 + t2))
        val idx1 = resolveH(mix.im.scatter(), toOE(1), idata.i)
        val val2 = val1.update(idx1, (t1 - t2))
        val2
      }
      })
    }
    if (inline) MaybeSFunction(stageme) else
      MaybeSFunction(doGlobalLambda(stageme, Some("F2" + stat.toSig()), Some("F2" + stat.toSig()))(expose, stat.expdata))
  }

  //we always "uprank" r
  def fuseIM(r: IMHBase, s: IMHBase, lv: AInt): IMH = IMH((r.base + (r.s0 * s.base)) + r.s1 * lv, r.s0 * s.s0, (toOE(0) + (r.s0 * s.s1)))

  /*  def addtoList(mix: Mix, direction: AInt): LInt = {
      if (mix.pos.ev.isRep()) R2LInt(listadd(mix.pos.ev.toRep(mix.pos.a), direction.ev.toRep(direction.a)))
      else {
        //clean me up!
        val l = mix.pos.a.asInstanceOf[List[Int]]
        val d = direction.a.asInstanceOf[Int]
        val t = l :+ d
        toOEL(t)
      }
    }*/


  def DFT_CT(stat: Stat,inline: Boolean): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix_b = Mix(stat, dyn)
      val (mix, parx): (Mix, Option[Int]) = mix_b.par.fold[(Mix, Option[Int])]((mix_b, None))(p => mix_b.lb.a match {
        case x: Int => if (x < 2) (mix_b, None) else (mix_b.copy(par = None), Some(p))
        case _ => (mix_b.copy(par = None), Some(p))
      })

      val ( rid,lid) = getChildIDs(mix.pos) //flipped because of radix

      val k = chooseRadix(mix.n, mix.pos)
      val m = mix.n / k

      val inlinechildren = inlinec(mix.getStat().getn())
      loop(mix, mix.x, mix.y, parx, { idata => {
        val stage1mix: Mix = {
          val (s0, s1) = if (!WHT) (k, toOE(1)) else (toOE(1), m)
          val inner = IMH(toOE(0), s0, s1)
          val s1_gather: IMH = fuseIM(mix.im.gather(), inner, idata.i)
          val s1_scatter: IMH = if (inplace && !hack) {
            fuseIM(mix.im.scatter(), IMH(toOE(0), toOE(1), m), idata.i) //fuseIM(mix.im.scatter(), IMH(toOE(0), m, toOE(1)), idata.i)
          } else IMH(toOE(0), toOE(1), m)
          val nim = mix.im match {
            case gt: GT_IM => GT_IM(s1_gather, s1_scatter)
            case gti: GTI_IM => GTT_IM(s1_gather, s1_scatter, fuseIM(gti.twim, inner, idata.i))
            case gtt: GTT_IM => GTT_IM(s1_gather, s1_scatter, fuseIM(gtt.twim, inner, idata.i))
          }
          val stage1_target: Data = {
            if (hack) {
              mix.n.ev.fold[Int, ScalarVector](mix.n.a, fa => {
                ???
              }, fb => {
                ScalarVector(new Array[Exp[Double]](fb * 2))
              })
            } else {
              if (inplace) idata.out else mix.y.create(mix.n)
            }
          }
          mix.copy(x = idata.in, y = stage1_target, n = m, lb = if(inline) k else R2AInt(k.ev.toRep(k.a)), im = nim, v = idata.i, pos = rid)
        }
        val dataafterS1 = DFT(stage1mix.getStat(), inlinechildren)(stage1mix.getDyn())


        val stage2mix: Mix = {
          val twid = TwiddleScaling(mix.n, m, toOE(1))
          val s2_gather: IMH = IMH(toOE(0), m, toOE(1))
          val s2_scatter: IMH = fuseIM(mix.im.scatter(), s2_gather, idata.i)
          val nim = if (inplace && !hack) GTI_IM(s2_scatter, s2_gather) else if (WHT) GT_IM(s2_gather, s2_scatter) else {
            GTT_IM(s2_gather, s2_scatter, s2_gather)
          }

          mix.copy(x = dataafterS1, y = idata.out, n = k, lb = if(inline) m else R2AInt(m.ev.toRep(m.a)), im = nim, v = idata.i, tw = Some(twid), pos = lid)
        }
        DFT(stage2mix.getStat(),inlinechildren)(stage2mix.getDyn())
      }
      })
    }
    if (inline) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT_CT" + stat.toSig()), Some("DFT_CT" + stat.toSig()))(expose, stat.expdata))
  }

  def binsearchpos(mix: Mix, check: Rep[Int], low: Int, high: Int): Data = {
    implicit val expose = mix.expdata
    if (low == high) {
      val nmix = mix.copy(pos = toOE(high))
      DFT(nmix.getStat(), inlinec(mix.getStat().getn())).mkfun(nmix.getStat(), nmix.getDyn())
    } else {
      myifThenElse(ordering_equiv(check, Const(high)), {
        val nmix = mix.copy(pos = toOE(high))
        (nmix.pos.a, nmix.n.a) match {
          case (ipos: Int,in: Int) => { //We check combinations that can never occur and would then yield inifnite code - workaround
            val realsize = lookup.id2size(ipos)
            if (realsize != in && in != 2)
              nmix.x
            else
              DFT(nmix.getStat(), inlinec(mix.getStat().getn())).mkfun(nmix.getStat(), nmix.getDyn())
          }
          case _ => ??? //this should never happen
        }

      }, {
        binsearchpos(mix, check, low, high - 1)
      })
    }
  }




  def binsearch2pow(mix: Mix, check: Rep[Int], low: Int, high: Int): Data = {
    val mid = low + (high - low) / 2
    implicit val expose = mix.expdata

    myifThenElse(ordering_equiv(check, Const(high)), {
      val nmix = mix.copy(n = toOE(high))
      nmix.pos.ev._if(nmix.pos.ev.less(nmix.pos.a, nmix.pos.ev.const(lookup.id2radix.size)), {
        binsearchpos(nmix, nmix.pos.ev.toRep(nmix.pos.a), 0, lookup.id2radix.size-1)
      }, {
        val nmix2 = nmix.copy(pos = toOE(-99))
        DFT(nmix2.getStat(), inlinec(mix.getStat().getn())).mkfun(nmix2.getStat(), nmix2.getDyn())
      })

    }, {
      if (high == 2) {
        val nmix = mix.copy(n = toOE(high))
        DFT(nmix.getStat(), inlinec(mix.getStat().getn())).mkfun(nmix.getStat(), nmix.getDyn())
      }
      else
        binsearch2pow(mix, check, low, high / 2)
    })
  }

  /*def DFT_placeholder(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => dyn.y
    if (inline(stat.getn())) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT_uneven" + stat.toSig()), Some("DFT_uneven" + stat.toSig()))(expose, stat.expdata))
  }*/

  def DFT_call(mix: Mix, stat: Stat, dyn: Dyn): Data = {
    //val dftct = mix.n.ev.equiv(mix.n.ev.mod(mix.n.a, mix.n.ev.const(2)), mix.n.ev.const(0))
    implicit val expose = mix.expdata
    //mix.n.ev._if(dftct, {
    val bool = mix.n.ev.equiv(mix.n.a, mix.n.ev.const(2))
    mix.n.ev._if(bool, {
      F2(stat, inlinec(mix.getStat().getn()))(dyn)
    }, {
      DFT_CT(stat, inlinec(mix.getStat().getn()))(dyn)
    })
    /*    }, {
          DFT_placeholder(stat)(dyn)
        })*/
  }

  def DFT(stat: Stat, inline: Boolean): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix2 = Mix(stat, dyn)

      val myid = getmyID(mix2.n, mix2.pos)

      //listadd(mix2.pos.ev.toRep(mix2.pos.a), mix2.n.ev.toRep(mix2.n.a))
      val mix = mix2.copy(pos = myid)
      implicit val exposedata = mix.expdata
      if (basecase_size.isDefined && mix.n.ev.isRep()) {
        val isbasecase = mix.n.ev.less(mix.n.a, mix.n.ev.const(basecase_size.get + 1))
        mix.n.ev._if(isbasecase, {
          binsearch2pow(mix, mix.n.ev.toRep(mix.n.a), 2, basecase_size.get)
        }, {
          DFT_call(mix, mix.getStat(), mix.getDyn())
        })
      }
      else DFT_call(mix, mix.getStat(), mix.getDyn())
    }
    if (inline) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT" + stat.toSig()), Some("DFT" + stat.toSig()))(expose, stat.expdata))
  }

  def ini(stat: Stat): (Dyn => Data) = {
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      DFT(stat,true)(dyn)
    }
    stageme
  }
}


