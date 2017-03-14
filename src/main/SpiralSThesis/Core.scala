package SpiralSThesis

import SpiralSThesis.Twiddle.MathUtilities

import scala.lms.targets.graphviz.{GraphVizCallGraph, GraphVizExport}
import scala.lms.targets.scalalike._

class Core(val radix_choice: Map[Int, Int], val interleaved: Boolean = false, val thread: Boolean = false,
           val base_default: Int = 0, val twid_inline: Boolean = true, val twid_default_precomp: Boolean = true, val inplace: Boolean = false,
           val inline: Boolean = true, val ignore_config: Boolean = true) extends Header {
 self =>
 val emitGraph = new GraphVizCallGraph {
  override val IR: self.type = self
 }
 override val codegen = new ScalaCodegen with EmitHeadNoTuples with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenOrderingOps {
  val IR: self.type = self
 }

 val basecase_size: Option[Int] = if (base_default < 2) None else Some(base_default)

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
     sample.create(Const(t.re), Const(t.im))
    }
    case _ => ???
   }
  } else {
   val (nr, dr, kr, ir): (Rep[Int], Rep[Int], Rep[Int], Rep[Int]) = (n.ev.toRep(n.a), d.ev.toRep(d.a), k.ev.toRep(k.a), i.ev.toRep(i.a))
   if (mix.precompute) sample.create(dtwiddle_apply_index(nr, dr, kr, ir, true), dtwiddle_apply_index(nr, dr, kr, ir, false))
   else if (twid_default_precomp) sample.create(dtwiddle_apply_index_load(nr, dr, kr, ir, true), dtwiddle_apply_index_load(nr, dr, kr, ir, false))
   else sample.create(dtwiddle_apply_index(nr, dr, kr, ir, true), dtwiddle_apply_index(nr, dr, kr, ir, false))
  }
 }

 def chooseRadix(n: AInt): AInt = n.ev.fold[Int, AInt](n.a, fna => R2AInt(choose_radix(fna)), fnb => toOE(radix_choice(fnb)))

 def unroll(mix: Mix): Boolean = {
  if (mix.lb.ev.isRep() || mix.n.ev.isRep()) false else {
   mix.n.ev.less(mix.n.a, mix.n.ev.const(basecase_size.getOrElse(0) + 1)) match {
    case b: Boolean => b
    case _ => false
   }
  }
 }

 def loop[A](mix: Mix, in: Data, out: Data, par: Option[Int], body: iData => Data): Data = {
  val till = mix.lb
  if (!unroll(mix)) sigmaLoop(till.ev.toRep(till.a), par, in.getdata(), out.getdata(), body)(exposeiData(mix.expdata), mix.expdata)
  else till.a match {
   case x: Int => (0 until x).foldLeft(in, out) { (acc, ele) => (acc._1, body(iData(acc._1, acc._2, ele, true))) }._2
   case _ => ??? //this should not be possible
  }
 }

 def F2(stat: Stat, inline: Boolean): MaybeSFunction = {
  val expose = exposeDyn(stat)
  val stageme: (Dyn => Data) = (dyn: Dyn) => {
   val mix = Mix(stat, dyn)
   loop(mix, mix.x, mix.y, None, { idata => {
     val v0 = resolveH(mix.im.gather(), toOE(0), idata.i)
     val v1 = resolveH(mix.im.gather(), toOE(1), idata.i)
    val t01 = idata.in.apply(resolveH(mix.im.gather(), toOE(0), idata.i))
    val t02 = idata.in.apply(resolveH(mix.im.gather(), toOE(1), idata.i))
    val (t1, t2): (DataEle, DataEle) = mix.im match {
     case im_git: GT_IM => (t01, t02)
     case im_gtt: GTT_IM => mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
      fb => (
       (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(0), idata.i)) * t01),
       (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(1), idata.i)) * t02)))
     case im_gti: GTI_IM => mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
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
  if (inline) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("F2" + stat.toSig()), Some("F2" + stat.toName()))(expose, stat.expdata))
 }

 def fuseIM(r: IMHBase, s: IMHBase, lv: AInt): IMH = IMH((r.base + (r.s0 * s.base)) + r.s1 * lv, r.s0 * s.s0, (toOE(0) + (r.s0 * s.s1)))


 def DFT_SplitRadix(stat: Stat, inline: Boolean): MaybeSFunction = {
  val expose = exposeDyn(stat)
  val stageme: (Dyn => Data) = (dyn: Dyn) => {
   val mix_b = Mix(stat, dyn)
   val (mix, parx): (Mix, Option[Int]) = mix_b.par.fold[(Mix, Option[Int])]((mix_b, None))(p => mix_b.lb.a match {
    case x: Int => if (x < 2) (mix_b, None) else (mix_b.copy(par = None), Some(p))
    case _ => (mix_b.copy(par = None), Some(p))
   })

   val nhalf = mix.n / toOE(2)
   val nquart = mix.n / toOE(4)

   val inlinechildren = inlinec(mix.getStat().getn())
   loop(mix, mix.x, mix.y, parx, { idata => {

     //L(n,2)

     val lowhalf_intarget: Data = {
       if (true) {
         mix.n.ev.fold[Int, ScalarVector](mix.n.a, fa => {
           ???
         }, fb => {
           ScalarVector(new Array[Exp[Double]](fb ))
         })
       } else {
         mix.y.create(mix.n / 2)
       }
     }


     val highhalf_intarget: Data = {
       if (true) {
         mix.n.ev.fold[Int, ScalarVector](mix.n.a, fa => {
           ???
         }, fb => {
           ScalarVector(new Array[Exp[Double]](fb ))
         })
       } else {
         mix.y.create(mix.n / 2)
       }
     }

     val halfmix: Mix = {
       val s2_gather: IMH = IMH(mix.im.gather().base,mix.im.gather().s0,mix.im.gather().s1)
       val s2_scatter: IMH = IMH(toOE(0), toOE(1), toOE(2))
       val nim = GT_IM(s2_gather, s2_scatter)
       mix.copy(x = mix.x, y = lowhalf_intarget, n = toOE(2), lb = nquart, im = nim, tw = None)
     }

     //val halfmix = mix.copy(n = mix.n/2, lb = mix.n/4)
     //gather low half
     val lowhalf_input = {
       val mix = halfmix
       loop(halfmix, mix.x, lowhalf_intarget, parx, { idata => {
         val nh = halfmix.n
         val t01 = idata.in.apply(resolveH(mix.im.gather(), toOE(0), idata.i) + nh)
         val t02 = idata.in.apply(resolveH(mix.im.gather(), toOE(1), idata.i) + nh)
         val (t1, t2): (DataEle, DataEle) = mix.im match {
           case im_git: GT_IM => (t01, t02)
           case im_gtt: GTT_IM => mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
             fb => (
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(0), idata.i)) * t01),
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(1), idata.i)) * t02)))
           case im_gti: GTI_IM => mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
             fb => (
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(0), idata.i)) * t01),
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(1), idata.i)) * t02)))
         }
         val idx0 = resolveH(mix.im.scatter(), toOE(0), idata.i)
         val val1 = idata.out.update(idx0, (t1))
         val idx1 = resolveH(mix.im.scatter(), toOE(1), idata.i)
         val val2 = val1.update(idx1, (t2))
         val2
       }})
     }
     //gather high half

     val highhalf_input = {
       val mix = halfmix
       loop(halfmix, mix.x, highhalf_intarget, parx, { idata => {
         val t01 = idata.in.apply(resolveH(mix.im.gather(), toOE(0), idata.i))
         val t02 = idata.in.apply(resolveH(mix.im.gather(), toOE(1), idata.i))
         val (t1, t2): (DataEle, DataEle) = mix.im match {
           case im_git: GT_IM => (t01, t02)
           case im_gtt: GTT_IM => mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
             fb => (
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(0), idata.i)) * t01),
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gtt.twim, toOE(1), idata.i)) * t02)))
           case im_gti: GTI_IM => mix.tw.fold[(DataEle, DataEle)]((t01, t02))(
             fb => (
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(0), idata.i)) * t01),
               (resolveTwid(t01, mix, fb.n, fb.d, fb.k, resolveH(im_gti.twim, toOE(1), idata.i)) * t02)))
         }
         val idx0 = resolveH(mix.im.scatter(), toOE(0), idata.i)
         val val1 = idata.out.update(idx0, (t1))
         val idx1 = resolveH(mix.im.scatter(), toOE(1), idata.i)
         val val2 = val1.update(idx1, (t2))
         val2
       }})
     }


     val stage1_target: Data = {
       if (true) {
         mix.n.ev.fold[Int, ScalarVector](mix.n.a, fa => {
           ???
         }, fb => {
           ScalarVector(new Array[Exp[Double]](fb * 2))
         })
       } else {
         mix.y.create(mix.n)
       }
     }
/*
     val lowhalf_target: Data = {
       if (idata.scalars) {
         mix.n.ev.fold[Int, ScalarVector](mix.n.a, fa => {
           ???
         }, fb => {
           ScalarVector(new Array[Exp[Double]](fb ))
         })
       } else {
         mix.y.create(mix.n / 2)
       }
     }

    // I(2) tensor dft(n/4) compose L(n/2,2)
     //ignore the L for now
    val stage1lowmix: Mix = {
     val (s0, s1) = (toOE(2), toOE(1))
     val inner = IMH(toOE(0), s0, s1)
     val s1_gather: IMH = inner//fuseIM(mix.im.gather(), inner, idata.i)
     val s1_scatter: IMH = inner//IMH(toOE(0), toOE(1), toOE(2))
     val nim = GT_IM(s1_gather, s1_scatter)

     mix.copy(x = lowhalf_input, y = lowhalf_target, n = nquart, lb = toOE(2), im = nim, scalars = idata.scalars)
    }
    //((D2(k) compose F_2()) tensor I(n/4))
    //compose T3L(n/2,2,k)

    val datalowhalf1 = DFT(stage1lowmix.getStat(), inlinechildren)(stage1lowmix.getDyn())
    //((D2(k) compose F_2()) tensor I(n/4))
    //compose T3L(n/2,2,k)


    val d2mix: Mix = {
      val s2_gather: IMH = IMH(toOE(0), nquart, toOE(1))
      val s2_scatter: IMH = s2_gather
      val nim = GT_IM(s2_scatter, s2_gather)
      mix.copy(x = datalowhalf1, y = stage1_target, n = toOE(2), lb = nquart, im = nim, tw = None, scalars = idata.scalars)
    }


     val sn: Int = mix.n.a match{
       case x: Int => x
       case _ => assert(false); 1
     }


     //T3L(n/2,2,k)
     val diag  = MathUtilities.diagTensor(MathUtilities.dLin((sn/2)/2,2,0), MathUtilities.dLin(2,2,1))
     val tx = E(sn)
     val clist_re = diag map ( ele => tx.re(ele.toInt))
     val clist_im = diag map ( ele => tx.im(ele.toInt))

     val root_list_re = clist_re.grouped(2).toList.transpose.flatten //this is the L(n/2,k) part
     val root_list_im = clist_im.grouped(2).toList.transpose.flatten

     val t3l = d2mix.x.create(sn/2)
     val sample = d2mix.x(toOE(0))
     val t3data = (0 until sn/2).foldLeft(t3l){
       (acc,ele) =>  acc.update(ele,sample.create(unit(root_list_re(ele)),unit(root_list_im(ele))))
     }



    val directresultlow = loop(d2mix, idata.in, stage1_target, None, { idata => {
       val t01 = idata.in.apply(resolveH(mix.im.gather(), toOE(0), idata.i))
       val t02 = idata.in.apply(resolveH(mix.im.gather(), toOE(1), idata.i))

       val t3idx0 = resolveH(IMH(toOE(0), nquart, toOE(1)), toOE(0), idata.i)
       val t3idx1 = resolveH(IMH(toOE(0), nquart, toOE(1)), toOE(0), idata.i)

       val scale0 = t3data(t3idx0)
       val scale1 = t3data(t3idx1)


       val (t1, t2): (DataEle, DataEle) = d2mix.im match {
         case im_git: GT_IM => (t01, t02)
       }

       val t = E(4)
       val re = t.re(1)
       val im = t.im(1)
       val ele = t1.create(unit(re),unit(im))
       val idx0 = resolveH(d2mix.im.scatter(), toOE(0), idata.i)
       val val1 = idata.out.update(idx0 + nhalf, (t1 + t2)  * scale0)
       val idx1 = resolveH(d2mix.im.scatter(), toOE(1), idata.i)
       val val2 = val1.update(idx1 + nhalf, (t1 - t2) * ele * scale1)

       val2
     } })*/
     //scatter high half

     //perform dft(n/2)


     val highhalfmix1: Mix = {
       val s2_gather: IMH = IMH(toOE(0), toOE(1), toOE(0))
       val s2_scatter: IMH = IMH(toOE(0), toOE(1), toOE(0)+nhalf)
       val nim = GT_IM(s2_scatter, s2_gather)
       mix.copy(x = highhalf_input, y = stage1_target, n = nhalf, lb = toOE(1), im = nim, tw = None, scalars = idata.scalars)
     }
     val directresultlow = DFT(highhalfmix1.getStat(), inlinechildren)(highhalfmix1.getDyn())


     val highhalfmix: Mix = {
       val s2_gather: IMH = IMH(toOE(0), toOE(1), toOE(0))
       val s2_scatter: IMH = IMH(toOE(0), toOE(1), toOE(0))
       val nim = GT_IM(s2_scatter, s2_gather)
       mix.copy(x = directresultlow, y = stage1_target, n = nhalf, lb = toOE(1), im = nim, tw = None, scalars = idata.scalars)
     }
     val directresulthigh = DFT(highhalfmix.getStat(), inlinechildren)(highhalfmix.getDyn())

    val stage2mix: Mix = {
     val s2_gather: IMH = IMH(toOE(0), toOE(1), toOE(2))
     val s2_scatter: IMH = fuseIM(mix.im.scatter(), s2_gather, idata.i)
     val nim = GT_IM(s2_scatter, s2_gather)
      mix.copy(x = directresulthigh, y = idata.out, n = toOE(2), lb = nhalf, im = nim, tw = None, scalars = idata.scalars)
      //mix.copy(x = idata.in, y = idata.out, n = toOE(2), lb = nhalf, im = nim, tw = None, scalars = idata.scalars)
    }
    val finalres = DFT(stage2mix.getStat(), inlinechildren)(stage2mix.getDyn())
     finalres

   }
   })
  }
  if (inline) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT_SR" + stat.toSig()), Some("DFT_SR" + stat.toName()))(expose, stat.expdata))
 }


 def DFT_CT(stat: Stat, inline: Boolean): MaybeSFunction = {
  val expose = exposeDyn(stat)
  val stageme: (Dyn => Data) = (dyn: Dyn) => {
   val mix_b = Mix(stat, dyn)
   val (mix, parx): (Mix, Option[Int]) = mix_b.par.fold[(Mix, Option[Int])]((mix_b, None))(p => mix_b.lb.a match {
    case x: Int => if (x < 2) (mix_b, None) else (mix_b.copy(par = None), Some(p))
    case _ => (mix_b.copy(par = None), Some(p))
   })
   val k = chooseRadix(mix.n)
   val m = mix.n / k

   val inlinechildren = inlinec(mix.getStat().getn())
   loop(mix, mix.x, mix.y, parx, { idata => {
    val stage1mix: Mix = {
     val (s0, s1) = (k, toOE(1))
     val inner = IMH(toOE(0), s0, s1)
     val s1_gather: IMH = fuseIM(mix.im.gather(), inner, idata.i)
     val s1_scatter: IMH = if (inplace && !idata.scalars) {
      fuseIM(mix.im.scatter(), IMH(toOE(0), toOE(1), m), idata.i)
     } else IMH(toOE(0), toOE(1), m)
     val nim = mix.im match {
      case gt: GT_IM => GT_IM(s1_gather, s1_scatter)
      case gti: GTI_IM => GTT_IM(s1_gather, s1_scatter, fuseIM(gti.twim, inner, idata.i))
      case gtt: GTT_IM => GTT_IM(s1_gather, s1_scatter, fuseIM(gtt.twim, inner, idata.i))
     }
     val stage1_target: Data = {
      if (idata.scalars) {
       mix.n.ev.fold[Int, ScalarVector](mix.n.a, fa => {
        ???
       }, fb => {
        ScalarVector(new Array[Exp[Double]](fb * 2))
       })
      } else {
       if (inplace) idata.out else mix.y.create(mix.n)
      }
     }
     mix.copy(x = idata.in, y = stage1_target, n = m, lb = if (inline) k else R2AInt(k.ev.toRep(k.a)), im = nim, scalars = idata.scalars)
    }
    val dataafterS1 = DFT(stage1mix.getStat(), inlinechildren)(stage1mix.getDyn())
    val stage2mix: Mix = {
     val twid = TwiddleScaling(mix.n, m, toOE(1))
     val s2_gather: IMH = IMH(toOE(0), m, toOE(1))
     val s2_scatter: IMH = fuseIM(mix.im.scatter(), s2_gather, idata.i)
     val nim = if (inplace && !idata.scalars) GTI_IM(s2_scatter, s2_gather) else GTT_IM(s2_gather, s2_scatter, s2_gather)
     mix.copy(x = dataafterS1, y = idata.out, n = k, lb = if (inline) m else R2AInt(m.ev.toRep(m.a)), im = nim, tw = Some(twid), scalars = idata.scalars)
    }
    val finalres = DFT(stage2mix.getStat(), inlinechildren)(stage2mix.getDyn())
     finalres
   }
   })
  }
  if (inline) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT_CT" + stat.toSig()), Some("DFT_CT" + stat.toName()))(expose, stat.expdata))
 }

 def binsearch2pow(mix: Mix, check: Rep[Int], low: Int, high: Int): Data = {
  implicit val expose = mix.expdata
  ifThenElse(ordering_equiv(check, Const(high)), {
   val nmix = mix.copy(n = toOE(high))
   DFT(nmix.getStat(), inlinec(mix.getStat().getn())).mkfun(nmix.getStat(), nmix.getDyn())
  }, {
   if (high == 2) {
    val nmix = mix.copy(n = toOE(high))
    DFT(nmix.getStat(), inlinec(mix.getStat().getn())).mkfun(nmix.getStat(), nmix.getDyn())
   }
   else
    binsearch2pow(mix, check, low, high / 2)
  })
 }

 def DFT(stat: Stat, inline: Boolean): MaybeSFunction = {
  val expose = exposeDyn(stat)
  val stageme: (Dyn => Data) = (dyn: Dyn) => {
   val mix = Mix(stat, dyn)
   implicit val exposedata = mix.expdata
   if (basecase_size.isDefined && mix.n.ev.isRep()) {
    val isbasecase = mix.n.ev.less(mix.n.a, mix.n.ev.const(basecase_size.get + 1))
    mix.n.ev._if(isbasecase,
     mix.n.ev._if(mix.n.ev.equiv(mix.n.a, mix.n.ev.const(2)),
      F2(stat, inlinec(mix.getStat().getn()))(dyn),
      binsearch2pow(mix, mix.n.ev.toRep(mix.n.a), 2, basecase_size.get)),
     DFT_CT(stat, inlinec(mix.getStat().getn()))(dyn))
   } else {
    mix.n.ev._if(mix.n.ev.equiv(mix.n.a, mix.n.ev.const(2)), F2(stat, inlinec(mix.getStat().getn()))(dyn),
      mix.n.ev._if(mix.n.ev.equiv(mix.n.a, mix.n.ev.const(4)),
        DFT_CT(stat, inlinec(mix.getStat().getn()))(dyn),
        DFT_SplitRadix(stat, inlinec(mix.getStat().getn()))(dyn)))
   }
  }
  if (inline) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("DFT" + stat.toSig()), Some("DFT" + stat.toName()))(expose, stat.expdata))
 }

 def ini(stat: Stat): (Dyn => Data) = (dyn: Dyn) => {
  DFT(stat, true)(dyn)
 }
}


