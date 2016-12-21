package SpiralS2

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


class Core extends Header {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenOrderingOps {
    val IR: self.type = self
  }

  val WHT = false
  val inplace = true

  def resolveH(h: IMH, i: AInt): AInt = ???
  def resolveTwid(n: AInt, d: AInt, k: AInt, i: AInt): DataEle = ???
  def chooseRadix(n: AInt): AInt = ???
  def sum[A](till: AInt, ini: Data, body: Data => Data): Data = ???

  def F2(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      val t01 = dyn.x.apply(resolveH(mix.im.gather(), toOE(0)))
      val t02 = dyn.x.apply(resolveH(mix.im.gather(), toOE(1)))
      val (t1, t2): (DataEle, DataEle) = mix.im match {
        case im_git: GT_IM => (t01, t02)
        case im_gti: GTI_IM => ((resolveTwid(mix.tw.n, mix.tw.d, mix.tw.k, resolveH(im_gti.twim, toOE(0))) * t01),
          (resolveTwid(mix.tw.n, mix.tw.d, mix.tw.k, resolveH(im_gti.twim, toOE(1))) * t02))
      }
      mix.y.update(resolveH(mix.im.scatter(), toOE(0)), (t1 + t2)).update(resolveH(mix.im.scatter(), toOE(1)), (t1 - t2))
    }
    val t: StagedFunction[Dyn, Data] = doGlobalLambda(stageme, Some("F2" + stat.toSig()), Some("F2" + stat.toSig()))(expose, exposeData)
    MaybeSFunction(t)
  }

  def DFT_CT(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      val m = chooseRadix(mix.n)
      val k = mix.n / m
      sum(mix.lb,mix.y, { input => {
        val stage1mix: Mix = {
          val s1_gather = {
            IMH(toOE(0),if (WHT) {
              //val t1 = ivecappend(t0, k.toRep())
              //ivecappend(t1, Const(1))
              ???} else {
              //val t1 = ivecappend(t0, Const(1))
              //ivecappend(t1, m.toRep())
              ???
            })
          }
          val s1_scatter: IMH = ???
          val nim = GT_IM(s1_gather, s1_scatter)
          val stage1_target = if (inplace) mix.y else mix.y.create(mix.n)
          mix.copy(y = stage1_target, n = m, lb = k, im = nim, v = ???)
        }
        val dataafterS1 = DFT(stage1mix.getStat())(stage1mix.getDyn())
        val stage2mix: Mix = {
          val s1_gather = {
            IMH(toOE(0),if (WHT) {
              //val t1 = ivecappend(t0, k.toRep())
              //ivecappend(t1, Const(1))
              ???} else {
              //val t1 = ivecappend(t0, Const(1))
              //ivecappend(t1, m.toRep())
              ???
            })
          }
          val s1_scatter: IMH = ???
          val nim = GT_IM(s1_gather, s1_scatter)
          val stage1_target = if (inplace) mix.y else mix.y.create(mix.n)
          mix.copy(y = stage1_target, n = m, lb = k, im = nim, v = ???)
        }
        DFT(stage2mix.getStat())(stage2mix.getDyn())
      }})
    }
    val t: StagedFunction[Dyn, Data] = doGlobalLambda(stageme, Some("DFT_CT" + stat.toSig()), Some("F2" + stat.toSig()))(expose, exposeData)
    MaybeSFunction(t)
  }
  def DFT(stat: Stat): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Data) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)
      val bool: Exp[Boolean] = mix.n.ev.toRep(mix.n.a) <= Const(2)
      myifThenElse( bool, { F2(stat)(dyn)}, { DFT_CT(stat)(dyn)})
    }
    val t: StagedFunction[Dyn, Data] = doGlobalLambda(stageme, Some("DFT" + stat.toSig()), Some("DFT" + stat.toSig()))(expose, exposeData)
    MaybeSFunction(t)
  }


}
