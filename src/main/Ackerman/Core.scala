package Ackerman

import Ackerman.Twiddle.MathUtilities

import scala.lms.targets.graphviz.{GraphVizCallGraph, GraphVizExport}
import scala.lms.targets.scalalike._

class Core(n: Int, m: Int) extends Header {
  self =>
  val emitGraph = new GraphVizCallGraph {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadNoTuples with ScalaGenPrimitivOps with ScalaGenSpiral_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse with ScalaGenOrderingOps {
    val IR: self.type = self
  }


  def ackf(stat: Stat, inline: Boolean): MaybeSFunction = {
    val expose = exposeDyn(stat)
    val stageme: (Dyn => Rep[Int]) = (dyn: Dyn) => {
      val mix = Mix(stat, dyn)


      val r: Rep[Int] = mix.m.ev._if(mix.m.ev.equiv(mix.m.a, mix.m.ev.const(0)), {
        val t: Rep[Int] = mix.n.ev.toRep(mix.n.ev.plus(mix.n.a, mix.n.ev.const(1)))
        t
      }
        ,
        mix.n.ev._if(mix.n.ev.equiv(mix.n.a, mix.n.ev.const(0)), {
          val nmix = mix.copy(n = toOE(1), mix.m - toOE(1))
          val t: Rep[Int] = ackf(nmix.getStat(), inline)(nmix.getDyn())
          t
        }, {

          val fmix = mix.copy(n = mix.n - 1)
          val f: Rep[Int] = ackf(fmix.getStat(), inline)(fmix.getDyn())
          val nmix = mix.copy(n = R2AInt(f), mix.m - toOE(1))
          val t: Rep[Int] = ackf(nmix.getStat(), inline)(nmix.getDyn())
          t
        }
        )
      )
      r
    }


    if (inline) MaybeSFunction(stageme) else MaybeSFunction(doGlobalLambda(stageme, Some("Ackerman" + stat.toSig()), Some("Ackerman" + stat.toName()))(expose, exposeRepFromRep[Int]))

  }


  def ini(stat: Stat): (Dyn => Rep[Int]) = (dyn: Dyn) => {
    ackf(stat, false)(dyn)
  }


}


