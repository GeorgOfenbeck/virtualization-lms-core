/*

/**
 * _______  _______  ___   __      ____     Automatic
 * / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 * _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 * /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 * of DSP Algorithms
 * https://bitbucket.org/GeorgOfenbeck/spirals
 * SpiralS 0.1 Prototype - ETH Zurich
 * Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/.
 */
package ch.ethz.spirals.rewrites

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import ch.ethz.spirals.dsls._


trait SPL2SigmaSPL extends PureDefaultTraversal {

  val IR: SPL_Exp with PureFunctionsExp
  import IR._


  trait EmitSigmaSPL extends Schedule {
    val traversal: Traversal {
      val cminfo: CodeMotion {
        val reifiedIR: ReificationPure {
          val IR: SPL_Exp with PureFunctionsExp}}}
    val sigmaspl: DFTSigmaSPL_Base

    def emitNode(tp: traversal.cminfo.reifiedIR.IR.TP[_], fmap: Map[Int, sigmaspl.Rep[sigmaspl.SigmaSPLVector] => sigmaspl.Rep[sigmaspl.SigmaSPLVector]], block_callback: traversal.cminfo.reifiedIR.IR.Block => Iterator[traversal.cminfo.reifiedIR.IR.TP[_]]):
    Map[Int, sigmaspl.Rep[sigmaspl.SigmaSPLVector] => sigmaspl.Rep[sigmaspl.SigmaSPLVector]] = {
      import traversal.cminfo.reifiedIR.IR._



      val newf = tp.rhs match {

        case Compose(a, b) => (in: sigmaspl.Rep[sigmaspl.SigmaSPLVector]) => fmap(a.id)(fmap(b.id)(in))
        case Tensor(Const(I(k: Int)), b) => (in: sigmaspl.Rep[sigmaspl.SigmaSPLVector]) => {
          val bmeta = getSPLMeta(b)
          val n = bmeta.size
          val tensorsize = n * k
          import sigmaspl._

          val h = imh_h(n,0,Vector(1,n),n,tensorsize)
          val A = fmap(b.id)
          gt(in,A(in),h,h,Vector(k))

          /*val fragsize: sigmaspl.Rep[Int] = tensorsize / n
          val loop: sigmaspl.Rep[sigmaspl.SigmaSPLVector] = sigmaspl.sigma(tensorsize, 0, n,
            i => {
              val gattered = gather(im_h(fragsize * i, 1, fragsize, tensorsize), in)
              val processed = fmap(b.id)(gattered)
              val scattered = scatter(im_h(fragsize * i, 1, fragsize, tensorsize), processed)
              scattered
            })
          loop*/
        }
        case Tensor(a, Const(I(k))) => (in: sigmaspl.Rep[sigmaspl.SigmaSPLVector]) => {
          val ameta = getSPLMeta(a)
          val n = ameta.size
          val tensorsize = n * k

          import sigmaspl._

          val h = imh_h(n,0,Vector(k,1),n,tensorsize)
          val A = fmap(a.id)
          gt(in,A(in),h,h,Vector(k))

        }
        case ConstDef(x: SPL) => (in: sigmaspl.Rep[sigmaspl.SigmaSPLVector]) => emitSigmaSPLOperator(x, in)
      }
      fmap + (tp.exp.id -> newf)
    }

    def emitSigmaSPLOperator(spl: SPL, in: sigmaspl.Rep[sigmaspl.SigmaSPLVector]): sigmaspl.Rep[sigmaspl.SigmaSPLVector] = {
      import ch.ethz.spirals.dsls._
      import sigmaspl._
      val out: sigmaspl.Rep[sigmaspl.SigmaSPLVector] = spl match {
        case I(n) => sigmaspl.infix_SPL_F2()
        case F_2() => sigmaspl.infix_SPL_F2()
        /*case L(n,k) => sigmaspl.infix_SPL_L(n,k,in)
        case W(n,phi,g) => sigmaspl.infix_SPL_W(n,phi,g,in)
        case Wt(n,phi,g) => sigmaspl.infix_SPL_Wt(n,phi,g,in)
        case V(r,s,alpha,beta) => sigmaspl.infix_SPL_V(r,s,alpha,beta,in)
        case Vt(r,s,alpha,beta) => sigmaspl.infix_SPL_Vt(r,s,alpha,beta,in)
        case Rader_Mid_Matrix(n) => sigmaspl.infix_SPL_RaderMid(n,in)
        case D2(k) => sigmaspl.infix_SPL_D2(k,in)
        case T(n,d,k) => sigmaspl.infix_SPL_T(n,d,k,in)
        case T3L(n,d,k) => sigmaspl.infix_SPL_T3L(n,d,k,in)
        case Rader_Diag(n,k,root) => sigmaspl.infix_SPL_RaderDiag(n,k,root,in)*/
      }
      out
    }
  }

  def SPL2SigmaSPL(splgenf: => IR.Exp[SPL], dsl_sigmaspl: DFTSigmaSPL_Base) = {
    val myf: IR.Exp[Unit] => IR.Exp[SPL] = (u: IR.Exp[Unit]) => splgenf
    val expos_u = IR.exposeRepFromRep[Unit]
    val expos_spl = IR.exposeRepFromRep[SPL]
    val emit = new EmitSigmaSPL {
      override val traversal = default_traversal(myf)(expos_u, expos_spl)
      override val sigmaspl: dsl_sigmaspl.type = dsl_sigmaspl
    }

    val col = new emit.MyCollection
    def tmp(b: emit.traversal.cminfo.reifiedIR.IR.Block): Iterator[emit.traversal.cminfo.reifiedIR.IR.TP[_]] = {
      val x = col.iterator(b)
      x
    }

    val bydecompmap = col.iterator().foldLeft(Map.empty[Int, dsl_sigmaspl.Rep[dsl_sigmaspl.SigmaSPLVector] => dsl_sigmaspl.Rep[dsl_sigmaspl.SigmaSPLVector]]) {
      (acc, ele) => {
        emit.emitNode(ele, acc, tmp)
      }
    }
    val finalnode = bydecompmap.map(x => x._1).reduceLeft(_ max _)
    val finalcall = bydecompmap(finalnode)
    finalcall

  }
}
*/
