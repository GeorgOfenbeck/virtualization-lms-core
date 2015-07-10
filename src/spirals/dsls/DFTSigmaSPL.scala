/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */

package ch.ethz.spirals.dsls


import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

trait DFTSigmaSPL_Base extends SigmaSPLBase with LiftNumeric {

 /*def infix_SPL_RaderDiag(n: Rep[Int], k: Rep[Int], root: Rep[Int], in : Rep[SigmaSPLVector]) : Rep[SigmaSPLVector]
 def infix_SPL_S_T(n: Rep[Int], d: Rep[Int], k: Rep[Int], f: Rep[IndexMapping], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_S_T3L(n: Rep[Int], d: Rep[Int], k: Rep[Int], f: Rep[IndexMapping], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_T(n: Rep[Int], d: Rep[Int], k: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_T3L(n: Rep[Int], d: Rep[Int], k: Rep[Int], in: Rep[SigmaSPLVector]) : Rep[SigmaSPLVector]
 def infix_SPL_I(n: Rep[Int], in: Rep[SigmaSPLVector]) : Rep[SigmaSPLVector]
 def infix_SPL_F2(in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_D2(k: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_RaderMid(n: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 //--------------- Permutations
 def infix_SPL_L (n: Rep[Int], d: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_V (r: Rep[Int], s: Rep[Int], alpha : Rep[Int], beta: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_Vt(r: Rep[Int], s: Rep[Int], alpha : Rep[Int], beta: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_W (n: Rep[Int], phi : Rep[Int], g: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_SPL_Wt(n: Rep[Int], phi : Rep[Int], g: Rep[Int], in: Rep[SigmaSPLVector]): Rep[SigmaSPLVector]
 def infix_im_twiddle(d: Rep[Int], n: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
 def infix_im_w(g: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
 def infix_im_wt(phi: Rep[Int], g: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
 def infix_im_v(m: Rep[Int], k: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]
 def infix_im_z(b: Rep[Int], s: Rep[Int], range: Rep[Int], domain: Rep[Int]): Rep[IndexMapping]*/

  def infix_SPL_F2(): Rep[SigmaSPLVector]
}


trait DFTSigmaSPLExp extends DFTSigmaSPL_Base with SigmaSPLExp {
  import scala.reflect.runtime.universe._
  def bla[T]()(implicit tag: TypeTag[T]): TypeTag[T]= tag //RF!

  case class SPL_F2() extends Def[SigmaSPLVector]
  def infix_SPL_F2(): Exp[SigmaSPLVector] = DefwithMeta(SPL_F2(),new SigmaSPLMeta(2,bla[SigmaSPLVector]()))
  /*
 def numeric_times(lhs: Exp[Int], rhs: Exp[Int]): Exp[Int] = super.numeric_times(lhs,rhs)

 case class IM_Twiddle (d: Exp[Int], n: Exp[Int], override val range: Exp[Int], override val domain: Exp[Int]) extends DefIM(range, domain)
 def infix_im_twiddle(d: Exp[Int], n: Exp[Int], range: Exp[Int], domain: Exp[Int]) : Exp[IndexMapping] = IM_Twiddle(d,n,range, domain)

 case class IM_W (g: Exp[Int], override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
 def infix_im_w(g: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_W(g,range, domain)

 case class IM_WT (phi: Exp[Int], g: Exp[Int], override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
 def infix_im_wt(phi: Exp[Int], g: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_WT(phi, g,range, domain)

 case class IM_V (m: Exp[Int], k: Exp[Int], override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
 def infix_im_v(m: Exp[Int], k: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_V(m,k,range,domain)

 case class IM_Z (b: Exp[Int], s: Exp[Int], override val range: Exp[Int], override val domain: Exp[Int])       extends DefIM(range, domain)
 def infix_im_z(b: Exp[Int], s: Exp[Int], range: Exp[Int], domain: Exp[Int]): Exp[IndexMapping] = IM_Z(b, s,range, domain)




 case class SPL_F2(inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_D2(k: Exp[Int],inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_RaderMid(n: Exp[Int], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]

 case class SPL_S_T(n: Exp[Int], d: Exp[Int], k: Exp[Int],f: Exp[IndexMapping], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_S_T3L(n: Exp[Int], d: Exp[Int], k: Exp[Int],f: Exp[IndexMapping], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_S_RaderDiag(n: Exp[Int], k: Exp[Int], root: Exp[Int],f: Exp[IndexMapping], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_T(n: Exp[Int], d: Exp[Int], k: Exp[Int], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_T3L(n: Exp[Int], d: Exp[Int], k: Exp[Int], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_RaderDiag(n: Exp[Int], k: Exp[Int], root: Exp[Int], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_I(n: Exp[Int],inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]

 case class SPL_L  (n: Exp[Int], d: Exp[Int], inr: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_V  (r: Exp[Int], s: Exp[Int], alpha : Exp[Int], beta: Exp[Int], in: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_Vt (r: Exp[Int], s: Exp[Int], alpha : Exp[Int], beta: Exp[Int], in: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_W  (n: Exp[Int], phi : Exp[Int], g: Exp[Int], in: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]
 case class SPL_Wt (n: Exp[Int], phi : Exp[Int], g: Exp[Int], in: Exp[SigmaSPLVector]) extends Def[SigmaSPLVector]



 def infix_SPL_I(n: Exp[Int],inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_I(n,inr)

 def infix_SPL_S_RaderDiag(n: Exp[Int], k: Exp[Int], root: Exp[Int],f: Exp[IndexMapping], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_S_RaderDiag(n,k,root,f,inr)

 def infix_SPL_S_T3L(n: Exp[Int], d: Exp[Int], k: Exp[Int],f: Exp[IndexMapping], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_S_T3L(n,d,k,f,inr)

 def infix_SPL_S_T(n: Exp[Int], d: Exp[Int], k: Exp[Int],f: Exp[IndexMapping], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_S_T(n,d,k,f,inr)

 def infix_SPL_T(n: Exp[Int], d: Exp[Int], k: Exp[Int], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_T(n,d,k,inr)

 def infix_SPL_T3L(n: Exp[Int], d: Exp[Int], k: Exp[Int], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_T3L(n,d,k,inr)

 def infix_SPL_RaderDiag(n: Exp[Int], k: Exp[Int], root: Exp[Int], inr : Exp[SigmaSPLVector]) : Exp[SigmaSPLVector] = SPL_RaderDiag(n,k,root,inr)

 def infix_SPL_RaderMid(n: Exp[Int],inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_RaderMid(n,inr)

 def infix_SPL_F2(inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_F2(inr)

 def infix_SPL_D2(k: Exp[Int],inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_D2(k,inr)

 def infix_SPL_L(n: Exp[Int], d: Exp[Int], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector] = SPL_L(n,d,inr)

 def infix_SPL_V(r: Exp[Int], s: Exp[Int], alpha : Exp[Int], beta: Exp[Int], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector]= SPL_V(r,s,alpha,beta,inr)

 def infix_SPL_Vt(r: Exp[Int], s: Exp[Int], alpha : Exp[Int], beta: Exp[Int], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector]= SPL_Vt(r,s,alpha,beta,inr)

 def infix_SPL_W(n: Exp[Int], phi : Exp[Int], g: Exp[Int], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector]= SPL_W(n,phi,g,inr)

 def infix_SPL_Wt(n: Exp[Int], phi : Exp[Int], g: Exp[Int], inr: Exp[SigmaSPLVector]): Exp[SigmaSPLVector]= SPL_Wt(n,phi,g,inr)

  */
 /*  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
     case SPL_W(n,phi,g,inr)               => infix_SPL_W(f(n),f(phi),f(g),f(inr))
     case SPL_Wt(n,phi,g,inr)              => infix_SPL_Wt(f(n),f(phi),f(g),f(inr))
     case SPL_V(r,s,alpha,beta,inr)        => infix_SPL_V(f(r),f(s),f(alpha),f(beta),f(inr))
     case SPL_Vt(r,s,alpha,beta,inr)       => infix_SPL_Vt(f(r),f(s),f(alpha),f(beta),f(inr))
     case SPL_RaderDiag(n,k,root,inr)      => infix_SPL_RaderDiag(f(n),f(k),f(root),f(inr))
     case SPL_I(n,inr)                     => infix_SPL_I(f(n),f(inr))
     case SPL_S_RaderDiag(n,k,root,im,inr) => infix_SPL_S_RaderDiag(f(n),f(k),f(root),f(im),f(inr))
     case SPL_S_T(n,d,k,im, inr)           => infix_SPL_S_T(f(n), f(d),f(k),f(im),f(inr))
     case SPL_S_T3L(n,d,k,im, inr)         => infix_SPL_S_T3L(f(n), f(d),f(k),f(im),f(inr))
     case SPL_T(n,d,k, inr)                => infix_SPL_T(f(n), f(d),f(k),f(inr))
     case SPL_T3L(n,d,k, inr)              => infix_SPL_T3L(f(n), f(d),f(k),f(inr))
     case SPL_L(n,d,inr)                   => infix_SPL_L(f(n), f(d),f(inr))
     case SPL_F2(inr)                      => infix_SPL_F2(f(inr))
     case SPL_D2(inr,k)                    => infix_SPL_D2(f(inr),f(k))
     case SPL_RaderMid(n,inr)              => infix_SPL_RaderMid(f(n),f(inr))
     case IM_Twiddle(d, n,  range, domain) => infix_im_twiddle(f(d), f(n),  f(range), f(domain))
     case IM_W(g,range, domain)            => infix_im_w(f(g),f(range), f(domain))
     case IM_WT(phi,g, range, domain)      => infix_im_wt(f(phi),f(g), f(range), f(domain))
     case IM_V(m,k, range, domain)         => infix_im_v(f(m),f(k), f(range), f(domain))
     case IM_Z(m,k, range, domain)         => infix_im_z(f(m),f(k), f(range), f(domain))
     case _ => super.mirror(e,f)
   }).asInstanceOf[Exp[A]] //2DO: tiark's type case - get rid off*/

}

class DFTSigmaSPL extends DFTSigmaSPL_Base with DFTSigmaSPLExp
