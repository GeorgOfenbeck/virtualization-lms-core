/*
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
package spirals.rewrites

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.collection.immutable.HashMap


trait SPL2SigmaSPL extends SPL2Size with GraphTraversal {
  import ch.ethz.spirals.dsls._
  val IR: SPL_Exp
  import IR._



  def SPL2SigmaSPL (start: Exp[Any], sigmaspl: DFTSigmaSPL_Base, tag_startsize: Int): sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector] = {
    val x = for (TP(sym,rhs) <- buildScheduleForResult(start)) yield TP(sym,rhs)
    //2DO: Georg -> can we seal the DSL to check for missing matches?
    def emitSigmaSPL(sym: Sym[Any], rhs: Def[Any], fmap : Map[Int,sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]], tag_start_pos: List[Int]): sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]  =   rhs match {
      //--------------------------------Compose -----------------------------
      case Compose(a1,b1) =>
      {
        (a1,b1) match {
          case (Sym(a),Sym(b)) => (in: sigmaspl.Rep[sigmaspl.Vector]) => fmap(a)(fmap(b)(in))
          case (Sym(a),Const(b: SPL))=> (in: sigmaspl.Rep[sigmaspl.Vector]) => fmap(a)(emitSigmaSPLOperator(b,in))
          case (Const(a: SPL),Sym(b))=> (in: sigmaspl.Rep[sigmaspl.Vector]) => emitSigmaSPLOperator(a,fmap(b)(in))
          case (Const(a: SPL),Const(b: SPL))=> (in: sigmaspl.Rep[sigmaspl.Vector]) => emitSigmaSPLOperator(a,emitSigmaSPLOperator(b,in))
        }
      }
      //--------------------------------Tensor -----------------------------
      case Tensor(a1,b1) =>
      {
        val tensor_size = emitSize(sym)
        (a1,b1) match {
          case (Const(I(n)),Sym(b)) => {(in: sigmaspl.Rep[sigmaspl.Vector]) => {
            import sigmaspl._
            val frag_size: sigmaspl.Rep[Int] = tensor_size/n
            val loop = sigma(tensor_size,n,
              i => {
                val gattered =  if (tag_start_pos.contains(sym.id))
                  G(H(frag_size*i,1, tensor_size/n, tensor_size), sigmaspl.tag_end(in, Unroll()))
                else
                  G(H(frag_size*i,1, tensor_size/n, tensor_size), in)
                val processed = fmap(b)(gattered)
                val scattered = S(H(frag_size*i,1, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.contains(sym.id)) sigmaspl.tag_start(loop,Unroll()) else loop
          }}
          case (Const(I(n)),Const(b: SPL)) => {(in: sigmaspl.Rep[sigmaspl.Vector]) => {
            import sigmaspl._
            val frag_size: sigmaspl.Rep[Int] = tensor_size/n
            val loop = sigma(tensor_size,n,
              i => {
                val gattered = if (tag_start_pos.contains(sym.id))
                  G(H(frag_size*i,1, tensor_size/n, tensor_size), sigmaspl.tag_end(in, Unroll()))
                else
                  G(H(frag_size*i,1, tensor_size/n, tensor_size), in)
                val processed = emitSigmaSPLOperator(b,(gattered))
                val scattered = S(H(frag_size*i,1, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.contains(sym.id)) sigmaspl.tag_start(loop,Unroll()) else loop
          }}
          case (Sym(a),Const(I(n))) => {(in: sigmaspl.Rep[sigmaspl.Vector]) => {
            import sigmaspl._
            val loop = sigma(tensor_size,n,
              i => {
                val gattered =  if (tag_start_pos.contains(sym.id))
                  G(H(i,n,tensor_size/n, tensor_size), sigmaspl.tag_end(in, Unroll()))
                else
                  G(H(i,n,tensor_size/n, tensor_size), in)
                val processed = fmap(a)(gattered)
                val scattered = S(H(i,n, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.contains(sym.id)) sigmaspl.tag_start(loop,Unroll()) else loop
          }}
          case (Const(a: SPL),Const(I(n))) => {(in: sigmaspl.Rep[sigmaspl.Vector]) => {
            import sigmaspl._
            val loop = sigma(tensor_size,n,
              i => {
                val gattered = if (tag_start_pos.contains(sym.id))
                  G(H(i,n, tensor_size/n, tensor_size), sigmaspl.tag_end(in, Unroll()))
                else
                  G(H(i,n, tensor_size/n, tensor_size), in)
                val processed = emitSigmaSPLOperator(a,(gattered))
                val scattered = S(H(i,n, tensor_size/n, tensor_size),processed)
                scattered
              })
            if (tag_start_pos.contains(sym.id)) sigmaspl.tag_start(loop,Unroll()) else loop
          }}
        }
      }
      //--------------------------------DirectSum -----------------------------
      case DirectSum(a1,b1) =>
      {
        (a1,b1) match {
          case (Sym(a),Sym(b)) => {
            val asize = emitSize(a1)
            val bsize = emitSize(b1)
            val fa: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (ina: sigmaspl.Rep[sigmaspl.Vector]) => fmap(a)(ina)
            val fb: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (inb: sigmaspl.Rep[sigmaspl.Vector]) => fmap(b)(inb)
            directsum(fa, fb, asize, bsize)
          }
          case (Sym(a),Const(b: SPL))=> {
            val asize = emitSize(a1)
            val bsize = b.size
            val fa: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (ina: sigmaspl.Rep[sigmaspl.Vector]) => fmap(a)(ina)
            val fb: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (inb: sigmaspl.Rep[sigmaspl.Vector]) => emitSigmaSPLOperator(b,inb)
            directsum(fa, fb, asize, bsize)
          }
          case (Const(a: SPL),Sym(b))=> {
            val asize = a.size
            val bsize = emitSize(b1)
            val fa: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (ina: sigmaspl.Rep[sigmaspl.Vector]) => emitSigmaSPLOperator(a,ina)
            val fb: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (inb: sigmaspl.Rep[sigmaspl.Vector]) => fmap(b)(inb)
            directsum(fa, fb, asize, bsize)
          }
          case (Const(a: SPL),Const(b: SPL))=>
          {
            val asize = a.size
            val bsize = b.size
            val fa: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (ina: sigmaspl.Rep[sigmaspl.Vector]) => emitSigmaSPLOperator(a,ina)
            val fb: (sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]) = (inb: sigmaspl.Rep[sigmaspl.Vector]) => emitSigmaSPLOperator(b,inb)
            directsum(fa, fb, asize, bsize)
          }
        }
      }
    }

    def directsum(fa: sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector], fb: sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector], asize: Int, bsize: Int) = {
      val f : sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector] = (in: sigmaspl.Rep[sigmaspl.Vector]) => {
        val tsize = asize + bsize
        import sigmaspl._
        val repasize: sigmaspl.Rep[Int] = asize
        val repbsize: sigmaspl.Rep[Int] = bsize
        val resa = {
          val gattered = G(H(0,1, asize,tsize),in)
          val processed = fa(gattered)
          val scattered = S(H(0,1,asize,tsize),processed)
          scattered
        }
        val resb = {
          val gattered = G(H(repasize,1, bsize,tsize),in)
          val processed = fb(gattered)
          val scattered = S(H(repasize,1, bsize,tsize),processed)
          scattered
        }
        resa ++ resb
      }
      f
    }

    def emitSigmaSPLOperator(spl : SPL, in: sigmaspl.Rep[sigmaspl.Vector]): sigmaspl.Rep[sigmaspl.Vector] =  {
      import sigmaspl._
      val out: sigmaspl.Rep[sigmaspl.Vector] = spl match {
        case T(n,d,k) => sigmaspl.infix_SPL_T(n,d,k,in)
        case T3L(n,d,k) => sigmaspl.infix_SPL_T3L(n,d,k,in)
        case Rader_Diag(n,k,root) => sigmaspl.infix_SPL_RaderDiag(n,k,root,in)
        case I(n) => sigmaspl.infix_SPL_I(n,in)
        case L(n,k) => sigmaspl.infix_SPL_L(n,k,in)
        case W(n,phi,g) => sigmaspl.infix_SPL_W(n,phi,g,in)
        case Wt(n,phi,g) => sigmaspl.infix_SPL_Wt(n,phi,g,in)
        case V(r,s,alpha,beta) => sigmaspl.infix_SPL_V(r,s,alpha,beta,in)
        case Vt(r,s,alpha,beta) => sigmaspl.infix_SPL_Vt(r,s,alpha,beta,in)
        case Rader_Mid_Matrix(n) => sigmaspl.infix_SPL_RaderMid(n,in)
        case F_2() => sigmaspl.infix_SPL_F2(in)
        case D2(k) => sigmaspl.infix_SPL_D2(k,in)
      }
      out
    }

    def emittag_startPos(sym: Sym[Any], rhs: Def[Any],fmap : Map[Int,List[Int]]): (List[Int])  = rhs match
    {
      case Compose(a1,b1) =>
        (a1,b1) match {
          case (Sym(a),Sym(b)) =>  fmap(a) ++ fmap(b)
          case (Sym(a),Const(b: SPL))=>  fmap(a)
          case (Const(a: SPL),Sym(b))=>  fmap(b)
          case (Const(a: SPL),Const(b: SPL))=> List():List[Int]
        }
      case DirectSum(a1,b1) =>
        (a1,b1) match {
          case (Sym(a),Sym(b)) =>  fmap(a) ++ fmap(b)
          case (Sym(a),Const(b: SPL))=>  fmap(a)
          case (Const(a: SPL),Sym(b))=>  fmap(b)
          case (Const(a: SPL),Const(b: SPL))=> List():List[Int]
        }
      case Tensor(a1,b1) =>
      {
        val tensor_size = emitSize(sym)
        if (tensor_size <= tag_startsize)
          List(sym.id)
        else{
          (a1,b1) match {
            case (Const(I(n)),Sym(b)) => fmap(b)
            case (Const(I(n)),Const(b: SPL)) => List():List[Int]
            case (Sym(a),Const(I(n))) => fmap(a)
            case (Const(a: SPL),Const(I(n))) => List():List[Int]
          }
        }
      }
    }


    val tag_pos = x.foldLeft(Map.empty[Int,List[Int]]){
      (acc,ele) => acc + ((ele.sym.id -> emittag_startPos(ele.sym,ele.rhs,acc)))
    }

    val tags = tag_pos(x.last.sym.id)
    val translation = x.foldLeft(Map.empty[Int,sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector]]){
      (acc,ele) => acc + ((ele.sym.id -> emitSigmaSPL(ele.sym,ele.rhs,acc,tags)))
    }


    val last: sigmaspl.Rep[sigmaspl.Vector] => sigmaspl.Rep[sigmaspl.Vector] = translation(x.last.sym.id)
    last
  }
}*/
