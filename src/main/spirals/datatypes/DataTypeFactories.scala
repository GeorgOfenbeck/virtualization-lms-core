/**
 * _______  _______  ___   __      ____     Automatic
 * / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 * _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 * /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 * of DSP Algorithms
 * https://bitbucket.org/GeorgOfenbeck/spirals
 * SpiralS 0.1 Prototype - ETH Zurich
 * Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
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

package ch.ethz.spirals.datatypes

object DataTypeFactories {
  import UnstagedImplicitOps._
  import ElementOpsUnstaged._
  class SplitComplexArray[V[_], A[_], R[_], T](s: V[Int], d: V[A[T]] = null)
                                                              (implicit
                                                               // Template operators
                                                               aops: ArrayOps[V, A, R, T],
                                                               vrep: LiftOps[V],
                                                               erep: ElementOps[Complex, R[T]],
                                                               nrep: NumericOps[R[T]],
                                                               irep: NumericOps[V[Int]]
                                                                ) extends CVector[V, Complex, R,T] {


    private val two = irep.fromInt(2)
    private val data = if (d == null) aops.alloc(irep.times(s, two)) else d

    def create(n: V[Int]) = {
      val arr: V[A[T]] = aops.alloc(irep.times(n, two))
      new SplitComplexArray[V, A, R, T](n, arr)
    }
    def apply(i: V[Int]): Complex[R[T]] = new Complex(
      _re = aops.apply(data, i),
      _im = aops.apply(data, irep.plus(s, i))
    )

    // (nrep.m)
    def update(i: V[Int], y: Complex[R[T]]) = {
      aops.update(data, i, y._re)
      aops.update(data, irep.plus(s, i), y._im)
    }

    def GT(A: CVector[V, Complex, R, T] => CVector[V, Complex, R, T],
           g: (Vector[V[Int]]) => V[Int],
           s: (Vector[V[Int]]) => V[Int],
           v: Vector[Int]
            )
    : CVector[V, Complex, R, T] => CVector[V, Complex, R, T] = (in: CVector[V, Complex, R, T]) => {

      val out = in.create(in.size()) //create a same size element


      def helper(loopv: Vector[Int], currv: Vector[V[Int]]): Unit = {
        if (loopv.tail.isEmpty){ //inner most loop
          val s0 = loopv.head
          val int = in.create(irep.fromInt(s0))
            for ( i <- 0 until s0)
            {
              val is: V[Int] = irep.fromInt(i)
              val newv: Vector[V[Int]] = is +: currv
              val idx: V[Int] = g(newv)
              val ele = in.apply(idx)
              int.update(is,ele)
            }
            val outt = A(int)
            for ( i <- 0 until s0)
            {
              val is: V[Int] = irep.fromInt(i)
              val newv: Vector[V[Int]] = is +: currv
              val idx: V[Int] = s(newv)
              val ele = outt.apply(is)
              out.update(idx,ele)
            }
          }
        else
        {
          val sk = loopv.head
          val rest = loopv.tail
          for (i <- 0 until sk) {
            val is: V[Int] = irep.fromInt(i)
            helper(rest, is +: currv)
          }
        }
      }

      helper(v,Vector.empty)
      out
    }





    def size() = s

    override def toString = "SplitComplexVector()"
  }

}



