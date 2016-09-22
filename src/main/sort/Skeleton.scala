package sort


import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._


trait Skeleton extends Sort_DSL {
  type NoRep[T] = T


  trait IRep[T[_]] {
    def isRep(): Boolean
    def getRep[A](x: T[A]): Option[Rep[A]]
    def getNoRep[A](x: T[A]): Option[A]
    def fresh[A: TypeRep](): Vector[Rep[_]]
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Option[T[A]])

    def convert[A[_],X: TypeRep](me: T[X], that: A[X])(implicit evthat: IRep[A]): (A[X],A[X])
    def convertc[X: TypeRep](me: T[X], that: NoRep[X]): (T[X],T[X])
    def convertc[X: TypeRep](me: T[X], that: Rep[X]): (T[X],T[X])
  }

  implicit object isRep extends IRep[Rep] {
    val isRep = true
    def getRep[A](x: Rep[A]): Some[Rep[A]] = Some(x)
    def getNoRep[A](x: Rep[A]): Option[A] = None
    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector(Arg[A])
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Some[Rep[A]]) = (x.tail,Some(x.head.asInstanceOf[Rep[A]]))

    def convert[A[_],X: TypeRep](me: Rep[X], that: A[X])(implicit evthat: IRep[A]): (A[X],A[X]) = evthat.convertc[X](that,me)
    def convertc[X: TypeRep](me: Rep[X], that: NoRep[X]): (Rep[X],Rep[X]) = (me, Const(that))
    def convertc[X: TypeRep](me: Rep[X], that: Rep[X]): (Rep[X],Rep[X]) = (me, that)


    //def push[A: TypeRep](x: Rep[A]): Vector[Rep[_]] = Vector(x)
  }
  implicit object noRep extends IRep[NoRep] {
    val isRep = false
    def getRep[A](x: NoRep[A]): Option[Rep[A]] = None
    def getNoRep[A](x: NoRep[A]): Some[A] = Some(x)
    def fresh[A: TypeRep](): Vector[Rep[_]] = Vector.empty
    def fetch[A: TypeRep](x: Vector[Rep[_]]): (Vector[Rep[_]],Option[NoRep[A]]) = (x,None)
    def convert[A[_],X: TypeRep](me: NoRep[X], that: A[X])(implicit evthat: IRep[A]): (A[X],A[X]) = evthat.convertc[X](that,me)
    def convertc[X: TypeRep](me: NoRep[X], that: NoRep[X]): (NoRep[X],NoRep[X]) = (me, that)
    def convertc[X: TypeRep](me: NoRep[X], that: Rep[X]): (Rep[X],Rep[X]) = (Const(me), that)
  }


}

