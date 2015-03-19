package scala.virtualization.lms
package internal                                                                                                                                                                                                                                  Index

import scala.collection.IndexedSeqLike


trait Emit {
  self =>
  val traversal: Traversal

  def emit(): Unit = {
    val t = traversal.getForwardIterator()
    trav(t)
  }

  def trav (t: traversal.MyTraverser): Unit = {
    val id2tp = t.cminfo.reifiedIR.id2tp
    if(!t.scheduleoptions.isEmpty) {
      val tp: t.cminfo.reifiedIR.IR.TP[_] = id2tp(t.scheduleoptions.head._1)
      def emitBlock(b: traversal.cminfo.reifiedIR.IR.Block): Unit = {
        val btrav: traversal.MyTraverser = traversal.getForwardIterator(b)
        trav(btrav)
      }

      emitNode(tp.exp,tp.rhs, emitBlock)
      trav(t.scheduleoptions.head._2())
    }
  }

  def emitNode(sym: traversal.cminfo.reifiedIR.IR.Exp[_], rhs: traversal.cminfo.reifiedIR.IR.Def[_], block_callback: traversal.cminfo.reifiedIR.IR.Block => Unit): Unit =  {
    println(rhs)

  }



  def createEmit = {
    val x = new MyCollection[traversal.cminfo.reifiedIR.IR.TP[_]] {
      val traversal: self.traversal.type = self.traversal
    }
    def apply

   // val x = new MyCollection[Traversal, traversal.cminfo.reifiedIR.IR.TP[_]](100)

    //x.filter(p => p.ta)
  }
}


trait Bla{
  type blatype
}




abstract class MyCollection[TP] extends Iterable[TP] {
  val traversal: Traversal

  def iterator(): Iterator[TP] = {
    val t = traversal.MyTraverser
    val it = new Iterator[TP]{
      def hasNext(): Boolean = t.scheduleoptions.isEmpty
      def next(): TP = {
        val id2tp = t.cminfo.reifiedIR.id2tp
        val tp: TP = id2tp(t.scheduleoptions.head._1)
        tp
      }
    }
    it
  }
  /*def length():Int = ???
  def apply(idx: Int): TP = ???*/


}



































