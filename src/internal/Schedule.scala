package scala.virtualization.lms
package internal



trait Schedule {
  val traversal: Traversal
  class MyCollection extends Iterable[traversal.cminfo.reifiedIR.IR.TP[_]] {
    class MyIterator(val state: traversal.MyTraverser ) extends Iterator[traversal.cminfo.reifiedIR.IR.TP[_]]{
      var typemadness : traversal.MyTraverser = state
      def getTrav(): traversal.MyTraverser = typemadness
      def setTrav(t: traversal.MyTraverser): Unit = {
        typemadness = t
      }
      override def hasNext(): Boolean = {
        val t = getTrav()
        !t.scheduleoptions.isEmpty
      }
      override def next(): traversal.cminfo.reifiedIR.IR.TP[_] = {
        val t = getTrav()
        val id2tp = t.cminfo.reifiedIR.id2tp
        val tp: traversal.cminfo.reifiedIR.IR.TP[_] = id2tp(t.scheduleoptions.head._1)
        val schedule_options = t.scheduleoptions
        setTrav(schedule_options.head._2())
        tp
      }
    }

    def iterator(block: traversal.cminfo.reifiedIR.IR.Block): Iterator[traversal.cminfo.reifiedIR.IR.TP[_]] =
      new MyIterator(traversal.getForwardIterator(block))

    def iterator(): Iterator[traversal.cminfo.reifiedIR.IR.TP[_]] =
      new MyIterator(traversal.getForwardIterator())
  }
}


