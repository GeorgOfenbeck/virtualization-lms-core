package scala.lms
package internal

trait Schedule {
 val esc: ExposeScheduleChoice
 class MyCollection extends Iterable[esc.cminfo.reifiedIR.IR.TP[_]] {
  class MyIterator(val state: esc.MyScheduleChoice ) extends Iterator[esc.cminfo.reifiedIR.IR.TP[_]]{
   private var typemadness : esc.MyScheduleChoice = state    //had issues with doing this without a var / therefore the strange name
   def getTrav(): esc.MyScheduleChoice = typemadness
   def setTrav(t: esc.MyScheduleChoice): Unit = {
    typemadness = t
   }
   override def hasNext(): Boolean = {
    val t = getTrav()
    !t.scheduleoptions.isEmpty
   }
   override def next(): esc.cminfo.reifiedIR.IR.TP[_] = {
    val t = getTrav()
    val id2tp = t.cminfo.reifiedIR.id2tp
    val tp: esc.cminfo.reifiedIR.IR.TP[_] = id2tp(t.scheduleoptions.head._1)
    val schedule_options = t.scheduleoptions
    setTrav(schedule_options.head._2())
    tp
   }
  }

  def iterator(block: esc.cminfo.reifiedIR.IR.Block): Iterator[esc.cminfo.reifiedIR.IR.TP[_]] =
   new MyIterator(esc.getForwardIterator(block))

  def iterator(): Iterator[esc.cminfo.reifiedIR.IR.TP[_]] =
   new MyIterator(esc.getForwardIterator())
 }
}


