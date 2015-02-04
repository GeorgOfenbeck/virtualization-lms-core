package scala.virtualization.lms
package internal


trait Emit {
  self =>
  val traversal: Traversal



  def emit(): Unit = {
    val t = traversal.getForwardIterator()
    trav(t)
  }

  def trav (t: Traverser): Unit = {
    val id2tp = t.cminfo.reifiedIR.id2tp
    if(!t.scheduleoptions.isEmpty) {
      val tp: t.cminfo.reifiedIR.IR.TP[_] = id2tp(t.scheduleoptions.head._1)
      def emitBlock(b: traversal.cminfo.reifiedIR.IR.Block): Unit = {
        val btrav: Traverser = traversal.getForwardIterator(b)
        trav(btrav)
      }

      emitNode(tp.exp,tp.rhs, emitBlock)
      /*if (!blocks.isEmpty)
      {
        blocks map (block => {
          val btrav: Traverser = traverser.getForwardIterator(block)
          trav(btrav)
        })
      }*/
      trav(t.scheduleoptions.head._2())
    }
  }

  def emitNode(sym: traversal.cminfo.reifiedIR.IR.Exp[_], rhs: traversal.cminfo.reifiedIR.IR.Def[_], block_callback: traversal.cminfo.reifiedIR.IR.Block => Unit): Unit =  {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }



}
