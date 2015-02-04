package scala.virtualization.lms
package internal

import scala.virtualization.lms.common._


trait PureDefaultTraversal {
  self =>
  val IR: BaseExp with ReifyPure with PureFunctionsExp



  def default_traversal[A,R](f: Function1[A,R])(implicit args: IR.IR.ExposeRep[A], returns: IR.IR.ExposeRep[R]) = {
    val reified = IR.reifyProgram(f)(args,returns)
    val scheduled = CodeMotion(reified)
    val traverser = Traversal(scheduled)
    val forward = traverser.getForwardIterator()
    val size = reified.id2tp.size
    def trav (t: Traverser): Unit = {
      val id2tp = t.cminfo.reifiedIR.id2tp

      t.scheduleoptions.map(x => {
        val tp = id2tp(x._1)

        println(s"${x._1} -> Type: ${tp.tag.tag.toString()}")
        //println(x._1)
      })

      println("--------")

      if(!t.scheduleoptions.isEmpty) {
        val blocks = traverser.cminfo.reifiedIR.IR.blocks(id2tp(t.scheduleoptions.head._1))
        if (!blocks.isEmpty)
        {
          blocks map (block => {
            val btrav: Traverser = traverser.getForwardIterator(block)
            trav(btrav)
          })
        }
        trav(t.scheduleoptions.head._2())
      }
    }

    trav(forward)
  }

  def emitNode(sym: IR.Exp[Any], rhs: IR.Def[Any]): Unit =  {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

}
