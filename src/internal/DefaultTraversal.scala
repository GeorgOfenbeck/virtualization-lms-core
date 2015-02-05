package scala.virtualization.lms
package internal

import scala.virtualization.lms.common._


trait PureDefaultTraversal extends ReifyPure{
  self =>
  val IR: BaseExp with PureFunctionsExp
  import IR._


  def default_traversal[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]):
    Traversal{ val cminfo: CodeMotion { val reifiedIR: ReificationPure{ val IR: self.IR.type }} } = {
    val reified = reifyProgram(f)(args,returns)
    val scheduled = CodeMotion(reified)
    val traverser: Traversal{ val cminfo: CodeMotion { val reifiedIR: ReificationPure{ val IR: self.IR.type }} } = Traversal(scheduled)
    //
    //val size = reified.id2tp.size
    traverser

/*

    def trav (t: Traverser): Unit = {
      val id2tp = t.cminfo.reifiedIR.id2tp

      /*t.scheduleoptions.map(x => {
        val tp = id2tp(x._1)

        println(s"${x._1} -> Type: ${tp.tag.tag.toString()}")
        //println(x._1)
      })

      println("--------")*/

      if(!t.scheduleoptions.isEmpty) {
        val tp = id2tp(t.scheduleoptions.head._1)


        //val blocks = traverser.cminfo.reifiedIR.IR.blocks(tp) //check if the first element contains a block
        def emitBlock(b: traverser.cminfo.reifiedIR.IR.Block): Unit = {
          val btrav: Traverser = traverser.getForwardIterator(b)
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

    trav(forward)*/
  }


  /*

  def emitNode(sym: IR.Exp[Any], rhs: IR.Def[Any], block_callback: Block => Unit): Unit =  {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }
*/

}
