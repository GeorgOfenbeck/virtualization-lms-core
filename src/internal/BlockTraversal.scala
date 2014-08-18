package scala.virtualization.lms
package internal


trait BlockTraversal {
  val cminfo: CodeMotion

  import cminfo.reifiedIR.IR._


  def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = Nil // RF - what does this do?

  def traverseBlock[A](block: Block[A]): Unit = {
    //GO: for now we assume this is called on the top symbol - which should be only a temporary hack
    val resid: Int = block match {
      case Block(Sym(id)) => id
      case Block(Const(x)) => {
        println(block)
        ??? //we never have this in our code - //TODO - fix for common case
      }
    }
    val binfo = cminfo.block_cache(resid)
    traverseStmsinBlock(binfo)
  }

  def traverseBlockFocused[A](block: Block[A]): Unit = {

  }

  def traverseStmsInBlock[A](stms: List[Stm]): Unit = {

  }

  def traverseProgram(): Unit = {
    val res = cminfo.reifiedIR.result
    traverseBlock(res)
  }

  def traverseStmsinBlock(binfo: cminfo.BlockInfo): Unit = {
    binfo.child_schedule.foreach( stmid => {
      val node = cminfo.enriched_graph(stmid)
      node.irdef map (x => {
        cminfo.reifiedIR.IR.globalDefsCache.get(x).map( stm => traverseStm(stm))
      })
    })
  }

  def traverseStm(stm: Stm): Unit = { // override this to implement custom traversal
    //blocks(stm.rhs) foreach traverseBlock
  }



}



object BlockTraversal{


}
