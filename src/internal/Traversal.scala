package scala.virtualization.lms
package internal


trait Traverser {
  self =>
  val cminfo: CodeMotion
  val explored: Vector[Int]

  type MyTraverser2 = Traverser{
    val cminfo :self.cminfo.type
  }
  val scheduleoptions: Vector[(Int , Unit => MyTraverser2)]

  protected def getNewFront(block: cminfo.BlockInfo, current_roots: Set[Int], root: Int): Set[Int] = {
    //val onblocks =
    val ir = cminfo.reifiedIR
    val tp = ir.id2tp(root)
    val onblocks = ir.IR.blocks(tp)
    val nexts = block.children(root).in //get all successors
    val withoutprev = nexts flatMap (
        next => { //for each successors
        val allprev = block.children(next).in //get its predecessors
        val withoutroot = allprev - root
          val onlyfromblock = withoutroot.filter(x => block.children.contains(x))
          if (!onlyfromblock.isEmpty) Some(next) else None
        }
        )
    val newroots = current_roots - root ++ withoutprev
    newroots
  }

  def getNewTraverser(block: cminfo.BlockInfo, current_roots: Set[Int], root: Int): MyTraverser2 = {
    val newfront = getNewFront(block,current_roots,root).toVector
    val newfs: Vector[(Int , Unit => MyTraverser2)] = newfront map (
      node => {
        val f: (Unit => MyTraverser2) = (u: Unit) => getNewTraverser(block,newfront.toSet,node)
        (node,f)
      }
      )

    val newtrav: MyTraverser2 = new Traverser {
      val cminfo: self.cminfo.type = self.cminfo
      val scheduleoptions: Vector[(Int , Unit => MyTraverser2)] = newfs
      val explored: Vector[Int] = Vector()
    }
    newtrav

  }
}




trait Traversal {
  self =>
  val cminfo: CodeMotion
  type MyTraverser = Traverser{
    val cminfo :self.cminfo.type
  }

  //this returns an iterator to traverse all free symbols (e.g. global variables)
  //symbols that are not bound on the arguments of the Block
  def getFreeSymsIterator() =  ???

  //returns a traversal iterator which traverses the DAG in Arguments -> Result direction
  def getForwardIterator(): MyTraverser = {
    val lam = cminfo.reifiedIR.rootlambda
    val t: MyTraverser = new Traverser {
      val cminfo: self.cminfo.type = self.cminfo
      val scheduleoptions = Vector()
      val explored: Vector[Int] = Vector()
    }
    val newfs: Vector[(Int , Unit => MyTraverser)] = {
      val id = cminfo.reifiedIR.def2tp(lam).exp.id
      val cache = cminfo.block_cache
      val f: (Unit => MyTraverser) = (u: Unit) => {
        t.getNewTraverser(cache.blockinfo(lam.y),Set.empty,id)
      }
      Vector((id,f))
    }

    val newtrav: MyTraverser = new Traverser {
      val cminfo: self.cminfo.type = self.cminfo
      val scheduleoptions: Vector[(Int , Unit => MyTraverser)]  = newfs
      val explored: Vector[Int] = Vector()
    }
    newtrav
  }


  //returns a traversal iterator which traverses the DAG in Arguments -> Result direction
  def getForwardIterator(block: cminfo.reifiedIR.IR.Block): MyTraverser = {
    val t: MyTraverser = new Traverser {
      val cminfo: self.cminfo.type = self.cminfo
      val scheduleoptions = Vector()
      val explored: Vector[Int] = Vector()
    }

    val binfo = cminfo.block_cache.blockinfo(block)
    val roots =  binfo.roots

    val newfs: Vector[(Int , Unit => MyTraverser)] = roots.toVector map (
      node => {
        val block = t.cminfo.block_cache.getHead()
        val f: (Unit => MyTraverser) = (u: Unit) => t.getNewTraverser(block,block.roots,node)
        (node,f)
      }
      )
    /*
        val newfs: Vector[(Int , Unit => Traverser)] = roots.toVector map (
          node => {
            val block = t.cminfo.block_cache.getHead()
            val f: (Unit => Traverser) = (u: Unit) => t.getNewTraverser(block,block.roots,node)
            (node,f)
          }
          )
    */
    val newtrav = new Traverser {
      val cminfo: self.cminfo.type = self.cminfo
      val scheduleoptions = newfs
      val explored: Vector[Int] = Vector()
    }
    newtrav
  }

  //returns a traversal iterator which traverses the DAG in Results -> Arguments direction
  def getBackwardIterator() = ???
}





object Traversal {
  def apply(cm : CodeMotion): Traversal = {
    val traversal = new Traversal{
      override val cminfo = cm
    }
    traversal
  }
}




/*trait BlockTraversalx {
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
    val binfo = cminfo.block_cache.getHead()
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


*/
