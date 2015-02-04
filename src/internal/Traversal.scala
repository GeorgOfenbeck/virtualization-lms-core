package scala.virtualization.lms
package internal



trait Traversal {
  self =>
  val cminfo: CodeMotion


  trait Traverser {
    self =>
    val cminfo: CodeMotion
    val explored: Vector[Int]
    val scheduleoptions: Vector[(Int , Unit => Traverser)]

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

    def getNewTraverser(block: cminfo.BlockInfo, current_roots: Set[Int], root: Int): Traverser = {
      val newfront = getNewFront(block,current_roots,root).toVector
      val newfs: Vector[(Int , Unit => Traverser)] = newfront map (
        node => {
          val f: (Unit => Traverser) = (u: Unit) => getNewTraverser(block,newfront.toSet,node)
          //val exp = cminfo.reifiedIR.id2tp(node).exp
          (node,f)
        }
        )

      /*val fnewfs = newfs.filter( p => {
        val id = p._1
        val tp = cminfo.reifiedIR.id2tp(id)
        tp.rhs match{
          case cminfo.reifiedIR.IR.ArgDef(x) => false
          case _ => true
        }
      })*/

      val newtrav = new Traverser {
        val cminfo: self.cminfo.type = self.cminfo
        val scheduleoptions = newfs
        val explored: Vector[Int] = Vector()
      }
      newtrav
    }
  }



  //this returns an iterator to traverse all free symbols (e.g. global variables)
  //symbols that are not bound on the arguments of the Block
  def getFreeSymsIterator() =  ???

  //returns a traversal iterator which traverses the DAG in Arguments -> Result direction
  def getForwardIterator(): Traverser = {
    val lam = cminfo.reifiedIR.rootlambda
    val t = new Traverser {
      val cminfo: self.cminfo.type = self.cminfo
      val scheduleoptions = Vector()
      val explored: Vector[Int] = Vector()
    }
    val newfs: Vector[(Int , Unit => Traverser)] = {
      val id = cminfo.reifiedIR.def2tp(lam).exp.id
      val cache = cminfo.block_cache
      val f: (Unit => Traverser) = (u: Unit) => {
        t.getNewTraverser(cache.blockinfo(lam.y),Set.empty,id)
      }
      Vector((id,f))
    }
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


  //returns a traversal iterator which traverses the DAG in Arguments -> Result direction
  def getForwardIterator(block: cminfo.reifiedIR.IR.Block): Traverser = {
    val t = new Traverser {
      val cminfo: self.cminfo.type = self.cminfo
      val scheduleoptions = Vector()
      val explored: Vector[Int] = Vector()
    }

    val binfo = cminfo.block_cache.blockinfo(block)
    val roots =  binfo.roots

    val newfs: Vector[(Int , Unit => Traverser)] = roots.toVector map (
      node => {
        val block = t.cminfo.block_cache.getHead()
        val f: (Unit => Traverser) = (u: Unit) => t.getNewTraverser(block,block.roots,node)
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
