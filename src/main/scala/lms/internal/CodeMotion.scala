package scala.lms
package internal

import scala.annotation.tailrec
import scala.lms.util._

/*
  The general assumption of the new Codemotion is, that there cannot be a Sym without a corresponding TP! (which was different in previous LMS versions)
 */


object CodeMotion {
  /** Takes a reified program as an input.
    * Traverses the resulting graph in reverse order (result to inputs). (which will result in DeadCode Elemination)
    * While doing so it stores also info such as reverse edges.
    * For CodeMotion purpose it also stores for each Block the following information:
    * Statements that have to be part of the Block (e.g. dependent on Loop iterator) (bound)
    * Statements that are "free" in the block (no dependencies) - this should only happen in the top most block
    * Uplinks of the current Block (Dependencies of Statements within the block on Statements outside the Bock)
    *
    * @param preifiedIR
    * @author Georg Ofenbeck
    * @return
    */
  def apply(preifiedIR: ReificationPure): CodeMotion {val reifiedIR: preifiedIR.type} = {
    val cm = new CodeMotion {
      override val reifiedIR: preifiedIR.type = preifiedIR
    }
    cm
  }
}


trait CodeMotion {
  val reifiedIR: ReificationPure

  import reifiedIR.IR._


  case class BlockInfo3(childnodes: Set[EnrichedGraphNode], roots: Set[Int]) {
    val children: Map[Int, EnrichedGraphNode] = childnodes.map(x => x.irdef -> x).toMap
  }

  /**
   * @param irdef ID of the TP
   * @param predecessors Set of Nodes that we depend on (IDs)
   * @param successors Set of Nodes that depend on this node (IDs)
   * @param bounds Symbols bound under the current TP
   * @param blocks The IDs of symbols bound within Blocks of this Node (e.g. (Block{ Vector(Sym(4)}, Block{ Vector(Sym(4),Sym(3)} - we would save 3,4)
   */
  case class EnrichedGraphNode(irdef: Int, predecessors: Set[Int], successors: Set[Int], bounds: Set[Int], blocks: Set[Block])


  /**
   * used to build block_cache - should only be used internal (mutable)
   */
  //protected var bcache3 = Map.empty[Block, BlockInfo3]


  /**
   *
   * @param defentry the target TP of which we gather all information available from its position
   * @return a Node carrying all local Information
   */
  protected def TP2EnrichedGraphNode(defentry: TP[_]): EnrichedGraphNode = {
    val sym = defentry.sym
    val node = defentry.rhs
    val tag = defentry.tag
    val out = node.productIterator.toSet
    val id = sym.id
    val nsyms = syms(node)
    val bound = boundSyms(node).map(x => x.id).toSet
    val precessors = nsyms.map(x => x.id).toSet
    val embedded_blocks = blocks(node).toSet
    EnrichedGraphNode(id, precessors, Set.empty, bound, embedded_blocks)
    //RF - make sure that this is not an issue
    //val blocksyms = embedded_blocks.flatMap(x => x.res.map(exp => exp.id))
    //val precessors_without_blocks = precessors -- blocksyms
  }


  def iscold(b: Block): Boolean = true

  /*  private def createBlockInfo(nroots: Set[Int],  blockinfo: Map[Block, BlockInfo3], curr_block: Block, bmark: Map[Int,Int], n: Int, curr_tlevel: Int, level2block: Map[Int, Block] , curr_scope: Map[Int, EnrichedGraphNode], addn: Boolean): Map[Block, BlockInfo3] = {
      //this path is the "we are done with the graph - check for subgraphs"
      val binfo = BlockInfo3(Set.empty, nroots)
      val tblockinfo = blockinfo + (curr_block -> binfo)
      val nbmark = if(addn) bmark + (n -> curr_tlevel) else bmark
      //RF - we can do that a bit more efficient (first acc all children per node and only then copy)
      val nblockinfo = nbmark.foldLeft(tblockinfo){ (acc,ele) => {
        val tblock = ele._2
        val id = ele._1
        val b = level2block(tblock)
        val sofar = acc(b)
        val nentry = sofar.copy(childnodes = sofar.childnodes + curr_scope(id))
        acc + (b -> nentry)
      }}
      nblockinfo
    }*/

  private case class RetTmp(
                             pmark: Map[Int, Set[Int]],
                             scope: Map[Int, EnrichedGraphNode],
                             level2block: Map[LevelInfo, Block],
                             block2level: Map[Block, LevelInfo],
                             roots: Map[Int,Set[Int]]
                             )

  //in the (Int,Int) Tuples the first is the tree level - the second the unique id

  //levelinfo - (Treelevel, absolut id, parent, all parents)

  private case class LevelInfo(treelevel: Int, treeid: Int, parentlevelid: Int, allparents: Set[Int], allneighbour: Set[Int])

  //pmark is a Set of treeids
  @tailrec
  private def visit_nested3(curr_tlevel: LevelInfo, curr_level: LevelInfo, curr_block: Block, successor: Int, n: Int, nexts: Vector[(Int, Set[Int])], pmark: Map[Int, Set[Int]], roots: Map[Int,Set[Int]],
                            scope: Map[Int, EnrichedGraphNode],
                            block_nexts: Vector[(Int, Block)] /*, blockinfo: Map[Block, BlockInfo3]*/ , level2block: Map[LevelInfo, Block], block2level: Map[Block, LevelInfo],
                            lastcold: Map[Block, LevelInfo], potentialroot: Map[Int, Set[Int]], parentlevel: LevelInfo
                             )
  //: (Block, Int, Vector[(Int, Set[Int])], Map[Int, (Int, Int)], Map[Int, EnrichedGraphNode], Set[Int], Vector[(Int, Block)] /*, Map[Block, BlockInfo3]*/) = {
  : RetTmp = {
    val (ln, lnext) = nexts.last
    if (!lnext.contains(n)) {
      assert(false, "this should not happen")
    }
    val nlnext = lnext - n

    if (n == 10)
      println("bla")

    val (rlastcold: Map[Block, LevelInfo], rlevel2block: Map[LevelInfo, Block], rblock2level: Map[Block, LevelInfo], nblocks_next, curr_scope) =
      if (scope.contains(n)) {
        val entry = scope(n)
        if (successor == -1) (lastcold, level2block, block2level, block_nexts, scope + (n -> entry))
        else (lastcold, level2block, block2level, block_nexts, scope + (n -> entry.copy(successors = entry.successors + successor))) //if its -1 we did come from a recursion
      } else {
        val entry = TP2EnrichedGraphNode(id2tp(n)) //getNode(n)
        val nodeblocks = entry.blocks
        val levelcounter = level2block.size
        val allneighborids: Set[Int] = (for (i <- 1 until nodeblocks.size+1) yield levelcounter+i).toSet
        val tmp =
         nodeblocks.zipWithIndex.map(b =>
          LevelInfo(curr_level.treelevel + 1, levelcounter + 1 + b._2, curr_level.treeid, curr_level.allparents + curr_level.treeid, allneighborids - (levelcounter + 1 + b._2)) -> b._1).toMap
        val nlevel2block = level2block ++ tmp
          //((curr_level._1 + 1, levelcounter + b._2 + 1) -> b._1)).toMap
        val nblock2level = block2level ++ nodeblocks.zipWithIndex.map(b =>
            b._1 -> LevelInfo(curr_level.treelevel + 1, levelcounter + 1 + b._2, curr_level.treeid, curr_level.allparents + curr_level.treeid,allneighborids - (levelcounter + 1 + b._2))).toMap
            //(b._1 ->(curr_level._1 + 1, levelcounter + b._2 + 1))).toMap

        val check1 = tmp.map (t => t._1.treeid).toSet //all ids
        assert(check1.diff(allneighborids).isEmpty, "we screwed up!")

        val cur_lastcold = lastcold(curr_block)
        val nlastcold = lastcold ++ nodeblocks.map(b => if (iscold(b)) (b -> nblock2level(b)) else (b -> nblock2level(curr_block))).toMap
        val nblocks = block_nexts ++ entry.blocks.toVector.map(b => (entry.irdef, b))
        if (successor == -1) (nlastcold, nlevel2block, nblock2level, nblocks, scope + (n -> entry))
        else (nlastcold, nlevel2block, nblock2level, nblocks, scope + (n -> entry.copy(successors = entry.successors + successor))) //if its -1 we did come from a recursion
      }
    val (pmarkcontain, rtparent, rtlevel, rbtwlevel) = if (pmark.contains(n)) {       //we hit that node already
      val plevels = pmark(n)
      //check if it lives in one the parents of our current target
      // (isparent of tlevel, istlevel, tlevel > actual level >= currentlevel)
      //small early abort optimization oportunity here
      val (rtparent, rtlevel, rbtwlevel) = plevels.foldLeft((false,false,false)) {
        (acc,ele) => {
          val (tparent, tlevel, btwlevel) = acc
          val ntparent = if (!tparent) curr_tlevel.allparents.contains(ele) else tparent
          val ntlevel = if (!tlevel) curr_tlevel.treeid == ele else tlevel
          val nbtwlevel = if (btwlevel) curr_level.allparents.contains(ele) || curr_level.treeid == ele else btwlevel
          (ntparent,ntlevel,nbtwlevel)
        }
      }
      (true, rtparent, rtlevel, rbtwlevel)
      /*
      val marklevel = pmark(n) //
      if (false){
      /*if (al != curr_level._2 && tl == curr_level._1) {
        //we hit a node from a block beside us - move it and its predecessors up a level {
        val node = curr_scope(n)
        val updatesuccroots = node.successors.foldLeft(pmark) {
          //all nodes that are sucessors of this one now become roots
          (acc, ele) => {
            val succnode = acc(ele)
            if (succnode._1 == parentlevel._1)
              acc + (ele -> succnode.copy(_3 = false))
            else
              acc + (ele -> succnode.copy(_3 = true))
          }
        }
        val fnext = node.predecessors

        if (fnext.isEmpty) {
          val npmark = updatesuccroots + (n ->(parentlevel._1, parentlevel._2, true))
          visit_nested3(curr_tlevel, curr_level, curr_block, successor, n, nexts, npmark, bmark, roots, scope, block_nexts, level2block, block2level, lastcold, potentialroot, parentlevel)
          //we call ourself with the exact same parameters bsides pmark - and this time its like we hit a node from above instead of beside
        } else {
          val t: (Int, Set[Int]) = (n, fnext) //the predecssors entry
          val nnext = nexts :+ t
          val samelevel = fnext.takeWhile(p => {
            val (pt, pa, proot) = updatesuccroots(p)
            pt != parentlevel._1
          })
          val updatemyself = if (samelevel.isEmpty) updatesuccroots + (n ->(parentlevel._1, parentlevel._2, true)) else updatesuccroots + (n ->(parentlevel._1, parentlevel._2, false))
          visit_nested3(curr_tlevel, curr_level, curr_block, n, nnext.last._2.head, nnext, updatemyself, bmark, roots, scope, block_nexts, level2block, block2level, lastcold, potentialroot, parentlevel)
        }*/
        ???
      } else {
        val npotentialroot: Map[Int, Set[Int]] = if (!marklevel.contains(curr_level.treeid)) {
          //we hit a node from a level above / or beside us
          if (potentialroot.contains(ln)) {
            potentialroot + (ln -> (potentialroot(ln) + n))
          } else potentialroot + (ln -> Set(n))
        } else potentialroot //we dont add the node - we could also do an early abort sign here - but dont think it pays


        if (nlnext.isEmpty) {
          //our successor has no more neighbors
          //we finished this note already - might still need to add reverse edge
          val nnext = nexts.dropRight(1) // this node has no neighbour so we can remove its next entry
          val npmark = if (npotentialroot.contains(ln) && (npotentialroot(ln).size == curr_scope(ln).predecessors.size)) {
              //all the predecessors are on a higher level - therefore we have to update ln to a root
              val (tl, al, troot) = pmark(ln) //
              pmark + (ln ->(tl, al, true))
            } else pmark

          if (nnext.isEmpty) {
            //we are actually done
            //val nblockinfo = ??? //createBlockInfo(roots,blockinfo,curr_block, bmark, n, curr_tlevel, level2block, curr_scope, false)
            if (nblocks_next.isEmpty) {
              //we are also done with all subgraphs - therefore we are done
              RetTmp(npmark, curr_scope, rlevel2block, rblock2level)
              //(curr_block, n, nnext, pmark + (n -> curr_tlevel), curr_scope, nblocks_next) //, nblockinfo)
            } else {
              //we recurse into a sub block
              val (id, block) = nblocks_next.head
              val ids: Set[Int] = block.res.map(t => t.id).toSet
              val rnexts = Vector((-1, ids))
              val ntlevel = rlastcold(block)
              val ppotentialroot = npotentialroot - ln //not sure if purging is worth it
              visit_nested3(ntlevel, block2level(block), block, -1, block.res.head.id, rnexts, npmark, Map.empty, Set.empty, curr_scope, nblocks_next.tail, rlevel2block, rblock2level, rlastcold, ppotentialroot, curr_level)
            }
          }
          else //we are in the case that we dont have neighbors but still stuff on the todo list of nodes on this level
            visit_nested3(curr_tlevel, curr_level, curr_block, nnext.last._1, nnext.last._2.head, nnext, npmark, roots, curr_scope, nblocks_next, /*blockinfo,*/ rlevel2block, rblock2level, rlastcold, npotentialroot - ln, parentlevel)
        } else {
          //we hit this node before - update potential node and recurse to other neighbors of ln
          val nnext = nexts.dropRight(1) :+(ln, nlnext) //remove our self from the next list and recurse
          val nblcok = level2block(parentlevel)
          visit_nested3(curr_tlevel, curr_level, curr_block, nnext.last._1, nnext.last._2.head, nnext, pmark, roots, curr_scope, nblocks_next, /*blockinfo,*/ rlevel2block, rblock2level, rlastcold, npotentialroot, parentlevel)
        }
      }       */
    } else (false, false, false, false) //we didnt see it in pmark

    if (pmarkcontain && ( ! (rtparent || rtlevel || rbtwlevel))) { //the node exists somewhere on the path already
      assert((rtparent != rtlevel != rbtwlevel), "this should not happen - why is it on more then one?!")
      ???
    }
    else {
      val focused = curr_scope(n)
      val fnext = focused.predecessors
      val npmark = if (pmarkcontain) {
        // lets check if its a neighbour
        if (curr_tlevel.allneighbour.contains(n)){ //its a neighbour - just mark it down
          println("fill me")
        }
        pmark + (n->  (pmark(n)+curr_tlevel.treeid)) //we add the additional site of this node
        //in any case we are actually adding it to the wished target level (and conflict with neighbours are resolved later
      } else pmark + (n-> Set(curr_tlevel.treeid))
      if (fnext.isEmpty) {
        //it has no predecessor - therefore we are done with the node
        val nroots: Map[Int,Set[Int]] = if (roots.contains(n)) roots + (n -> (roots(n) + curr_tlevel.treeid)) else roots + (n -> Set(curr_tlevel.treeid))
        if (nlnext.isEmpty) {
          // this node has no predecessors - and was the last predec. of its sucessor (therefore a root)
          val nnext = nexts.dropRight(1)
          if (nnext.isEmpty) {
            if (nblocks_next.isEmpty) {
              //we are also done with all subgraphs - therefore we are done
              RetTmp(npmark, curr_scope, rlevel2block, rblock2level, nroots)
            }
            else {
              //we recurse into a sub block
              val (id, block) = nblocks_next.head
              val ids: Set[Int] = block.res.map(t => t.id).toSet
              val rnexts = Vector((-1, ids))
              val ntlevel = rlastcold(block)
              visit_nested3(ntlevel, block2level(block), block, -1, block.res.head.id, rnexts, npmark, nroots, curr_scope, nblocks_next.tail, /*nblockinfo,*/ rlevel2block, rblock2level, rlastcold, potentialroot, curr_level)
            }
          }
          else {
            visit_nested3(curr_tlevel, curr_level, curr_block, nnext.last._1, nnext.last._2.head, nnext, npmark, nroots, curr_scope, nblocks_next, /*blockinfo,*/ rlevel2block, rblock2level, rlastcold, potentialroot, curr_level)
          }
        } else {
          val nnext = nexts.dropRight(1) :+(ln, nlnext) //remove our self from the next list and recurse
          visit_nested3(curr_tlevel, curr_level, curr_block, nnext.last._1, nnext.last._2.head, nnext, npmark, nroots, curr_scope, nblocks_next, /*blockinfo,*/ rlevel2block, rblock2level, rlastcold, potentialroot, curr_level)
        }
      } else {
        //it has predecessors - also the only case we have to create sucessor entries
        val t: (Int, Set[Int]) = (n, fnext) //the predecssors entry
        if (nlnext.isEmpty) {
          // this node has no neighbour so we can remove its next entry
          val nnext = nexts.dropRight(1) :+ t
          visit_nested3(curr_tlevel, curr_level, curr_block, n, nnext.last._2.head, nnext, npmark, roots, curr_scope, nblocks_next, rlevel2block, rblock2level, rlastcold, potentialroot, curr_level)
        } else {
          val nnext = nexts.dropRight(1) :+(ln, nlnext) :+ t //remove our self from the next list and recurse
          visit_nested3(curr_tlevel, curr_level, curr_block, n, nnext.last._2.head, nnext, npmark, roots, curr_scope, nblocks_next, rlevel2block, rblock2level, rlastcold, potentialroot, curr_level)
        }
      }
    }




  }


  /**
   * This will traverse the DAG in topological order (result to input direction) and create the BlockInfo data structure
   * To do so it will recurse into all blocks and bind symbols to blocks
   * It will also note all symbols within blocks that are referencing blocks higher up in the nesting
   * After this function is executed bcache will be filled with a BlockInfo data structure per block that appears in the DAG
   * (those that are not eliminated by DeadCodeElimination)
   *
   * @param block The bottom most symbol of the graph which should always be the block contained in the root lambda
   * @return A Hashmap of Block -> BlockInfo which also contains info about the root block
   */
  protected def getBlockInfo3[A, B](lambda: AbstractLambda[A, B]): (Map[Int, EnrichedGraphNode], Map[Block, BlockInfo3]) = {
    TimeLog.timer("CodeMotion_getBlockInfo3", true)
    val nexts = Vector((-1, lambda.y.res.map(t => t.id).toSet))
    val n = lambda.y.res.head.id
    val rlinfo = LevelInfo(0,0,0,Set.empty,Set.empty)
    val l2b: Map[LevelInfo, Block] = Map(rlinfo -> lambda.y)
    val b2l: Map[Block, LevelInfo] = Map(lambda.y -> rlinfo)
    val lastcold: Map[Block, LevelInfo] = Map(lambda.y -> rlinfo)
    val r = visit_nested3(rlinfo, rlinfo, lambda.y, -1, n, nexts, Map.empty, Map.empty, Map.empty, Vector.empty, l2b, b2l, lastcold, Map.empty, rlinfo)
    val rlevel2block = r.level2block
    ???
    //val check = r.pmark.filter(p => p._2._3)
    /*val binfo = r.pmark.foldLeft(Map.empty[Block, BlockInfo3]) {
      (acc, ele) => {
        val nodeid = ele._1
        val (treelevel, levelid, isroot) = ele._2
        val leveltuple = (treelevel, levelid)
        val block = rlevel2block(leveltuple)
        if (acc.contains(block)) {
          val sofar = acc(block)
          val t = if (isroot)
            BlockInfo3(sofar.childnodes + r.scope(nodeid), sofar.roots + nodeid)
          else
            BlockInfo3(sofar.childnodes + r.scope(nodeid), sofar.roots)
          acc + (block -> t)
        }
        else {
          val t = if (isroot)
            BlockInfo3(Set(r.scope(nodeid)), Set(nodeid))
          else
            BlockInfo3(Set(r.scope(nodeid)), Set.empty)
          acc + (block -> t)
        }
      }
    }*/
    printlog("finished CM")
    TimeLog.timer("CodeMotion_getBlockInfo", false)
    //(r.scope, binfo)
    ???
  }

  /**
   * This is the actual starting point of the CM - we start the CM process on the Block contained in the toplevel
   * lambda of our staged program
   */
  lazy val (enriched_graph, block_cache3): (Map[Int, EnrichedGraphNode], IRBlockInfo3) = {
    TimeLog.timer("CodeMotion_getBlockInfo", true)
    val (fulldag, binfo) = getBlockInfo3(reifiedIR.rootlambda)
    val entry = TP2EnrichedGraphNode(def2tp(reifiedIR.rootlambda))
    val r = IRBlockInfo3(reifiedIR.def2tp(reifiedIR.rootlambda), binfo)
    TimeLog.timer("CodeMotion_getBlockInfo", false)
    (fulldag + (entry.irdef -> entry), r)
  }


  /**
   * @param root
   * @param blockinfo
   */
  case class IRBlockInfo3(val root: TP[_], val blockinfo: Map[Block, BlockInfo3]) {
    def getHead(): BlockInfo3 = {
      root.rhs match {
        case InternalLambda(f, x, y, args, returns) => blockinfo(y)
        case _ => assert(false, "a IRBlockInfo was created that did not have a Internal Lambda as root"); ???
      }
    }
  }

}


//------------------------------------------------------
/*

  /**
   * This is the actual starting point of the CM - we start the CM process on the Block contained in the toplevel
   * lambda of our staged program
   */
  lazy val block_cache: IRBlockInfo = {
    if (!bcache.isEmpty)
      printlog("Block Cache was not empty when starting CM - this should not happen!")
    bcache = Map.empty[Block, BlockInfo] //just in case someone initialized that by accident before
    val r = getBlockInfo(reifiedIR.rootlambda.y)
    getBlockInfo2(reifiedIR.rootlambda.y)
    val t = getBlockInfo3(reifiedIR.rootlambda)
    r
  }







  //
  case class GraphNodeLocalView(irdef: Int, predecessors: Set[Int], bounds: Set[Int], blocks: Set[Block])


  /**
   * @param children The children of the current block (if the block contains nested blocks, the nested children will be saved within the sub block)
   * @param child_schedule One possible topological sort that was used during code motion
   * @param uplinks All Nodes used from outside this block
   * @param roots All Nodes without predecessor in the current Block
   */
  case class BlockInfo(children: Map[Int, EnrichedGraphNode], child_schedule: Vector[Int], uplinks: Set[Int], roots: Set[Int])

  case class BlockInfo2(children: Map[Int, GraphNodeLocalView], child_schedule: Vector[Int], uplinks: Set[Int], roots: Set[Int])



  /**
   * @param root
   * @param blockinfo
   */
  case class IRBlockInfo(val root: TP[_], val blockinfo: Map[Block, BlockInfo]) {
    def getHead(): BlockInfo = {
      root.rhs match {
        case InternalLambda(f, x, y, args, returns) => blockinfo(y)
        case _ => assert(false, "a IRBlockInfo was created that did not have a Internal Lambda as root"); ???
      }
    }
  }

  case class IRBlockInfo2(val root: TP[_], val blockinfo: Map[Block, BlockInfo2]) {
    def getHead(): BlockInfo2 = {
      root.rhs match {
        case InternalLambda(f, x, y, args, returns) => blockinfo(y)
        case _ => assert(false, "a IRBlockInfo was created that did not have a Internal Lambda as root"); ???
      }
    }
  }

  /**
   * An Intmap with ID -> EnchancedNode for all TPs
   * see enhanceDAG() for more info
   **/
  lazy val enriched_graph = enhanceDAG()

  /**
   * used to build block_cache - should only be used internal (mutable)
   */
  protected var bcache = Map.empty[Block, BlockInfo]
  protected var bcache2 = Map.empty[Block, BlockInfo2]



  /**
   * This will traverse the DAG in topological order (result to input direction) and create the BlockInfo data structure
   * To do so it will recurse into all blocks and bind symbols to blocks
   * It will also note all symbols within blocks that are referencing blocks higher up in the nesting
   * After this function is executed bcache will be filled with a BlockInfo data structure per block that appears in the DAG
   * (those that are not eliminated by DeadCodeElimination)
   *
   * @param block The bottom most symbol of the graph which should always be the block contained in the root lambda
   * @return A Hashmap of Block -> BlockInfo which also contains info about the root block
   */
  protected def getBlockInfo(block: Block): IRBlockInfo = {
    TimeLog.timer("CodeMotion_getBlockInfo", true)
    val res = block.res
    printlog("doing CM")


    /*val focused = enriched_graph(blocksym)
    val rootids = block.res.map(r => r.id).toSet
    val children = depGraph(rootids, rootids ++ focused.bounds, Map.empty[Int, EnrichedGraphNode], enriched_graph) //get the subgraph that depends on that bounds
*/
    //RF - check if we need a fold left
    val (mark, pmark, stack, rscope, rfullscope, uplinks, roots) = //calling visited_nested with the empty status variables and the full graph to start things off
      res.foldLeft((Set.empty[Int], Set.empty[Int], Vector.empty[Int], enriched_graph, enriched_graph, Set.empty[Int], Set.empty[Int])) {
        (acc, ele) => {
          val blocksym = ele.id
          val focused = enriched_graph(blocksym)
          val rootids = block.res.map(r => r.id).toSet
          val children = depGraph(rootids, rootids ++ focused.bounds, Map.empty[Int, EnrichedGraphNode], enriched_graph) //get the subgraph that depends on that bounds
          val childrenwithroot = children + (blocksym -> focused)
          val (mark, pmark, stack, rscope, rfullscope, uplinks, roots) = visit_nested(ele.id, acc._1, acc._2, acc._3, acc._4, acc._5, acc._6 - ele.id, acc._7)
          val cache_entry: (Block, BlockInfo) = (block, BlockInfo(childrenwithroot, stack, uplinks, roots))
          bcache = bcache + cache_entry
          (mark, pmark, stack, rscope, rfullscope, uplinks, roots)
        }
      }

    assert(bcache.contains(block), "sanity check fails?")
    printlog("finished CM")
    val r = IRBlockInfo(reifiedIR.def2tp(reifiedIR.rootlambda), bcache)
    TimeLog.timer("CodeMotion_getBlockInfo", false)
    r
  }


  ////symbols that are not bound within the current block will recognized as "uplinks" in the current scope

  /**
   * finds all symbols that are transitively connected to the root symbols
   * @param roots the block result symbols from which we backtrack
   * @param backtrack used while recursively calling itself to keep track of where to backtrack (branches in DAG)
   * @param currentmap what we discovered so far
   * @param full the full graph
   * @return the final discovered graph that is bound within the block
   **/

  private def depGraph(roots: Set[Int], backtrack: Set[Int], currentmap: Map[Int, EnrichedGraphNode], full: Map[Int, EnrichedGraphNode]): (Map[Int, EnrichedGraphNode]) = {
    //private for tailrec
    if (backtrack.isEmpty)
      (currentmap)
    else {
      val track: Int = backtrack.head //backtrack is a stack of nodes we still not to go through
      if (!full.contains(track))
        assert(false, "??")
      val tracknode: EnrichedGraphNode = full(track) //get the head and traverse from there
      val tpredessors = tracknode.predecessors
      val newtrack = tpredessors filter (e => !(currentmap.contains(e)) && !roots.contains(e)) //make sure we didnt visit that path already and that we are not at the origin of the subgraph
      //val local_uplink = tracknode.out.filter( x => !full.contains(x))
      //val newuplink = uplink ++ local_uplink
      val newbacktrack = backtrack.tail ++ newtrack //add the alternative paths to the stack
      val entry: (Int, EnrichedGraphNode) = (track, tracknode)
      val newcurrent = currentmap + entry //add the new node of the path to the result
      depGraph(roots, newbacktrack, newcurrent, full) //recurse on this path
    }
  }


  /**
   * This will traverse the DAG in topological order (result to input direction) and create the BlockInfo data structure
   * To do so it will recurse into all blocks and bind symbols to blocks
   * It will also note all symbols within blocks that are referencing blocks higher up in the nesting
   * After this function is executed bcache will be filled with a BlockInfo data structure per block that appears in the DAG
   * (those that are not eliminated by DeadCodeElimination)
   *
   * @param block The bottom most symbol of the graph which should always be the block contained in the root lambda
   * @return A Hashmap of Block -> BlockInfo which also contains info about the root block
   */
  protected def getBlockInfo2(block: Block): IRBlockInfo2 = {
    TimeLog.timer("CodeMotion_getBlockInfo", true)
    val res = block.res.map(x => x.id).toSet
    printlog("doing CM")
    val children = depGraph2(res, res, Map.empty) //TODO: Check if we also need bound symbols of res here


    /*val (mark, pmark, stack, rscope, uplinks, roots) = //calling visited_nested with the empty status variables and the full graph to start things off
      res.foldLeft((Set.empty[Int], Set.empty[Int], Vector.empty[Int], children, Set.empty[Int], Set.empty[Int])) {
        (acc, ele) => {
          visit_nested2(ele, acc._1, acc._2, acc._3, acc._4, acc._5 - ele, acc._6)
        }
      }*/

    val starttmp = RetTmp(Set.empty[Int], Set.empty[Int], Vector.empty[Int], children, Set.empty[Int], Set.empty[Int])
    val r = visit_nested2(res, starttmp.tmark, starttmp.pmark, starttmp.sort, starttmp.curr_scope, starttmp.uplinks - res.head, starttmp.root)
    val cache_entry: (Block, BlockInfo2) = (block, BlockInfo2(children, r.sort, r.uplinks, r.root))
    bcache2 = bcache2 + cache_entry

    assert(bcache2.contains(block), "sanity check fails?")
    printlog("finished CM")
    val b = IRBlockInfo2(reifiedIR.def2tp(reifiedIR.rootlambda), bcache2)
    TimeLog.timer("CodeMotion_getBlockInfo", false)
    b
  }

  /**
   * finds all symbols that are transitively connected to the root symbols (without descending into blocks / subgraphs)
   * @param roots the block result symbols from which we backtrack
   * @param backtrack used while recursively calling itself to keep track of where to backtrack (branches in DAG)
   * @param currentmap what we discovered so far
   * @return the final discovered graph that is bound within the block
   **/
  @tailrec
  private def depGraph2(roots: Set[Int], backtrack: Set[Int], currentmap: Map[Int, GraphNodeLocalView])
  : Map[Int, GraphNodeLocalView] = {
    if (backtrack.isEmpty)
      currentmap
    else {
      val track: Int = backtrack.head //backtrack contains all nodes we still need to backtrack from, we pick any of those
      assert(id2tp.isDefinedAt(track),
        "IR Graph seems to be broken - Symbol " + track + "is referenced but not part of the graph")
      val tracktp = id2tp(track)
      val enrichedtrack = TP2LocalViewNode(tracktp)
      val newtrack = enrichedtrack.predecessors.filter(e => !(currentmap.contains(e)) && !roots.contains(e)) //make sure we didnt visit that path already and that we are not at the origin of the subgraph
      val newbacktrack = backtrack.tail ++ newtrack //add the alternative paths to the stack
      val entry: (Int, GraphNodeLocalView) = (track, enrichedtrack)
      val newcurrent = currentmap + entry
      depGraph2(roots, newbacktrack, newcurrent)
    }
  }

  /**
   *
   * @param defentry the target TP of which we gather all information available from its position
   * @return a Node carrying all local Information
   */
  protected def TP2LocalViewNode(defentry: TP[_]): GraphNodeLocalView = {
    val sym = defentry.sym
    val node = defentry.rhs
    val tag = defentry.tag
    val out = node.productIterator.toSet
    val id = sym.id
    val nsyms = syms(node)
    val bound = boundSyms(node).map(x => x.id).toSet
    val precessors = nsyms.map(x => x.id).toSet
    val embedded_blocks = blocks(node).toSet
    GraphNodeLocalView(id, precessors, bound, embedded_blocks)
    //RF - make sure that this is not an issue
    //val blocksyms = embedded_blocks.flatMap(x => x.res.map(exp => exp.id))
    //val precessors_without_blocks = precessors -- blocksyms
  }


  /**
   *
   * @param n The current node (index into globaldefs therefore also enriched_graph)
   * @param tmark A set of nodes that we are currently working on (and therefore should not see while exploring the rest of the graph)
   * @param pmark The set of nodes already processed
   * @param sort One possible topological sort of the current graph (the one used during CM)
   * @param curr_scope The subset of nodes that are available in the current scope - at the start this is the whole DAG and shrinks with block traversed
   * @param full_scope The whole Dag (index into globaldefs -> EnrichedNode)
   * @param uplinks All symbols that are predecessors, but are not in the current scope
   * @param roots All symbols that do not have a predecessor in the current scope (might have in a higher nest)
   * @return
   */
  protected def visit_nested(n: Int, tmark: Set[Int], pmark: Set[Int], sort: Vector[Int], curr_scope: Map[Int, EnrichedGraphNode], full_scope: Map[Int, EnrichedGraphNode],
                             uplinks: Set[Int], roots: Set[Int]): (Set[Int], Set[Int], Vector[Int], Map[Int, EnrichedGraphNode], Map[Int, EnrichedGraphNode], Set[Int], Set[Int]) = {
    if (!curr_scope.contains(n)) {
      return (tmark, pmark, sort, curr_scope, full_scope, uplinks + n, roots) //add n to uplinks
    }
    if (tmark.contains(n)) //if n has a temporary mark then stop (not a DAG)
      assert(false, "not a dag")

    val newtmark = tmark + n //mark n temporarily

    if (pmark.contains(n)) {
      //we already did that node - so return
      return (tmark, pmark, sort, curr_scope, full_scope, uplinks, roots)
    }

    else {
      val focused: EnrichedGraphNode = curr_scope(n)
      val next = focused.predecessors //.filter(x => full_scope(x).irdef.isDefined) //all nexts

      val (allnext, curr_nscope, full_nscope) =
        if (!focused.blocks.isEmpty) {
          //we have a block
          if (focused.blocks.size > 1)
            assert(false, "implement this!")
          val (curr_uscope, full_uscope): (Map[Int, EnrichedGraphNode], Map[Int, EnrichedGraphNode]) =
            if (!bcache.contains(focused.blocks.head)) {

              update_nest(focused.blocks.head, focused.bounds, curr_scope, full_scope)
            }
            else
              (curr_scope, full_scope) //we update the scope to have the blockinfo

          val nfocused = focused //curr_uscope(n) //refresh the focused
          if (!bcache.contains(focused.blocks.head)) assert(false, "this should just not happen")
          val binfo = bcache(focused.blocks.head)
          (binfo.uplinks ++ next, curr_uscope, full_uscope)
        }
        else {
          (next, curr_scope, full_scope)
        }

      val newroots =
        if (allnext.filter(x => curr_scope.contains(x)).isEmpty) //this node is one of the root symbols within the block
          roots + n
        else
          roots



      val (rtmark, rpmark, rsort, rcurrscope, rfullscope, ruplinks, rroots) =
        allnext.foldLeft((newtmark, pmark, sort, curr_nscope, full_nscope, uplinks, newroots)) {
          (acc, ele) => visit_nested(ele, acc._1, acc._2, acc._3, acc._4, acc._5, acc._6 - ele, acc._7)
        }

      val newpmark = rpmark + n
      val newstack = rsort :+ n
      (tmark, newpmark, newstack, rcurrscope, rfullscope, ruplinks, rroots)
    }
  }


  /**
   *
   * @param n The current node (index into globaldefs therefore also enriched_graph)
   * @param tmark A set of nodes that we are currently working on (and therefore should not see while exploring the rest of the graph)
   * @param pmark The set of nodes already processed
   * @param sort One possible topological sort of the current graph (the one used during CM)
   * @param curr_scope The subset of nodes that are available in the current scope - at the start this is the whole DAG and shrinks with block traversed
   * @param uplinks All symbols that are predecessors, but are not in the current scope
   * @param roots All symbols that do not have a predecessor in the current scope (might have in a higher nest)
   * @return
   */

  private def visit_nested2(nexts: Set[Int], tmark: Set[Int], pmark: Set[Int], sort: Vector[Int],
                            curr_scope: Map[Int, GraphNodeLocalView],
                            uplinks: Set[Int],
                            roots: Set[Int]
                             ): RetTmp = {
    val n = nexts.head
    //(Set[Int], Set[Int], Vector[Int], Map[Int, GraphNodeLocalView], Set[Int], Set[Int]) = {
    //n is not part of our currently visible scope - therefore it needs to be an uplink in correct LMS Dag
    if (!curr_scope.contains(n)) RetTmp(tmark, pmark, sort, curr_scope, uplinks + n, roots)
    else {
      if (tmark.contains(n)) assert(false, "not a dag") //if n has a temporary mark then stop (not a DAG)
      if (pmark.contains(n)) RetTmp(tmark, pmark, sort, curr_scope, uplinks, roots)
      else {
        val newtmark = tmark + n
        val focused: GraphNodeLocalView = curr_scope(n)
        val next = focused.predecessors
        val (allnext, curr_nscope) =
          if (focused.blocks.isEmpty) (next, curr_scope)
          else {
            //we have blocks - therefore we are looking into the subgraphs
            val curr_uscope = focused.blocks.foldLeft(curr_scope) {
              (acc_scope, fblock) => {
                //-------------------
                if (bcache2.contains(fblock)) acc_scope //RF - check
                else {
                  val blockrestail = fblock.res.tail.map(r => r.id).toSet
                  val roots = blockrestail + fblock.res.head.id
                  val children = depGraph2(roots, roots ++ focused.bounds, Map.empty)
                  val starttmp = RetTmp(Set.empty[Int], Set.empty[Int], Vector.empty[Int],
                    curr_scope, Set.empty[Int], Set.empty[Int])

                  val allnext = fblock.res.map(x => x.id).toSet
                  val r = visit_nested2(allnext, starttmp.tmark, starttmp.pmark, starttmp.sort, starttmp.curr_scope, starttmp.uplinks - allnext.head, starttmp.root)
                  val binfo = BlockInfo2(children, r.sort, r.uplinks, r.root)
                  val cache_entry: (Block, BlockInfo2) = (fblock, binfo)
                  bcache2 = bcache2 + cache_entry



                  curr_scope
                }
                //------------------
              }
            }
            assert(focused.blocks.filter(b => !bcache2.contains(b)).isEmpty,
              "Something went wrong during CodeMotion and visiting nested blocks")
            val allbinfos = focused.blocks.map(b => bcache2(b))
            val alluplinks = allbinfos.flatMap(b => b.uplinks)
            (next ++ alluplinks, curr_uscope)
          }
        val newroots = if (allnext.filter(x => curr_scope.contains(x)).isEmpty) roots + n else roots


        /*val (rtmark, rpmark, rsort, rcurrscope, ruplinks, rroots) =
          allnext.foldLeft(starttmp)//(newtmark, pmark, sort, curr_nscope, uplinks, newroots)) {
            (acc, ele) => visit_nested2(ele, acc._1, acc._2, acc._3, acc._4, acc._5 - ele, acc._6)
          }
      (tmark, rpmark + n, rsort :+ n, rcurrscope, ruplinks, rroots)*/

        val starttmp = RetTmp(newtmark, pmark, sort, curr_nscope, uplinks, newroots)
        val r = visit_nested2(allnext, starttmp.tmark, starttmp.pmark, starttmp.sort, starttmp.curr_scope, starttmp.uplinks - allnext.head, starttmp.root)
        RetTmp(r.tmark, r.pmark + allnext.head, r.sort :+ allnext.head, r.curr_scope, r.uplinks, r.root)
      }
    }
  }

  case class RetTmp(val tmark: Set[Int], val pmark: Set[Int], val sort: Vector[Int], val curr_scope: Map[Int, GraphNodeLocalView], val uplinks: Set[Int], root: Set[Int])








  /**
   * This is called by visit_nested. Will update the given block (blocksym) in the bache by calling in turn visit_nested
   * on the subgraph that is bound within the block
   *
   * @param blocksym The Block Symbol (e.g. Block(Sym(4)) -> 4) for which we want to create a new bcache entry
   * @param curr The current scope - this shrinks with every node already visited in the process
   * @param full The full scope of the DAG - stays full always
   * @return Returns the updated (curr,full) tuple
   */
  protected def update_nest(blockhead: Block, boundsyms: Set[Int], curr: Map[Int, EnrichedGraphNode], full: Map[Int, EnrichedGraphNode]): (Map[Int, EnrichedGraphNode], Map[Int, EnrichedGraphNode]) = {
    //val focused = curr(blocksym)
    //val blockhead = focused.blocks.head
    //if (!focused.blocks.tail.isEmpty) assert(false, "we are not handling this yet (multiple blocks in one symbol")
    val blockrestail = blockhead.res.tail.map(r => r.id).toSet
    val roots = blockhead.res.map(r => r.id).toSet
    val children = depGraph(roots, roots ++ boundsyms, Map.empty[Int, EnrichedGraphNode], full) //get the subgraph that depends on that bounds
    //val centry: (Int, EnrichedGraphNode)  = (blockhead,focused)
    //val children = children_dep + centry //depgraph doesnt include the root
    val (mark, pmark, stack, rcurrscope, rfullscope, uplinks, rroots) =
      blockhead.res.foldLeft(Set.empty[Int], Set.empty[Int], Vector.empty[Int], curr, full, Set.empty[Int], Set.empty[Int]) {
        (acc, ele) => visit_nested(ele.id, acc._1, acc._2, acc._3, acc._4, acc._5, acc._6 - ele.id, acc._7)
      }


    //val childrenwithroot = children + (root -> enriched_graph(root))
    val binfo = BlockInfo(children, stack, uplinks, rroots)
    //val binfo = BlockInfo(children,stack,uplinks,roots)
    val cache_entry: (Block, BlockInfo) = (blockhead, binfo)
    bcache = bcache + cache_entry
    //val newfocused: EnrichedGraphNode = focused.copy( blockinfo = Some(binfo))
    //val entry: (Int, EnrichedGraphNode)  = (blocksym,focused)

    //val tscope = curr + entry
    //val ret = curr -- children.map(k => k._1)
    //(ret,rfullscope)// + entry)
    //(curr + entry ,rfullscope + entry)
    (curr, full)
  }


  /**
   * This will take the whole DAG and add the reverse dependency information to the nodes
   * (instead of only having the "who do I depend on" - also having "who depends on me"
   * Will be the first thing done during construction of an instance of this class
   * @return An IntMap with TP ID -> EnrichedNode
   */
  protected def enhanceDAG(): Map[Int, EnrichedGraphNode] = {
    //this gives us the dag without the reverse lookup
    val dagmap: Map[Int, EnrichedGraphNode] = id2tp.foldLeft(Map.empty[Int, EnrichedGraphNode]) {
      (acc, ele) => acc + DeftoDagEntry(ele._2)
    }

    //creates a hashmap of the reverse edges (one hashmap per origin node)
    val reverse_dag = dagmap map {
      entry => {
        val blockouts: Set[Int] = entry._2.blocks.flatMap(x => x.res.map(y => y.id))
        val outedges = entry._2.predecessors
        val hmap = outedges.foldLeft(scala.collection.immutable.IntMap.empty[Set[Int]]) {
          //using IntMap here for the unionWith later
          (acc, ele) => acc + (ele -> Set(entry._1))
        }
        hmap
      }
    }
    //merges those hashmaps
    val merged = reverse_dag.foldLeft(scala.collection.immutable.IntMap.empty[Set[Int]]) {
      (acc, ele) => acc.unionWith(ele, (index, a, b: Set[Int]) => a ++ b)
    }

    //create edges for symbols that appear in the graph, but which dont have a Def node
    //examples for such edges would be function parameters or loop indices
    val withpuresyms = merged.foldLeft(dagmap) {
      (acc, ele) => {
        if (dagmap.contains(ele._1)) acc
        else {
          //in this case it has no Def
          //in this version of LMS this should never happen - throw an assertion
          assert(false, "it seems we discovered a Symbol without an assoziated TP - this should not happen")
          acc
        }
      }
    }


    //finally fuse with dag to incorporate reverse info
    val full = withpuresyms.foldLeft(Map.empty[Int, EnrichedGraphNode]) {
      (acc, ele) => {
        val key = ele._1
        val content = ele._2
        val tp = id2tp(content.irdef)
        val predecessors = content.predecessors
        val successors = merged.get(key).getOrElse(Set.empty[Int])
        val blocks = content.blocks
        val blocksyms = blocks.flatMap(b => b.res.map(r => r.id)) //we remove all symbols from blocks from being bound
        val bound = boundSyms(tp.rhs).map(x => x.id).toSet -- blocksyms
        val entry: (Int, EnrichedGraphNode) = (key, EnrichedGraphNode(content.irdef, predecessors, successors, bound, blocks))
        acc + entry
      }
    }
    full
  }


  protected def DeftoDagEntry(defentry: TP[_]): (Int, EnrichedGraphNode) = {
    defentry match {
      case TP(sym: Exp[_], node: Def[_] with Product, tag) => {
        //pattern match only to get Product - rewrite
        val out = node match {
          case _ => {
            val out = node.productIterator.toSet
            val id = sym.id
            val nsyms = syms(node)
            val precessors = nsyms.map(x => x.id).toSet
            val embedded_blocks = blocks(defentry.rhs).toSet
            val blocksyms = embedded_blocks.flatMap(x => x.res.map(exp => exp.id))
            val precessors_without_blocks = precessors -- blocksyms
            val successors = Set.empty[Int] // we will fill this in a next step
            (id, EnrichedGraphNode(sym.id, precessors_without_blocks, successors, Set.empty[Int], embedded_blocks))
            //(id,EnrichedGraphNode(sym.id,precessors,successors,Set.empty[Int],embedded_blocks))
          }
        }
        out
      }
    }
  }

}



*/
