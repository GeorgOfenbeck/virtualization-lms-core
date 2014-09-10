package scala.virtualization.lms
package internal

import scala.collection.immutable._
import scala.virtualization.lms.common.Reification
import scala.annotation.tailrec

/**
 * Trait CodeMotion will perform the code motion on the IR without requiring knowledge of the individual Def Types (the type of the DSL nodes)
 * While doing so it will persist all info in immutable data structures that it will hand out to the user
 **/

trait CodeMotion{
  val reifiedIR: Reification

  /**
   * @param children The children of the current block (if the block contains nested blocks, the nested children will be saved within the sub block)
   * @param child_schedule One possible topological sort that was used during code motion
   * @param uplinks All Nodes used from outside this block (should always be empty for the root block!)
   * @param roots All Nodes without predecessor in the current Block
   */
  case class BlockInfo(children: IntMap[EnrichedGraphNode], child_schedule: Stack[Int], uplinks: Set[Int], roots: Set[Int])

  case class IRBlockInfo(val head: Int, val blockinfo: IntMap[BlockInfo]){
    def getHead(): BlockInfo = blockinfo(head)
  }

  /**
   * @param irdef Some(Int) = an index into globaldefs to the according IR Definition
   *              None() = it refers to a symbol without a Def - usually those are symbols which will be created during
   *                       unparsing such as function parameters or loop variables
   * @param out Set of Nodes (indices into globaldefs) that we depend on
   * @param in Set of Nodes (indices into globaldefs) that depend on this node
   * @param bounds ? //RF!
   * @param blocks The result id of the blocks bound within this node (e.g. Block(Sym(4)) - we would save the 4)
   */
  case class EnrichedGraphNode( irdef: Option[Int], out: Set[Int], in: Set[Int], bounds: Set[Int], blocks: Set[Int] )

  import reifiedIR.IR._

  /**
   * An Intmap with globalDefEntryIndex -> EnchancedNode for all of globaldefs
   * see enhanceDAG() for more info
   **/
  lazy val enriched_graph = enhanceDAG()

  /**
   * used to build block_cache - should only be used internal (mutable)
   */
  protected var bcache = IntMap.empty[BlockInfo]




  lazy val block_cache: IRBlockInfo = {
    bcache = IntMap.empty[BlockInfo] //just in case someone initialized that by accident before
    getBlockInfo(reifiedIR.result)
  }



  protected def DeftoDagEntry(defentry: Stm, odep: Option[List[Exp[Any]]]): (Int,EnrichedGraphNode) = {
    defentry match {
      /*case TP(sym: Sym[Any], Reflect(rdef,u,es)) => {
        val id = sym.id
        val x = TP(sym,rdef)
        val node: Def[Any] with Product = rdef
        val out = node.productIterator.toSet
        //val outsyms = out.map( x =>syms(x) )
        val int_out = out.collect { case ele: Sym[Any] => ele.id }
        val int_blocks = out.collect { case Block(res: Sym[Any] ) => res.id}
        val in = Set.empty[Int]
        (id,EnrichedGraphNode(Some(x),int_out,in,Set.empty[Int],int_blocks))
      } */
      case TP(sym: Sym[Any], node: Def[Any] with Product) => {

        val out = node match {

          /*
          case Reflect(u,summary,deps) => {
            val x = TP(sym,u)
            DeftoDagEntry(x,Some(deps))

          }
          case Reify(u,summary,deps) => {
            //val out = node.productIterator.toSet
            val out: Set[Exp[Any]] = deps.toSet
            val x = TP(sym,node)
            val id = sym.id
            //val outsyms = out.map( x =>syms(x) )
            val int_out = out.collect { case ele: Sym[Any] => ele.id } ++ odep.collect{ case ele: Sym[Any] => ele.id }
            val int_blocks = Set.empty[Int]
            val in = Set.empty[Int]
            (id,EnrichedGraphNode(Some(x),int_out,in,Set.empty[Int],int_blocks))
          }
          */ //RF - move this to effectful variant

          case _ => {
            //val out = node.productIterator.toSet
            val out = syms(node)

            val x = TP(sym,node)
            val id = sym.id
            //val outsyms = out.map( x =>syms(x) )
            //val int_out = out.collect { case ele: Sym[Any] => ele.id } ++ odep.collect{ case ele: Sym[Any] => ele.id }
            val int_out = ( out.map( x => x.id) ++  odep.collect{ case ele: Sym[Any] => ele.id } ).toSet
            //            val int_blocks = out.collect { case IR.Block(res: IR.Sym[Any] ) => res.id}
            val int_blocks = blocks(node).collect { case Block(res: Sym[Any] ) => res.id}.toSet
            val in = Set.empty[Int]
            (id,EnrichedGraphNode(Some(x.sym.id),int_out,in,Set.empty[Int],int_blocks))
          }
        }
        out
      }
      case _ => {
        println("seems this case can happen for " + defentry)
        ???
      }
    }
  }


  /**

   * This will take the whole DAG and add the reverse dependency information to the nodes
   * (instead of only having the "who do I depend on" - also having "who depends on me"
   * Additionally it will create Nodes for Symbols which do not have a Def bound to them (e.g. loop indices)
   * Will be the first thing done during construction of an instance of this class
   * @return An IntMap with index into globaldefs -> EnrichedNode
   */
  protected def enhanceDAG(): IntMap[EnrichedGraphNode] = {
    //this gives us the dag without the reverse lookup
    val dagmap = globalDefs.foldLeft(IntMap.empty[EnrichedGraphNode]){
      (acc,ele) => acc + DeftoDagEntry(ele,None)
    }
    //creates a hashmap of the reverse edges (one hashmap per origin node)
    val reverse_dag = dagmap map {
      entry => {
        val outedges = entry._2.out ++ entry._2.blocks
        val hmap = outedges.foldLeft(IntMap.empty[Set[Int]]){
          (acc,ele) => acc + (ele -> Set(entry._1))
        }
        hmap
      }
    }
    //merges those hashmaps
    val merged = reverse_dag.foldLeft(IntMap.empty[Set[Int]]){
      (acc,ele) => acc.unionWith(ele, (index,a,b: Set[Int]) => a ++ b )
    }


    //create edges for symbols that appear in the graph, but which dont have a Def node
    //examples for such edges would be function parameters or loop indices
    val withpuresyms = merged.foldLeft(dagmap){
      (acc,ele) => {
        if (dagmap.contains(ele._1)) acc
        else {
          //in this case it has no Def
          acc + (ele._1,EnrichedGraphNode(None,Set.empty[Int],Set.empty[Int],Set.empty[Int],Set.empty[Int]))
        }
      }
    }

    //finally fuse with dag to incorporate reverse info
    val full = withpuresyms.foldLeft(IntMap.empty[EnrichedGraphNode]){
      (acc,ele) => {
        val key = ele._1
        val content = ele._2
        val rhs = content.irdef
        val out = content.out
        val in = merged.get(key).getOrElse(Set.empty[Int])
        val bound = boundSyms(rhs).map( x => x.id).toSet
        val blocks = content.blocks
        val entry : (Int,EnrichedGraphNode) = (key,EnrichedGraphNode(rhs,out,in,bound,blocks))
        acc + entry
      }
    }
    full
  }


  /**
   * Called by update_nest - finds all symbols that are transitively bound within the current block
   * Doing this is crucial for code motion - symbols that are not bound within the current block will recognized as "uplinks" in the current
   * scope
   * @param root the block result symbol from which we backtrack
   * @param backtrack used while recursively calling itself to keep track of where to backtrack (branches in DAG)
   * @param currentmap what we discovered so far
   * @param full the full graph
   * @return the final discovered graph that is bound within the block
  **/

  @tailrec
  private def depGraph(root: Int, backtrack: Set[Int], currentmap: IntMap[EnrichedGraphNode], full: IntMap[EnrichedGraphNode]): (IntMap[EnrichedGraphNode])  = { //private for tailrec
    if (backtrack.isEmpty)
      (currentmap)
    else {
      val track: Int = backtrack.head //backtrack is a stack of nodes we still not to go through
      val tracknode: EnrichedGraphNode = full(track) //get the head and traverse from there
      val newtrack = tracknode.in filter ( e => !(currentmap.contains(e)) && e != root) //make sure we didnt visit that path already and that we are not at the origin of the subgraph
      //val local_uplink = tracknode.out.filter( x => !full.contains(x))
      //val newuplink = uplink ++ local_uplink
      val newbacktrack = backtrack.tail ++ newtrack //add the alternative paths to the stack
      val entry : (Int,EnrichedGraphNode) = (track,tracknode)
      val newcurrent = currentmap + entry //add the new node of the path to the result
      depGraph(root,newbacktrack,newcurrent,full) //recurse on this path
    }
  }

  /**
   * This is called by visit_nested. Will update the given block (blocksym) in the bache by calling in turn visit_nested
   * on the subgraph that is bound within the block
   *
   * @param blocksym The Block Symbol (e.g. Block(Sym(4)) -> 4) for which we want to create a new bcache entry
   * @param curr The current scope - this shrinks with every node already visited in the process
   * @param full The full scope of the DAG - stays full always
   * @return Returns the updated (curr,full) tuple
   */
  protected def update_nest(blocksym: Int, curr: IntMap[EnrichedGraphNode], full: IntMap[EnrichedGraphNode]): (IntMap[EnrichedGraphNode], IntMap[EnrichedGraphNode])  = {
    val focused = curr(blocksym)
    val blockhead = focused.blocks.head
    if (!focused.blocks.tail.isEmpty) assert(false, "we are not handling this yet (multiple blocks in one symbol")
    val children = depGraph(blocksym,focused.bounds,IntMap.empty[EnrichedGraphNode],full) //get the subgraph that depends on that bounds
    //val centry: (Int, EnrichedGraphNode)  = (blockhead,focused)
    //val children = children_dep + centry //depgraph doesnt include the root
    val  (mark,pmark,stack,rcurrscope,rfullscope,uplinks,roots) =
      visit_nested(blockhead,Set.empty[Int],Set.empty[Int],Stack.empty[Int],children, full,Set.empty[Int],Set.empty[Int])
    val binfo = BlockInfo(children,stack,uplinks,roots)
    val cache_entry: (Int, BlockInfo) = (blockhead,binfo)
    bcache = bcache + cache_entry
    //val newfocused: EnrichedGraphNode = focused.copy( blockinfo = Some(binfo))
    val entry: (Int, EnrichedGraphNode)  = (blocksym,focused)
    (curr + entry,rfullscope + entry)
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
  protected def visit_nested (n: Int, tmark: Set[Int], pmark: Set[Int], sort: Stack[Int], curr_scope: IntMap[EnrichedGraphNode], full_scope: IntMap[EnrichedGraphNode],
                              uplinks: Set[Int], roots: Set[Int]): (Set[Int], Set[Int], Stack[Int], IntMap[EnrichedGraphNode],IntMap[EnrichedGraphNode], Set[Int], Set[Int])  = {
    if (!curr_scope.contains(n)) {
      return (tmark,pmark,sort, curr_scope, full_scope, uplinks + n, roots) //add n to uplinks
    }
    if (tmark.contains(n))  //if n has a temporary mark then stop (not a DAG)
      assert(false,"not a dag")

    val newtmark = tmark + n //mark n temporarily

    if (pmark.contains(n)){ //we already did that node - so return
      return (tmark,pmark,sort, curr_scope, full_scope, uplinks, roots)
    }

    else {
      val focused: EnrichedGraphNode = curr_scope(n)

      val next = focused.out //.filter(x => full_scope(x).irdef.isDefined) //all nexts

      val (allnext, curr_nscope, full_nscope) =
        if (!focused.blocks.isEmpty) {//we have a block
          val (curr_uscope, full_uscope): (IntMap[EnrichedGraphNode],IntMap[EnrichedGraphNode]) =
            if (!bcache.contains(focused.blocks.head))
              update_nest(n,curr_scope, full_scope)
            else
              (curr_scope, full_scope) //we update the scope to have the blockinfo


          val nfocused = focused //curr_uscope(n) //refresh the focused
            if (!bcache.contains(focused.blocks.head)) assert(false, "this should just not happen")
            val binfo = bcache(focused.blocks.head)
            (binfo.uplinks ++ next, curr_uscope, full_uscope)
        }
        else {
          (next,curr_scope, full_scope)
        }

      val newroots =
        if (allnext.filter(x => curr_scope.contains(x)).isEmpty) //this node is one of the root symbols within the block
          roots + n
        else
          roots



      val (rtmark,rpmark,rsort,rcurrscope,rfullscope,ruplinks,rroots) =
        allnext.foldLeft((newtmark,pmark,sort,curr_nscope, full_nscope,uplinks,newroots)){
          (acc,ele) => visit_nested(ele,acc._1,acc._2,acc._3,acc._4,acc._5,acc._6 - ele,acc._7) }

      val newpmark = if (full_scope(n).irdef.isEmpty) rpmark else rpmark + n
      val newstack = if (full_scope(n).irdef.isEmpty) rsort else rsort :+ n
      (tmark,newpmark,newstack,rcurrscope,rfullscope,ruplinks, rroots)
    }
  }


  /** This will traverse the DAG in topological order (result to input direction) and create the BlockInfo datastructure
    * To do so it will recurse into all blocks and bind symbols to blocks
    * It will also note all symbols within blocks that are referencing blocks higher up in the nesting
    * After this function is executed bcache will be filled with a BlockInfo data structure per block that appears in the DAG
   * @param block The block on which code motion should be performed - should always be the result block of the reified DAG
   * @tparam A
   */
  protected def getBlockInfo[A](block: Block[A]): IRBlockInfo = {
    val res = getBlockResult(block)
    //RF! just not nice
    val resid: Int = block match {
      case Block(Sym(id)) => id
      case Block(Const(x)) => {
        ??? //we never have this in our code - //TODO - fix for common case
      }
    }


    if (!bcache.contains(resid)) { //this should always be true


      val  (mark,pmark,stack,rscope,rfullscope,uplinks,roots) = //calling visited_nested with the empty status variables and the full graph to start things off
        visit_nested(resid,Set.empty[Int],Set.empty[Int],Stack.empty[Int],enriched_graph,enriched_graph,Set.empty[Int],Set.empty[Int])


      val cache_entry: (Int, BlockInfo) = (resid,BlockInfo(rfullscope,stack,uplinks,roots))
      bcache = bcache + cache_entry
    }
    else{
      assert(false,"in the current setup getBlockInfo should only be callable from the root block - therefore we should" +
        "never end up here!")
    }
    assert(bcache.contains(resid),"sanity check fails?")
    IRBlockInfo(resid,bcache)
  }

}

object CodeMotion {
  /** Takes a reified Program as an input. It then follows the dependencies of the the result of the reified program
   * and also stores the reverse edges (for each node - which nodes depend on it).
   * For CodeMotion purpose it also stores for each Block the following information:
    * RF! //update this
    *  Statements that have to be part of the Block (e.g. dependent on Loop iterator)
    *  Statements that are "free" in the block (no dependencies) - this should only happen in the top most block
    *  Uplinks of the current Block (Dependencies of Statements within the block on Statements outside the Bock)
    *
   * @param preifiedIR
   * @author Georg Ofenbeck
   * @return
   */
  def apply(preifiedIR : Reification): CodeMotion = {
    val cm = new CodeMotion {
      override val reifiedIR: Reification = preifiedIR
    }
    cm
  }




}
