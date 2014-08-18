package scala.virtualization.lms
package internal

import scala.collection.immutable._
import scala.virtualization.lms.common.Reification

trait CodeMotion{
  val reifiedIR: Reification
  //val block: IR.BlockInfo
  case class BlockInfo(children: IntMap[EnrichedGraphNode], child_schedule: Stack[Int], uplinks: Set[Int])
  case class EnrichedGraphNode( irdef: Option[Int], out: Set[Int], in: Set[Int], bounds: Set[Int], blocks: Set[Int] )

  import reifiedIR.IR._


  lazy val enriched_graph = enhanceDAG()


  protected var schedule_cache = IntMap.empty[BlockInfo] //used to build block cache
  lazy val block_cache: IntMap[BlockInfo] = {
    getBlockInfo(reifiedIR.result)
    schedule_cache
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
            val out = node.productIterator.toSet
            val x = TP(sym,node)
            val id = sym.id
            //val outsyms = out.map( x =>syms(x) )
            val int_out = out.collect { case ele: Sym[Any] => ele.id } ++ odep.collect{ case ele: Sym[Any] => ele.id }
            val int_blocks = out.collect { case Block(res: Sym[Any] ) => res.id}
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


    //create nodes for the edges that are not a def
    val withpuresyms = merged.foldLeft(dagmap){
      (acc,ele) => {
        if (dagmap.contains(ele._1)) acc
        else { //its a pure sym
          acc + (ele._1,EnrichedGraphNode(None,Set.empty[Int],Set.empty[Int],Set.empty[Int],Set.empty[Int]))
        }
      }
    }

    //finally fuse with dag to incorporate reverse info
    //val full = dagmap.foldLeft(HashMap.empty[Int,(IR.Def[Any],Set[Int],Set[Int])]){
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








  protected def depGraph(root: Int, backtrack: Set[Int], currentmap: IntMap[EnrichedGraphNode], full: IntMap[EnrichedGraphNode]): (IntMap[EnrichedGraphNode])  = {
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


  protected def update_nest(blocksym: Int, curr: IntMap[EnrichedGraphNode], full: IntMap[EnrichedGraphNode]): (IntMap[EnrichedGraphNode], IntMap[EnrichedGraphNode])  = {
    val focused = curr(blocksym)
    val blockhead = focused.blocks.head
    if (!focused.blocks.tail.isEmpty) assert(false, "we are not handling this yet (multiple blocks in one symbol")
    val children = depGraph(blocksym,focused.bounds,IntMap.empty[EnrichedGraphNode],full) //get the subgraph that depends on that bounds
    //val centry: (Int, EnrichedGraphNode)  = (blockhead,focused)
    //val children = children_dep + centry //depgraph doesnt include the root
    val  (mark,pmark,stack,rcurrscope,rfullscope,uplinks) =
      visit_nested(blockhead,Set.empty[Int],Set.empty[Int],Stack.empty[Int],children, full,Set.empty[Int])
    val binfo = BlockInfo(children,stack,uplinks)
    val cache_entry: (Int, BlockInfo) = (blockhead,binfo)
    schedule_cache = schedule_cache + cache_entry
    //val newfocused: EnrichedGraphNode = focused.copy( blockinfo = Some(binfo))
    val entry: (Int, EnrichedGraphNode)  = (blocksym,focused)
    (curr + entry,rfullscope + entry)
  }

  protected def visit_nested (n: Int, tmark: Set[Int], pmark: Set[Int], sort: Stack[Int], curr_scope: IntMap[EnrichedGraphNode], full_scope: IntMap[EnrichedGraphNode], uplinks: Set[Int]): (Set[Int], Set[Int], Stack[Int], IntMap[EnrichedGraphNode],IntMap[EnrichedGraphNode], Set[Int])  = {
    if (!curr_scope.contains(n)) {
      return (tmark,pmark,sort, curr_scope, full_scope, uplinks + n) //add n to uplinks
    }
    if (tmark.contains(n))  //if n has a temporary mark then stop (not a DAG)
      assert(false,"not a dag")
    val newtmark = tmark + n //mark n temporarily
    if (pmark.contains(n)){
      return (tmark,pmark,sort, curr_scope, full_scope, uplinks)
    }
    else {
      val focused = curr_scope(n)
      val next = focused.out //.filter(x => full_scope(x).irdef.isDefined) //all nexts

      val (allnext, curr_nscope, full_nscope) =  if (!focused.blocks.isEmpty) {//we have a block
      val (curr_uscope, full_uscope): (IntMap[EnrichedGraphNode],IntMap[EnrichedGraphNode]) =

        if (!schedule_cache.contains(focused.blocks.head))
          update_nest(n,curr_scope, full_scope)
        else
          (curr_scope, full_scope) //we update the scope to have the blockinfo
      val nfocused = focused //curr_uscope(n) //refresh the focused
        if (!schedule_cache.contains(focused.blocks.head)) assert(false, "this should just not happen")
        val binfo = schedule_cache(focused.blocks.head)
        (binfo.uplinks ++ next, curr_uscope, full_uscope)
      }
      else {
        (next,curr_scope, full_scope)
      }

      val (rtmark,rpmark,rsort,rcurrscope,rfullscope,ruplinks) =
        allnext.foldLeft((newtmark,pmark,sort,curr_nscope, full_nscope,uplinks)){
          (acc,ele) => visit_nested(ele,acc._1,acc._2,acc._3,acc._4,acc._5,acc._6 - ele) }

      val newpmark = if (full_scope(n).irdef.isEmpty) rpmark else rpmark + n
      val newstack = if (full_scope(n).irdef.isEmpty) rsort else rsort :+ n
      (tmark,newpmark,newstack,rcurrscope,rfullscope,ruplinks)
    }
  }


  protected def getBlockInfo[A](block: Block[A]): BlockInfo = {
    val res = getBlockResult(block)
    val resid: Int = block match {
      case Block(Sym(id)) => id
      case Block(Const(x)) => {
        println(block)
        println(res)
        ??? //we never have this in our code - //TODO - fix for common case
      }
    }
    // in case we didnt schedule yet - do it now
    if (!schedule_cache.contains(resid)) {
      val  (mark,pmark,stack,rscope,rfullscope,uplinks) =
        visit_nested(resid,Set.empty[Int],Set.empty[Int],Stack.empty[Int],enriched_graph,enriched_graph,Set.empty[Int])
      val binfo = BlockInfo(rfullscope,stack,uplinks)
      val cache_entry: (Int, BlockInfo) = (resid,binfo)
      schedule_cache = schedule_cache + cache_entry
    }
    assert(schedule_cache.contains(resid),"sanity check fails?")
    val binfo = schedule_cache(resid)
    binfo
  }

}

object CodeMotion {




  /** Takes a reified Program as an input. It then follows the dependencies of the the result of the reified program
   * and also stores the reverse edges (for each node - which nodes depend on it).
   * For CodeMotion purpose it also stores for each Block the following information:
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
