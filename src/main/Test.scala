
import scala.annotation.tailrec

object BLA extends App {

 case class Block(res: Int)
 case class EnrichedGraphNode(irdef: Int, predecessors: Set[Int], successors: Set[Int])

 val t1 = EnrichedGraphNode(1, Set(3, 9), Set.empty)
 val t2 = EnrichedGraphNode(2, Set(4), Set.empty)
 val t3 = EnrichedGraphNode(3, Set(5, 6), Set.empty)
 val t4 = EnrichedGraphNode(4, Set(6, 7), Set.empty)
 val t5 = EnrichedGraphNode(5, Set(8), Set.empty)
 val t6 = EnrichedGraphNode(6, Set(8), Set.empty)
 val t7 = EnrichedGraphNode(7, Set.empty, Set.empty)
 val t8 = EnrichedGraphNode(8, Set.empty, Set.empty)
 val t9 = EnrichedGraphNode(9, Set.empty, Set.empty)

 val nxts = Vector((0, Set(1, 2)))

 val cs = Map(1 -> t1, 2 -> t2, 3 -> t3, 4 -> t4, 5 -> t5, 6 -> t6, 7 -> t7, 8 -> t8, 9 -> t9)

 val x = visit_nested3(-1, 1, nxts, Set.empty, Map.empty, Set.empty)
 println(x._3)
 println(x._4)
 println(x._5)

 def getNode(n: Int): EnrichedGraphNode = {
  cs(n)
 }

 @tailrec
 private def visit_nested3(successor: Int, n: Int, nexts: Vector[(Int, Set[Int])], pmark: Set[Int], scope: Map[Int, EnrichedGraphNode], uplinks: Set[Int]): (Int, Vector[(Int, Set[Int])], Set[Int], Map[Int, EnrichedGraphNode], Set[Int]) = {
  val (ln, lnext) = nexts.last
  if (!lnext.contains(n)) assert(false, "this should not happen")
  val nlnext = lnext - n

  val curr_scope =
   if (scope.contains(n)) {
    val entry = scope(n)
    if (successor == -1) scope + (n -> entry) else scope + (n -> entry.copy(successors = entry.successors + successor))//if its -1 we did come from a recursion
   } else {
    val entry = getNode(n)
    if (successor == -1) scope + (n -> entry) else scope + (n -> entry.copy(successors = entry.successors + successor))//if its -1 we did come from a recursion
   }
  if (pmark.contains(n)) {
   if (nlnext.isEmpty) { //we finished this note already - might still need to add reverse edge
   val nnext = nexts.dropRight(1) // this node has no neighbour so we can remove its next entry
    visit_nested3(nnext.last._1, nnext.last._2.head, nnext, pmark, curr_scope, uplinks)
   } else {
    val nnext = nexts.dropRight(1) :+ (ln, nlnext) //remove our self from the next list and recurse
    visit_nested3(nnext.last._1, nnext.last._2.head, nnext, pmark, curr_scope, uplinks)
   }
  } else {
   val focused = curr_scope(n)
   val fnext = focused.predecessors
   if (fnext.isEmpty) { //it has no predecessor - therefore we are done with the node
    if (nlnext.isEmpty) { // this node has no predecessors - and was the last predec. of its sucessor
    val nnext = nexts.dropRight(1)
     if (nnext.isEmpty) (n, nnext, pmark + n, curr_scope, uplinks) else
      visit_nested3(nnext.last._1, nnext.last._2.head, nnext, pmark + n, curr_scope, uplinks)
    } else {
     val nnext = nexts.dropRight(1) :+ (ln, nlnext) //remove our self from the next list and recurse
     visit_nested3(nnext.last._1, nnext.last._2.head, nnext, pmark + n, curr_scope, uplinks)
    }
   } else { //it has predecessors - also the only case we have to create sucessor entries
   val t: (Int, Set[Int]) = (n, fnext) //the predecssors entry
    if (nlnext.isEmpty) { // this node has no neighbour so we can remove its next entry
    val nnext = nexts.dropRight(1) :+ t
     visit_nested3(n, nnext.last._2.head, nnext, pmark + n, curr_scope, uplinks)
    } else {
     val nnext = nexts.dropRight(1) :+ (ln, nlnext) :+ t //remove our self from the next list and recurse
     visit_nested3(n, nnext.last._2.head, nnext, pmark + n, curr_scope, uplinks)
    }
   }
  }
 }

}