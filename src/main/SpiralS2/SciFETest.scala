package scife
package enumeration
package showcase



import scife.util.Math.Catalan

object Bla{
  def DivisorPairs(n: Int): List[(Int,Int)] =  {    (2 to Math.sqrt(n).toInt ).filter(n%_== 0).flatMap(x=>(if(n/x == x) List(x) else List(n/x,x)) ).toList.sortWith(_>_).map(x=> (n/x,x))  }
}


object BinarySearchTreeTest {

  // define your own test
  def testFun(tree: Tree) = println(tree.toString)

  // example data structure--full classes of data structures available in scife.util.structures
  trait Tree {
    def invariant(tree: Tree) =
      valueOrdering(tree)

    def valuesInRange(t: Tree, min: Int, max: Int): Boolean = t match {
      case Leaf => true
      case Node(l, v, r) => min <= v && max >= v &&
        valuesInRange(l, min, max) && valuesInRange(r, min, max)
    }
    def valueOrdering(t: Tree): Boolean = {
      def correctOrdering(t: Tree, min: Int, max: Int): Boolean = t match {
        case Leaf => true
        case Node(l, v, r) => min <= v && max > v &&
          correctOrdering(l, min, v) && correctOrdering(r, v + 1, max)
      }

      correctOrdering(t, Int.MinValue, Int.MaxValue)
    }

    def size(t: Tree): Int = t match {
      case Leaf => 0
      case Node(l, v, r) => 1 + size(l) + size(r)
    }
  }

  case object Leaf extends Tree
  case class Node(l: Tree, v: Int, r: Tree) extends Tree {
    def this(v: Int) = this(Leaf, v, Leaf)
  }

  object Node {
    def apply(v: Int) = new Node(v)
  }
}

object SciFETest extends App{
  import dependent._
  import memoization._
  import scife.util._

  import scife.{ enumeration => e }
  // DSL
  import e._
  import Enum._
  import Depend._
  import BinarySearchTreeTest._






  val bst =
    rec[(Int, Range), Tree]({
      case (self, (size, r)) => {
        if (size <= 0) Leaf
        else {
          val left: DependFinite[(Int, Int), Tree] =
            self ↓[(Int, Int)] {
              case (ls, m) =>
                (ls, r.start to (m - 1))
            }

          val right: DependFinite[(Int, Int), Tree] =
            self ↓[(Int, Int)] {
              case (ls, m) =>
                (size - ls - 1, (m + 1) to r.end)
            }

          val part1: Finite[(Int,Int)] = (0 until size) ⊗ r
          val part2: DependFinite[(Int,Int),(Tree,Tree)] = (left ⊗ right)
          //val sofar =  part1 ⊘  part2
          val sofar: Finite[((Int,Int),(Tree,Tree))] =  part1 ⊘  part2

           sofar ↑ {
            case ((_, root), (lTree, rTree)) =>
              Node(lTree, root, rTree)
          }
        }
      }
    })
  def getBreakdown() = {
    val breakdown:DependFinite[Int, Tree]  =
      rec[Int, Tree]({
        case (self, size) => {
          if (size <= 2) Leaf
          else {
            val left: DependFinite[(Int, Int), Tree] =
              self ↓[(Int, Int)] {
                case (l, r) => l
              }

            val right: DependFinite[(Int, Int), Tree] =
              self ↓[(Int, Int)] {
                case (l, r) => r
              }

            val divpairs: Vector[(Int, Int)] = Bla.DivisorPairs(size).toVector
            val part1: Finite[(Int, Int)] = divpairs
            val part2: DependFinite[(Int, Int), (Tree, Tree)] = (left ⊗ right)
            val sofar: Finite[((Int, Int), (Tree, Tree))] = part1 ⊘ part2
            sofar ↑ {
              case ((l, r), (lTree, rTree)) =>
                Node(lTree, l * r, rTree)
            }
          }
        }
      })
    breakdown
  }

  //  val t = bst(5, 1 to 5) // enumerate all trees of size 15
    val size: Int = java.lang.Math.pow(2.0,13.0).toInt
    val t = getBreakdown()(size)

    //for (e <- t) println(e)
    println(t.size)



    println("aha")
  }

