package SpiralS2

import javax.swing.JFrame

import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph



/**
  * Created by rayda on 29-Dec-16.
  */

case class BreakDownNode(val left: BreakDownNode, val right: BreakDownNode){

}



object Bla{
  def DivisorPairs(n: Int): List[(Int,Int)] =  {    (2 to Math.sqrt(n).toInt ).filter(n%_== 0).flatMap(x=>(if(n/x == x) List(x) else List(n/x,x)) ).toList.sortWith(_>_).map(x=> (n/x,x))  }
}
object BreakDown{

  trait Tree{}
  case object Leaf extends Tree
  case class Node(val l: Tree, val v: Int, val r: Tree) extends Tree


  import scife.enumeration.dependent.Depend
  import scife.enumeration.{dependent, memoization}
  import dependent._
  import memoization._
  import scife.util._

  import scife.{ enumeration => e }
  // DSL
  import e._
  import Enum._
  import Depend._


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
}
class HelloWorld extends JFrame("Hello World") {
  ini()
  def ini() {
    import BreakDown._
    val breakdown_enum = getBreakdown()


    val dft64s = breakdown_enum(64)
    val graph = new mxGraph
    val parent: Object = graph.getDefaultParent

    val nodewidth = 40
    val nodeheigth = 40

    val dist = 20

    graph.getModel.beginUpdate()
    try {
      val dft = dft64s(0)
      dft

      def draw(t: Tree, x: Int, y: Int, parentnode: Option[Object]): Unit = {
        t match{
          case Node(l,v,r) => {
            val v = graph.insertVertex(parent, null, "DFT"+v, x, y, nodewidth, nodeheigth)
            draw(l,x - nodewidth, y + nodewidth + dist,Some(v))
            draw(l,x - nodewidth, y + nodewidth + dist,Some(v))
          }
          case Leaf =>{
            graph.insertVertex(parent, null, "F2", x, y, nodewidth, nodeheigth)
          }
        }
      }

      val v1 = graph.insertVertex(parent, null, "Hello", 20, 20, 80, 30)
      val v2 = graph.insertVertex(parent, null, "World!", 240, 150, 80, 30)

      graph.insertEdge(parent, null, "Edge", v1, v2)
    }
    finally graph.getModel.endUpdate()
    val graphComponent = new mxGraphComponent(graph)
    getContentPane.add(graphComponent)
  }

}

object Gui extends App{
  val frame = new HelloWorld
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setSize(400, 320)
  frame.setVisible(true)
}




