package SpiralS2

import scala.swing.SimpleSwingApplication
import java.io._

import scala.swing._
import scalaswingcontrib.event.TreeNodeSelected
import scalaswingcontrib.tree.{InternalTreeModel, Tree}
import scala.xml.{Node, XML}
import scala.swing.{Action, BorderPanel, Button, Component, Dimension, GridPanel, Label, MainFrame, ScrollPane, SimpleSwingApplication, Swing, TabbedPane}
import Swing.{Icon, pair2Dimension}
import scalaswingcontrib.tree.{ExternalTreeModel, InternalTreeModel, Tree, TreeModel}
import scalaswingcontrib.event.TreeNodeSelected
import scala.collection.mutable
import Tree.{Editor, Renderer}
import scala.swing._
import scala.swing.event._
import scala.swing.Swing._
import scala.swing.ListView._
import scife.enumeration.dependent.Depend
import scife.enumeration.{dependent, memoization}
import dependent._
import memoization._

import scife.{enumeration => e}

import scala.swing.TabbedPane.Page
import TabbedPane.Page
import BorderPanel.Position._


object Bla {
  def DivisorPairs(n: Int): List[(Int, Int)] = {
    (2 to Math.sqrt(n).toInt).filter(n % _ == 0).flatMap(x => (if (n / x == x) List(x) else List(n / x, x))).toList.sortWith(_ > _).map(x => (n / x, x))
  }
}

object BreakDown {
  // DSL
  import e._
  import Enum._
  import Depend._
  import scife.util._

  trait Tree {
    def getsize(): Int
    val unroll: Boolean
    val isbasecase: Boolean
  }

  case class Leaf(val unroll: Boolean, val twiddlecomp: Boolean) extends Tree {
    override def getsize() = 2
    override val isbasecase: Boolean = true
  }

  case class Node(val l: Tree, val v: Int, val r: Tree, val unroll: Boolean, val isbasecase: Boolean) extends Tree {
    override def getsize() = v
  }


  import scife.enumeration.dependent.Depend
  import scife.enumeration.{dependent, memoization}
  import dependent._
  import memoization._
  import scife.util._

  import scife.{enumeration => e}
  // DSL
  import e._
  import Enum._
  import Depend._


  def getBreakdown(config_base: Option[(Int,Int)], base_default: Int, config_twiddle: Option[Boolean]) = {
    val breakdown: DependFinite[ (Int,Boolean), Tree] =
      rec[(Int,Boolean), Tree]({
        case (self, (size,isbase)) => {
          if (size <= 2) Finite.colToEnum(Vector(Leaf(true,true))) //Finite.colToEnum(Vector(Leaf(true,true),Leaf(true,false)))
          else {

            val left: DependFinite[(Boolean,(Int, Int)), Tree] =
              self ↓[(Boolean,(Int, Int))] {
                case ((basecase,(l, r))) => (l,basecase)
              }

            val right: DependFinite[(Boolean,(Int, Int)), Tree] =
              self ↓[(Boolean,(Int, Int))] {
                case ((basecase,(l, r))) => (r,basecase)
              }

            val divpairs: Vector[(Int, Int)] = Bla.DivisorPairs(size).toVector
            val baserange: Vector[Boolean] = config_base.fold[Vector[Boolean]](Vector(size <= base_default))(fb => {
              val (base_min, base_max) = fb
              Set(size <= base_min, size <= base_max).toVector
            })

            val twiddle = Vector(true,false)

            val part1: Finite[(Int, Int)] = divpairs

            val partb: Finite[(Boolean,(Int, Int))] = Finite.colToEnum(baserange) ⊗ part1 //base case

            val part2: DependFinite[(Boolean,(Int, Int)), (Tree, Tree)] = (left ⊗ right)

            val sofar: Finite[((Boolean,(Int, Int)), (Tree, Tree))] = partb ⊘ part2
            sofar ↑ {
              case ((b,(l, r)), (lTree, rTree)) =>  Node(lTree, l * r, rTree, true, (b || isbase))
            }
          }
        }
      })
    breakdown
  }
}




abstract class EnumTree extends SimpleSwingApplication{
  import ExampleData._


  def variant2Map(x: BreakDown.Tree, sofar: Map[List[Int], (Int,Boolean,Boolean)], parent: List[Int]): Map[List[Int], (Int,Boolean,Boolean)] = {
    x match {
      case BreakDown.Leaf(unroll,twid) => {
        val cur = parent :+ 2
        sofar + (cur -> (-1,true,twid))
      }
      case BreakDown.Node(l, v, r,unroll,isbasecase) => {
        val cur = parent :+ v
        val nentry = sofar + (cur -> (r.getsize(),isbasecase,false))

        val left = variant2Map(l, nentry, cur :+ Constants.encode_left)
        val right = variant2Map(r, left, cur :+ Constants.encode_right)
        right
      }
    }
  }

  case class BreakDownNode(private var nameVar: String, private val children: BreakDownNode*) {
    var parent: Option[BreakDownNode] = None
    children foreach {
      _.parent = Some(this)
    }
    private var childBuffer = mutable.ListBuffer(children: _*)

    override def toString = name

    def name = nameVar

    def siblingExists(siblingName: String) = parent.exists(_ childExists siblingName)

    def childExists(childName: String) = children.exists(_.name == childName)

    def getchildren: Seq[BreakDownNode] = childBuffer
  }

  def node_unroll(x: BreakDown.Tree): BreakDownNode = BreakDownNode("Unroll = " + x.unroll)
  def node_isbasecase(x: BreakDown.Tree): BreakDownNode = BreakDownNode("is Base Case = " + x.isbasecase)
  def node_twiddle(twiddlecomp: Boolean, x: BreakDown.Tree): BreakDownNode = if (x.isbasecase) BreakDownNode("Twiddles: inlined") else
  if (twiddlecomp) BreakDownNode("Twiddles: on the fly") else BreakDownNode("Twiddles: precomputed")

  def tree2model(x: BreakDown.Tree): BreakDownNode = {
    x match {
      case BreakDown.Node(l, v, r,unroll,isbasecase) => BreakDownNode("DFT" + v,node_unroll(x), node_isbasecase(x), tree2model(l), tree2model(r) )
      case BreakDown.Leaf(unroll, twiddlecomp) => BreakDownNode("F2", node_unroll(x), node_isbasecase(x), node_twiddle(twiddlecomp,x))
    }
  }

  def getInternalBreakdownTree(x: BreakDown.Tree) = new Tree[BreakDownNode] {
    renderer = Renderer.labeled { f =>
      val icon = if (f.getchildren.isEmpty) fileIcon else folderIcon
      (icon, f.name)
    }
    val modtree = tree2model(x)
    model = InternalTreeModel(modtree)(_.getchildren)
    expandAll()
  }



  object ExampleData {

    // File system icons
    def getIconUrl(path: String) = resourceFromClassloader(path) ensuring(_ != null, "Couldn't find icon " + path)

    val fileIcon = Icon(getIconUrl("/scalaswingcontrib/test/images/file.png"))
    val folderIcon = Icon(getIconUrl("/scalaswingcontrib/test/images/folder.png"))

    // Contrived class hierarchy
    case class Customer(id: Int, title: String, firstName: String, lastName: String)

    case class Product(id: String, name: String, price: Double)

    case class Order(id: Int, customer: Customer, product: Product, quantity: Int) {
      def price = product.price * quantity
    }

    // Contrived example data
    val bob = Customer(1, "Mr", "Bob", "Baxter")
    val fred = Customer(2, "Dr", "Fred", "Finkelstein")
    val susan = Customer(3, "Ms", "Susan", "Smithers")
    val powerSaw = Product("X-123", "Power Saw", 99.95)
    val nailGun = Product("Y-456", "Nail gun", 299.95)
    val boxOfNails = Product("Z-789", "Box of nails", 23.50)
    val orders = List(
      Order(1, fred, powerSaw, 1),
      Order(2, fred, boxOfNails, 3),
      Order(3, bob, boxOfNails, 44),
      Order(4, susan, nailGun, 1))


    // Pretend file system, so we can safely add/edit/delete stuff
    case class PretendFile(private var nameVar: String, private val childFiles: PretendFile*) {
      var parent: Option[PretendFile] = None
      childFiles foreach {
        _.parent = Some(this)
      }
      private var childBuffer = mutable.ListBuffer(childFiles: _*)

      override def toString = name

      def name = nameVar

      def rename(str: String): Boolean = if (siblingExists(str)) false
      else {
        nameVar = str;
        true
      }

      def insertChild(child: PretendFile, index: Int): Boolean = {
        if (!isDirectory) false
        else if (childExists(child.name)) false
        else {
          child.parent = Some(this)
          childBuffer.insert(index, child)
          true
        }
      }

      def delete(): Boolean = parent.exists(_ removeChild this)

      def removeChild(child: PretendFile): Boolean = if (children contains child) {
        childBuffer -= child;
        true
      }
      else false

      def siblingExists(siblingName: String) = parent.exists(_ childExists siblingName)

      def childExists(childName: String) = children.exists(_.name == childName)

      def children: Seq[PretendFile] = childBuffer

      def isDirectory = children.nonEmpty
    }

    val pretendFileSystem = PretendFile("~",
      PretendFile("lib",
        PretendFile("coolstuff-1.1.jar"),
        PretendFile("coolstuff-1.2.jar"),
        PretendFile("robots-0.2.5.jar")),
      PretendFile("bin",
        PretendFile("cleanup"),
        PretendFile("morestuff"),
        PretendFile("dostuff")),
      PretendFile("tmp",
        PretendFile("log",
          PretendFile("1.log"),
          PretendFile("2.log"),
          PretendFile("3.log"),
          PretendFile("4.log")),
        PretendFile("readme.txt"),
        PretendFile("foo.bar"),
        PretendFile("bar.foo"),
        PretendFile("dingus")),
      PretendFile("something.moo"))
  }


}
