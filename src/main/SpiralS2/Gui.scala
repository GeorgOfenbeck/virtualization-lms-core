package SpiralS2

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

object Bla {
  def DivisorPairs(n: Int): List[(Int, Int)] = {
    (2 to Math.sqrt(n).toInt).filter(n % _ == 0).flatMap(x => (if (n / x == x) List(x) else List(n / x, x))).toList.sortWith(_ > _).map(x => (n / x, x))
  }
}

object BreakDown {

  trait Tree {
    def getsize(): Int
  }

  case object Leaf extends Tree {
    override def getsize() = 2
  }

  case class Node(val l: Tree, val v: Int, val r: Tree) extends Tree{
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


  def getBreakdown() = {
    val breakdown: DependFinite[Int, Tree] =
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


object Gui extends SimpleSwingApplication {
  /*val frame = new HelloWorld
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setSize(2048, 1080)
  frame.setVisible(true)*/

  import ExampleData._





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


  val internalTreeStatusBar = new Label {
    preferredSize = (100, 12)
  }

  def tree2model(x: BreakDown.Tree): BreakDownNode = {
    x match {
      case BreakDown.Node(l, v, r) => BreakDownNode("DFT" + v, tree2model(l), tree2model(r))
      case BreakDown.Leaf => BreakDownNode("F2")
    }
  }

  def getInternalBreakdownTree(x: BreakDown.Tree) = new Tree[BreakDownNode] {
    renderer = Renderer.labeled { f =>
      val icon = folderIcon
      (icon, f.name)
    }
    val modtree = tree2model(x)
    model = InternalTreeModel(modtree)(_.getchildren)
    expandAll()
  }

  // Use case 6: Mutable internal tree model
  val mutableInternalTree = new Tree[PretendFile] {


    model = InternalTreeModel(pretendFileSystem)(_.children)
    listenTo(selection)
    reactions += {
      case TreeNodeSelected(node) => internalTreeStatusBar.text = "Selected: " + node
    }

    renderer = Renderer.labeled { f =>
      val icon = if (f.isDirectory) folderIcon
      else fileIcon
      (icon, f.name)
    }
    editor = Editor((_: PretendFile).name, new PretendFile(_: String))
    expandRow(0)
  }

  class ButtonPanel(pretendFileTree: Tree[PretendFile], setStatus: String => Unit) extends GridPanel(10, 1) {

    val updateButton = new Button(Action("Directly update") {
      val pathToRename = pretendFileTree.selection.paths.leadSelection
      for (path <- pathToRename) {
        val oldName = path.last.name
        pretendFileTree.model.update(path, PretendFile("directly-updated-file"))
        setStatus("Updated " + oldName)
      }
    })

    val editButton = new Button(Action("Edit") {
      val pathToEdit = pretendFileTree.selection.paths.leadSelection
      for (path <- pathToEdit) {
        pretendFileTree.startEditingAtPath(path)
        setStatus("Editing... ")
      }
    })

    val insertButton = new Button(Action("Insert under") {
      val pathToInsertUnder = pretendFileTree.selection.paths.leadSelection
      for (path <- pathToInsertUnder) {
        val succeeded = pretendFileTree.model.insertUnder(path, PretendFile("new-under-" + path.last.name), 0)
        setStatus("Inserting " + (if (succeeded) "succeeded" else "failed"))
      }
    })

    val insertBeforeButton = new Button(Action("Insert before") {
      val pathToInsertBefore = pretendFileTree.selection.paths.leadSelection
      for (path <- pathToInsertBefore) {
        val succeeded = if (path.lengthCompare(1) > 0) {
          pretendFileTree.model.insertBefore(path, PretendFile("new-before-" + path.last.name))
        } else false
        setStatus("Inserting " + (if (succeeded) "succeeded" else "failed"))
      }
    })

    val insertAfterButton = new Button(Action("Insert after") {
      val pathToInsertAfter = pretendFileTree.selection.paths.leadSelection
      for (path <- pathToInsertAfter) {
        val succeeded = if (path.lengthCompare(1) > 0) {
          pretendFileTree.model.insertAfter(path, PretendFile("new-after-" + path.last.name))
        } else false
        setStatus("Inserting " + (if (succeeded) "succeeded" else "failed"))
      }
    })

    val removeButton = new Button(Action("Remove") {
      val pathToRemove = pretendFileTree.selection.paths.leadSelection
      for (path <- pathToRemove) {
        val succeeded = if (path.lengthCompare(1) > 0) {
          pretendFileTree.model remove path
        } else false
        setStatus("Remove " + (if (succeeded) "succeeded" else "failed"))
      }
    })

    contents += editButton
    contents += updateButton
    contents += insertButton
    contents += insertBeforeButton
    contents += insertAfterButton
    contents += removeButton
  }



  def top = new MainFrame {
    title = "DFT Decompositions"

    import BreakDown._

    val breakdown_enum = getBreakdown()
    val default_dft_size = 8

    var dft_variants = breakdown_enum(Math.pow(2,default_dft_size).toInt)
    var cur_variant = dft_variants(0)


    def variant2Map(x: BreakDown.Tree, sofar: Map[List[Int],Int], parent: List[Int]): Map[List[Int],Int] = {
      x match {
        case BreakDown.Leaf => sofar
        case BreakDown.Node(l,v,r) => {
          val cur = parent :+ v
          val nentry = sofar + (cur -> l.getsize())

          val left = variant2Map(l,nentry,cur :+ -1)
          val right = variant2Map(r,left,cur :+ 1)
          right
        }
      }
    }



    contents = new TabbedPane {

      import TabbedPane.Page
      import BorderPanel.Position._





      //Create the label.
      val variants = new BoxPanel(Orientation.Vertical) {
        border = CompoundBorder(TitledBorder(EtchedBorder, "Variant"), EmptyBorder(5,5,5,10))
        val sizelabel = new Label("DFT size 2^n")
        val dft_size =
          new Slider() {
            min = 1
            value = default_dft_size
            max = 14
            majorTickSpacing = 1
            paintLabels = true
            paintTicks = true
          }
        val variantlabel = new Label("Variant")
        val slider_variant =
          new Slider() {
            min = 0
            value = 0
            max = dft_variants.size-1
            majorTickSpacing = (dft_variants.size-1)/10
            paintLabels = true
            paintTicks = true
          }

        contents += sizelabel
        contents += dft_size
        contents += variantlabel
        contents += slider_variant
        listenTo(slider_variant)
        listenTo(dft_size)
        reactions += {
          case ValueChanged(`dft_size` ) => {
            dft_variants = breakdown_enum(Math.pow(2,dft_size.value).toInt)
            slider_variant.max_=(dft_variants.size-1)
            slider_variant.paintLabels_=(false)
            slider_variant.paintTicks_=(false)
            slider_variant.majorTickSpacing_=(0)
            slider_variant.majorTickSpacing_=((dft_variants.size-1)/10)
            slider_variant.value_=(0)
            //slider_variant.revalidate()
            //slider_variant.repaint()
          }
          case ValueChanged(`slider_variant`) => {
            cur_variant = dft_variants(slider_variant.value)
            val newtree = getInternalBreakdownTree(cur_variant)
            scpanel.viewportView_=(newtree)
          }
        }
      }

      val displaytree = getInternalBreakdownTree(dft_variants(0))

      val scpanel = new ScrollPane(displaytree)

      val buttons =new FlowPanel {
        border = Swing.EmptyBorder(5, 5, 5, 5)
        contents += new Button(Action("Generate Code") {

          val varmap = variant2Map(cur_variant,Map.empty,List.empty)
          val dsl = new Core(cur_variant, varmap)
          dsl.codeexport()
        })
      }

      pages += new Page("Runtime Breakdown",
        new BorderPanel {
          layout(variants) = North
          layout(internalTreeStatusBar) = South
          layout(scpanel) = Center
          layout(buttons) = East
        })
    }

    size = (1024, 768): Dimension
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




