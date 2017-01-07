/*
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
    var basecase_min = 16
    var basecase_max = 64
    val breakdown_enum : scife.enumeration.dependent.DependFinite[ (Int,Boolean), BreakDown.Tree] = ??? //getBreakdown(basecase_min,basecase_max)
    val default_dft_size = 4

    var dft_variants = breakdown_enum((Math.pow(2, default_dft_size).toInt, false))
    var cur_variant = dft_variants(0)
    var cur_dft_size = default_dft_size


    var slidervarcopy: Slider = null

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


    contents = new TabbedPane {

      import TabbedPane.Page
      import BorderPanel.Position._
      val slider_variant =
        new Slider() {
          min = 0
          value = 0
          max = dft_variants.size - 1
          majorTickSpacing = (dft_variants.size - 1) / 10
          paintLabels = true
          paintTicks = true
        }

      val checkbox_ndynamic = new CheckBox("Dynamic input size")
      checkbox_ndynamic.selected = true
      val textfield_basecase_min = new TextField {
        text = basecase_min.toString
        horizontalAlignment = Alignment.Left
      }
      val textfield_basecase_max = new TextField {
        text = basecase_max.toString
        horizontalAlignment = Alignment.Left
      }

      val textfield_nr_thread = new TextField {
        text = "4"
        horizontalAlignment = Alignment.Left
      }
      val textfield_vector_length = new TextField {
        text = "4"
        horizontalAlignment = Alignment.Left
      }

      val boxpanel_basecase = new BoxPanel(Orientation.Vertical){
        border = CompoundBorder(TitledBorder(EtchedBorder, "use Base Cases"), EmptyBorder(5, 5, 5, 10))
        contents.append(new Label("always when < "),textfield_basecase_min, new Label("try when < "), textfield_basecase_max)
      }
      val boxpanel_parallel = new BoxPanel(Orientation.Vertical){
        border = CompoundBorder(TitledBorder(EtchedBorder, "Parallelism"), EmptyBorder(5, 5, 5, 10))
        contents.append(new Label("# Threads"),textfield_nr_thread, new Label("SIMD vector length"), textfield_vector_length)
      }

      val radio_twid_fly = new RadioButton("Always on-the-fly")
      val radio_twid_pre = new RadioButton("Always pre-compute")
      val radio_twid_mix = new RadioButton("Try mix - inline if possible")
      val mutex_twiddle = new ButtonGroup(radio_twid_fly,radio_twid_pre,radio_twid_mix)


      val boxpanel_twiddles = new BoxPanel(Orientation.Vertical){
        border = CompoundBorder(TitledBorder(EtchedBorder, "Twiddle Factors"), EmptyBorder(5, 5, 5, 10))
        contents ++= mutex_twiddle.buttons
      }



      //Create the label.
      val variants = new BoxPanel(Orientation.Vertical) {
        border = CompoundBorder(TitledBorder(EtchedBorder, "Variant"), EmptyBorder(5, 5, 5, 10))
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


        contents += sizelabel
        contents += dft_size
        contents += checkbox_ndynamic
        contents += boxpanel_basecase
        contents += boxpanel_twiddles
        contents += boxpanel_parallel
        contents += variantlabel
        contents += slider_variant
        slidervarcopy = slider_variant //to manipulate from outside
        listenTo(slider_variant)
        listenTo(dft_size)
        reactions += {
          case ValueChanged(`dft_size`) => {
            dft_variants = breakdown_enum(Math.pow(2, dft_size.value).toInt, false)
            slider_variant.max_=(dft_variants.size - 1)
            slider_variant.paintLabels_=(false)
            slider_variant.paintTicks_=(false)
            slider_variant.majorTickSpacing_=(0)
            slider_variant.majorTickSpacing_=((dft_variants.size - 1) / 10)
            slider_variant.value_=(0)
            //slider_variant.revalidate()
            //slider_variant.repaint()
          }
          case ValueChanged(`slider_variant`) => {
            cur_variant = dft_variants(slider_variant.value)
            cur_dft_size = dft_size.value
            val newtree = getInternalBreakdownTree(cur_variant)
            scpanel.viewportView_=(newtree)
          }
        }
      }

      val displaytree = getInternalBreakdownTree(dft_variants(0))

      val scpanel = new ScrollPane(displaytree)

      val buttons = new FlowPanel {
        border = Swing.EmptyBorder(5, 5, 5, 5)
        contents += new Button(Action("Generate Code") {
          val varmap = variant2Map(cur_variant, Map.empty, List.empty)
          val dsl = new CorewGlue(cur_variant, ???, cur_dft_size)
          dsl.codeexport()
        })
        contents += new Button(Action("Generate and Time Code") {
          val varmap = variant2Map(cur_variant, Map.empty, List.empty)
          val dsl = new CorewGlue(cur_variant, ???, cur_dft_size)
          val f = dsl.compile()
          f();
        })
        contents += new Button{
          text = "Gen ALL"
          reactions += {
            case ButtonClicked(_) => {
              for (i <- 0 until dft_variants.size) {
                println("Variant " + i + " of " + dft_variants.size)
                slider_variant.value_=(i)
                val varmap = variant2Map(cur_variant, Map.empty, List.empty)
                val dsl = new CorewGlue(cur_variant, varmap, cur_dft_size)
                val f = dsl.compile()
                f();
              }
            }
          }
        }


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




*/
