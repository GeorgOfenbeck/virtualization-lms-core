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


import scala.swing.TabbedPane.Page
import TabbedPane.Page
import BorderPanel.Position._
/**
  * Created by rayda on 05-Jan-17.
  */
object Gui2 extends EnumTree with scalax.chart.module.Charting {



  object SmallEnum{

    var  basecase_min = 4
    var  basecase_max = 8
    var  basecase_default = 4
    val default_dft_size = 4 //2^n
    val cur_dft_size = default_dft_size

    var breakdown_enum = BreakDown.getBreakdown(Some(basecase_min,basecase_max),basecase_default, None)
    var dft_variants = breakdown_enum((Math.pow(2, default_dft_size).toInt, false))
    var cur_variant = dft_variants(0)


    val radio_dft = new RadioButton("DFT")
    val radio_wht = new RadioButton("WHT")
    val mutex_transformtype = new ButtonGroup(radio_dft,radio_wht)

    val boxpanel_transformtype = new BoxPanel(Orientation.Horizontal){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Transform type"), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex_transformtype.buttons
    }

    val checkbox_threading = new CheckBox("Threading")
    val checkbox_vectorization = new CheckBox("Vectorization")

    val boxpanel_parallelism = new BoxPanel(Orientation.Horizontal){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Parallelism"), EmptyBorder(5, 5, 5, 10))
      contents += checkbox_vectorization
      contents += checkbox_threading
    }


    val radio_format_complex = new RadioButton("Complex Class")
    val radio_format_interleaved = new RadioButton("Interleaved Complex")
    val radio_format_splitcomplex = new RadioButton("Split Complex")
    val mutex_dataformat = new ButtonGroup(radio_format_complex,radio_format_interleaved,radio_format_splitcomplex)


    val boxpanel_dataformat = new BoxPanel(Orientation.Horizontal){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Data layout"), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex_dataformat.buttons
    }


    val radio_stat_size = new RadioButton("Static Input size")
    val radio_dyn_size = new RadioButton("General Input size")
    val mutex_size = new ButtonGroup(radio_stat_size,radio_dyn_size)


    val boxpanel_statvdyn = new BoxPanel(Orientation.Horizontal){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Fixed vs general sized input"), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex_size.buttons
    }







    val radio_basecase_default = new RadioButton("Use default base case size")
    val radio_basecase_pernode = new RadioButton("Use per node base case from config")
    val mutex_basecase = new ButtonGroup(radio_basecase_default, radio_basecase_pernode)

    val textfield_basecase_min = new TextField {
      text = basecase_min.toString
      horizontalAlignment = Alignment.Left
    }
    val textfield_basecase_max = new TextField {
      text = basecase_max.toString
      horizontalAlignment = Alignment.Left
    }
    val boxpanel_basecase_pernode = new BoxPanel(Orientation.Vertical){
      border = CompoundBorder(TitledBorder(EtchedBorder, "per Node Search space"), EmptyBorder(5, 5, 5, 10))
      contents.append(radio_basecase_pernode, new Label("Base case always when < "),textfield_basecase_min, new Label("try when < "), textfield_basecase_max)
    }
    val textfield_basecase_default = new TextField {
      text = basecase_default.toString
      horizontalAlignment = Alignment.Left
    }
    val boxpanel_basecase_default = new BoxPanel(Orientation.Vertical){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Default base case size / Fallback @ per Node"), EmptyBorder(5, 5, 5, 10))
      contents.append(radio_basecase_default, new Label("Default / fallback "),textfield_basecase_default)
    }

    val boxpanel_basecase = new BoxPanel(Orientation.Horizontal){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Base case config"), EmptyBorder(5, 5, 5, 10))
      contents.append(boxpanel_basecase_default, boxpanel_basecase_pernode)
    }



    val radio_twiddle_default = new RadioButton("Use default twiddle config")
    val radio_twiddle_pernode = new RadioButton("Use per node config config")
    val mutex_twiddle = new ButtonGroup(radio_twiddle_default, radio_twiddle_pernode)


    val radio_twiddle_onthefly = new RadioButton("Compute on the fly")
    val radio_twiddle_precompute = new RadioButton("Precompute")
    val mutex_twiddle_default = new ButtonGroup(radio_twiddle_onthefly, radio_twiddle_precompute)

    val checkbox_twiddle_inline = new CheckBox("Inline Twiddles @ unrolled code")

    val boxpanel_twiddle_default = new BoxPanel(Orientation.Vertical){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Default Twiddle policy"), EmptyBorder(5, 5, 5, 10))
      contents.append(checkbox_twiddle_inline)
      contents ++= mutex_twiddle_default.buttons
    }


    val boxpanel_twiddle = new BoxPanel(Orientation.Horizontal){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Twiddle config"), EmptyBorder(5, 5, 5, 10))
      contents.append(radio_twiddle_default, radio_twiddle_pernode, boxpanel_twiddle_default)
    }


    val checkbox_validate= new CheckBox("Validate Code (part of the timed code) ")

    val leftconfig = new BoxPanel(Orientation.Horizontal){
      contents.append(boxpanel_statvdyn,boxpanel_transformtype,boxpanel_dataformat,boxpanel_parallelism, checkbox_validate)
    }

    val rightconfig = new BoxPanel(Orientation.Horizontal){
      contents.append(boxpanel_basecase, boxpanel_twiddle)
    }

    val config = new BoxPanel(Orientation.Vertical){
      border = CompoundBorder(TitledBorder(EtchedBorder, "Config"), EmptyBorder(5, 5, 5, 10))
      contents.append( leftconfig,rightconfig)
    }

    //-------------------------------------------------Variants
    val slider_variant =
      new Slider() {
        min = 0
        value = 0
        max = dft_variants.size - 1
        majorTickSpacing = (dft_variants.size - 1) / 10
        paintLabels = true
        paintTicks = true
      }

    val displaytree = getInternalBreakdownTree(dft_variants(0))


    val scpanel = new ScrollPane(displaytree)
    scpanel.preferredSize_=((200, 768): Dimension)



    val data = for (i <- 1 to 5) yield (i,i)
    val chart = XYLineChart(data)


    val plotting = new BoxPanel(Orientation.Horizontal){
      contents.append(scpanel)
      contents.append(chart.toComponent)
    }





    val variants = new BoxPanel(Orientation.Vertical){
      contents.append(slider_variant)
      contents.append(plotting)
    }







    //-----------------------------------------------Buttons

    def makecode():CorewGlue = {
      val varmap = variant2Map(cur_variant, Map.empty, List.empty)
      new CorewGlue(cur_variant, varmap, cur_dft_size,radio_wht.selected,
        if(radio_dyn_size.selected) None else Some((Math.pow(2, default_dft_size).toInt)),
        radio_format_interleaved.selected,
        checkbox_threading.selected,
        textfield_basecase_default.text.toInt,
        checkbox_twiddle_inline.selected,
        radio_twiddle_precompute.selected,
        checkbox_validate.selected
      )
    }

    val buttons = new FlowPanel {
      border = Swing.EmptyBorder(5, 5, 5, 5)
      contents += new Button(Action("Generate Code") {
        val dsl = makecode()
        dsl.codeexport()
      })
      contents += new Button(Action("Generate and Time Code") {
        val dsl = makecode()
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

    radio_dyn_size.selected_=(true)
    radio_wht.selected_=(true)
    radio_format_complex.selected_=(true)
    checkbox_validate.selected_=(true)
    textfield_basecase_default.text_=("0")
    radio_basecase_default.selected_=(true)
    radio_twiddle_default.selected_=(true)
    checkbox_twiddle_inline.selected_=(true)
    radio_twiddle_onthefly.selected_=(true)


    //Refresh the tree
    breakdown_enum = if (radio_basecase_default.selected) BreakDown.getBreakdown(None,basecase_default,None) else BreakDown.getBreakdown(Some(basecase_min,basecase_max),basecase_default,None)
    dft_variants = breakdown_enum((Math.pow(2, default_dft_size).toInt, false))
    cur_variant = dft_variants(0)
    scpanel.viewportView_=(getInternalBreakdownTree(cur_variant))

    slider_variant.max_=(dft_variants.size - 1)
    if (dft_variants.size < 20){
      slider_variant.majorTickSpacing_=( if ( (dft_variants.size -1) < 5) dft_variants.size-1 else 5)
      slider_variant.minorTickSpacing_=( 1)
    } else {
      slider_variant.majorTickSpacing_=( (dft_variants.size - 1) / 10)
      slider_variant.minorTickSpacing_=( (dft_variants.size - 1) / 5)
    }

    slider_variant.paintLabels_=(true)
    slider_variant.paintTicks_=(true)


    val smallenum = new Page("Exhaustive enumeration for small sizes",
      new BoxPanel(Orientation.Vertical) {
        contents.append(config)
        contents.append(variants)
        contents.append(buttons)

        listenTo(slider_variant)
        reactions += {
          case ValueChanged(`slider_variant`) => {
            cur_variant = dft_variants(slider_variant.value)
            val newtree = getInternalBreakdownTree(cur_variant)
            scpanel.viewportView_=(newtree)
          }

        }
      }

    )


  }





  def top = new MainFrame {
    title = "SpiralS"
    size = (2*1024, 2*768): Dimension

    contents = new TabbedPane {
      pages += SmallEnum.smallenum


    }
  }





}
