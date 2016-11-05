package Filter2

import scala.swing._
import java.awt.image.BufferedImage
import java.net.URL
import javax.imageio.ImageIO


class ImagePanel(rows0: Int, cols0: Int) extends GridPanel(rows0, cols0) {
  private var _imagePath = ""
  private var buf = Option.empty[BufferedImage]

  def imagePath = _imagePath
  def imagePath_=(value: String): Unit = {
    _imagePath = value
    buf.foreach(_.flush()); buf = None
    buf = Some(ImageIO.read(new URL(value)))
    repaint()
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    buf.foreach(g.drawImage(_, 0, 0, null))
  }
}

object GenMain extends App {


  val f        = new Frame()
  val p        = new ImagePanel(1, 3)
  p.imagePath  = "http://i2.kym-cdn.com/entries/icons/facebook/000/001/030/dickbutt.jpg"
  p.contents ++= Seq.tabulate(p.rows * p.columns)(i => new Label((i + 1).toString))
  f.contents   = p
  f.visible    = true

  val frame = new MainFrame{
    title = "Simple Gui"
    size = new Dimension(1024,768)
    contents = Button("Click Me!")(println("Button was clicked"))
    centerOnScreen()
  }

  frame.visible = true
  val dsl = new Core
  dsl.codeexport()
  //dsl.graphvizexport()

}
