


import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._
import scala.lms.internal.InternalFunctionsExp
import scala.lms.targets.graphviz.GraphVizExport

class MyDSLProgram extends SPL_DSL with StagedScala_Exp{
  self =>

  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  val emitMat = new SPL_DSL2Mat {
    override val IR: self.type = self
  }

  val emitScala = new SPL_DSL2Scala {
    override val IR: self.type = self
  }

  val emitStagedScala = new SPL_DSL2StagedScala[StagedComplex]{
    override val IR: self.type = self
  }

  def myf(u: Rep[Unit]) = {
    val f2: Exp[SPL] = unit(F_2())
    val i2: Exp[SPL] = unit(I(2))
    (f2 tensor i2) compose (i2 tensor f2)
  }

  def exportgraph() = {
    val (code, cm) = emitGraph.emitDepGraphf(myf)
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("spl_SON.dot"))
    stream.println(code)
    stream.flush()
    stream.close()
  }

  def createMat() = {
    val (map,cm) = emitMat.emit(Map.empty,myf)
    for ((key,matrix) <- map) {
      println("matrix: " + key)
      MathUtilities.printm(matrix)
    }
  }

  def createScalaCode() = {
    val (map,cm) = emitScala.emit(Map.empty,myf)
    val toplevelf = map(map.keysIterator.max)
    val inputComplexVector = Vector(MyComplex(1.0,1.0),MyComplex(1.0,1.0),MyComplex(1.0,1.0),MyComplex(1.0,1.0))
    val output = toplevelf(inputComplexVector)
    println(output)
  }

}


val myprog = new MyDSLProgram

//myprog.myf()

myprog.exportgraph()
myprog.createMat()

myprog.createScalaCode()


