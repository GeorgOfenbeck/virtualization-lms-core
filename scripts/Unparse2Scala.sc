


import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._
import scala.lms.internal.InternalFunctionsExp
import scala.lms.targets.graphviz.GraphVizExport

class MyDSLProgram extends SPL_DSL {
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

  val newIR = new StagedScala_Exp{}

  val emitStagedScala = new SPL_DSL2StagedScalaW{
    override val originIR: self.type = self
    override val targetIR: newIR.type = newIR
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

  def createStagedScalaCode() = {
    val iemit = new emitStagedScala.SPL_DSL2StagedScala {} //thats pretty ugly
    val (map,cm) = iemit.emit(Map.empty,myf)
    val toplevelf = map(map.keysIterator.max)
    val f = (in: newIR.ComplexVector) => newIR.ComplexVector(toplevelf(in.vec))

    val emitGraphScala = new GraphVizExport {
      override val IR: newIR.type = newIR
    }
    val (scalagraph, cm2) = {
      import newIR._
      emitGraphScala.emitDepGraphf(f)(exposeRepFromVComplex(4), exposeRepFromVComplex(4))
    }
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream("scala_SON.dot"))
    stream.println(scalagraph)
    stream.flush()
    stream.close()
  }

}


val myprog = new MyDSLProgram

//myprog.myf()

myprog.exportgraph()
myprog.createMat()

myprog.createScalaCode()
myprog.createStagedScalaCode()

