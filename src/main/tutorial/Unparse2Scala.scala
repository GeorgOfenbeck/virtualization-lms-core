


import ch.ethz.spirals.datatypes.DataTypeFactories.SplitComplexArray
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._

import scala.lms.internal.InternalFunctionsExp
import scala.lms.ops.PurePrimitiveOpsExp
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

object Main extends App {

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

    val newIR = new StagedScala_Exp with PurePrimitiveOpsExp with InternalFunctionsExp with ScalaCompile{
      self =>
      val codegen = new ScalaCodegen
        with EmitHeadInternalFunctionAsClass
        with ScalaGenPrimitivOps
        with ScalaGenNumericOps
      {
        val IR: self.type = self
      }
    }

    val emitStagedScala = new SPL_DSL2StagedScalaW {
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
      val (map, cm) = emitMat.emit(Map.empty, myf)
      for ((key, matrix) <- map) {
        println("matrix: " + key)
        MathUtilities.printm(matrix)
      }
    }

    def createScalaCode() = {
      val (map, cm) = emitScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      val inputComplexVector = Vector(MyComplex(1.0, 1.0), MyComplex(1.0, 1.0), MyComplex(1.0, 1.0), MyComplex(1.0, 1.0))
      val output = toplevelf(inputComplexVector)
      println(output)
    }

    def createStagedScalaCode() = {
      val iemit = new emitStagedScala.SPL_DSL2StagedScala {} //thats pretty ugly
      val (map, cm) = iemit.emit(Map.empty, myf)
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

    def createParamizedNonStagedScalaCode() = {
      import ch.ethz.spirals.datatypes._
      import ch.ethz.spirals.datatypes.DataTypeFactories._
      import ch.ethz.spirals.datatypes.UnstagedImplicitOps._
      import ElementOpsUnstaged._
      //val bla = new SplitComplexArray[NoRep, Array, NoRep, Double](4)
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = 4

      val input = new SplitComplexArray[NoRep, Array, NoRep, Double](size)

      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[NoRep, ElementOpsUnstaged.Complex,NoRep,Double]{
        override val IR: self.type = self
        override val targetIR: newIR.type = newIR
      }

      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      //toplevelf(input)
      val out = toplevelf(input)
      println(out)
      println("---")
    }


    def createParamizedStagedScalarScalaCode() = {
      import ch.ethz.spirals.datatypes._
      import ch.ethz.spirals.datatypes.UnstagedImplicitOps._
      import ElementOpsUnstaged._
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = 4

      implicit val aops: ArrayOps[NoRep,Array,newIR.Rep,Double] = new newIR.ScalarSingleArrayOps[Double]
      implicit val nrep: NumericOps[newIR.Rep[Double]] =  new newIR.StagedNumericOps.NumericRepOps[Double]
      implicit val erep: ElementOps[Complex,newIR.Rep[Double]] = new ElementOpsUnstaged.ComplexOps[newIR.Rep[Double]]()





      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[NoRep, ElementOpsUnstaged.Complex,newIR.Rep,Double]{
          override val IR: self.type = self
          override val targetIR: newIR.type = newIR
        }

      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      //toplevelf(input)


      implicit def exposeRepFromCVector(size: Int)(implicit tag: Manifest[Double]): newIR.ExposeRep[CVector[NoRep, ElementOpsUnstaged.Complex,emitGraphScala.IR.Rep,Double]]
      = new newIR.ExposeRep[CVector[NoRep, ElementOpsUnstaged.Complex,emitGraphScala.IR.Rep,Double]] (){
        import newIR._
        val freshExps: (Unit => Vector[newIR.Exp[_]]) = (u: Unit) => {
          val t = for(i <- 0 until size) yield Vector(newIR.Arg[Double](newIR.convertFromManifest(tag)), newIR.Arg[Double](newIR.convertFromManifest(tag)))
          t.foldLeft(Vector.empty[newIR.Exp[_]])((acc,ele) => { acc ++ ele })
        }
        val vec2t: (Vector[newIR.Exp[_]] => CVector[NoRep, ElementOpsUnstaged.Complex,newIR.Rep,Double]) = (v: Vector[newIR.Exp[_]]) => {
          val x = for (i <- 0 until size) yield{
            val re: newIR.Exp[Double] = v(i*2).asInstanceOf[newIR.Exp[Double]] //RF: use the Manifest to make this safe
            val im: newIR.Exp[Double] = v(i*2+1).asInstanceOf[newIR.Exp[Double]]
            Complex(re,im)
          }
          val t = new SplitComplexArray[NoRep, Array, newIR.Rep, Double](size)
          val r: CVector[NoRep, ElementOpsUnstaged.Complex,newIR.Rep,Double] = t.ini(x)
          r
        }
        val t2vec = (c: CVector[NoRep, ElementOpsUnstaged.Complex,emitGraphScala.IR.Rep,Double]) => {
          val t = (for (i <- 0 until size) yield c(i)).toVector
          t.foldLeft(Vector.empty[emitGraphScala.IR.Rep[_]]){(acc,ele) => acc ++ Vector(ele._re,ele._im)}
        }
      }
      val expose = exposeRepFromCVector(4)

      val emitGraphScala = new GraphVizExport {
        override val IR: newIR.type = newIR
      }
      val (scalagraph, cm2) = {
        emitGraphScala.emitDepGraphf(toplevelf)(expose,expose)
      }
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("scala_scalar_SON.dot"))
      stream.println(scalagraph)
      stream.flush()
      stream.close()

      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("source.txt"))
      val esc = newIR.codegen.emitSource(toplevelf,"testClass",stream2)(expose,expose)
      stream2.flush()
      stream2.close()

      println("---")
    }


    def createParamizedStagedScalaCode() = {
      import ch.ethz.spirals.datatypes._
      import ch.ethz.spirals.datatypes.UnstagedImplicitOps._
      import ElementOpsUnstaged._
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = 4

      implicit val aops: ArrayOps[newIR.Rep,Vector,newIR.Rep,Double] = new newIR.VectorArrayOps[Double]
      implicit val nrep: NumericOps[newIR.Rep[Double]] =  new newIR.StagedNumericOps.NumericRepOps[Double]
      implicit val erep: ElementOps[Complex,newIR.Rep[Double]] = new ElementOpsUnstaged.ComplexOps[newIR.Rep[Double]]()


      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[newIR.Rep, ElementOpsUnstaged.Complex,newIR.Rep,Double]{
          override val IR: self.type = self
          override val targetIR: newIR.type = newIR
        }

      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      //toplevelf(input)


      implicit def exposeRepFromCVector(size: Int)(implicit tag: Manifest[Double]): newIR.ExposeRep[CVector[Rep, ElementOpsUnstaged.Complex,emitGraphScala.IR.Rep,Double]]
      = new newIR.ExposeRep[CVector[Rep, ElementOpsUnstaged.Complex,emitGraphScala.IR.Rep,Double]] (){
        import newIR._
        val freshExps: (Unit => Vector[newIR.Exp[_]]) = (u: Unit) => {
          //val t = for(i <- 0 until size) yield Vector(newIR.Arg[Double](newIR.convertFromManifest(tag)), newIR.Arg[Double](newIR.convertFromManifest(tag)))
          //t.foldLeft(Vector.empty[newIR.Exp[_]])((acc,ele) => { acc ++ ele })
          Vector(newIR.Arg(Vector[Complex[Double]]))
        }
        val vec2t: (Vector[newIR.Exp[_]] => CVector[Rep, ElementOpsUnstaged.Complex,newIR.Rep,Double]) = (v: Vector[newIR.Exp[_]]) => {
          val x = for (i <- 0 until size) yield{
            val re: newIR.Exp[Double] = v(i*2).asInstanceOf[newIR.Exp[Double]] //RF: use the Manifest to make this safe
            val im: newIR.Exp[Double] = v(i*2+1).asInstanceOf[newIR.Exp[Double]]
            Complex(re,im)
          }
          val t = new SplitComplexArray[Rep, Vector, newIR.Rep, Double](size)
          val r: CVector[Rep, ElementOpsUnstaged.Complex,newIR.Rep,Double] = t.ini(x)
          r
        }
        val t2vec = (c: CVector[Rep, ElementOpsUnstaged.Complex,emitGraphScala.IR.Rep,Double]) => {
          val t = (for (i <- 0 until size) yield c(i)).toVector
          t.foldLeft(Vector.empty[emitGraphScala.IR.Rep[_]]){(acc,ele) => acc ++ Vector(ele._re,ele._im)}
        }
      }
      val expose = exposeRepFromCVector(4)

      val emitGraphScala = new GraphVizExport {
        override val IR: newIR.type = newIR
      }
      val (scalagraph, cm2) = {
        emitGraphScala.emitDepGraphf(toplevelf)(expose,expose)
      }
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("scala_scalar_SON.dot"))
      stream.println(scalagraph)
      stream.flush()
      stream.close()

      /*val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("source.txt"))
      val esc = newIR.codegen.emitSource(toplevelf,"testClass",stream2)(expose,expose)
      stream2.flush()
      stream2.close()
*/
      println("---")
    }

  }


  val myprog = new MyDSLProgram

  //myprog.myf()

  myprog.exportgraph()
  myprog.createMat()
  myprog.createScalaCode()
  myprog.createStagedScalaCode()
  //myprog.createParamizedNonStagedScalaCode()
  //myprog.createParamizedStagedScalarScalaCode()
  myprog.createParamizedStagedScalarScalaCode()
}