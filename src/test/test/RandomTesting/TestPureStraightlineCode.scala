
package RandomTesting



import java.io.{PrintWriter, StringWriter}

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._
import scala.reflect.Manifest
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util


object TestPureStraightlineCode  extends RandomTester{

 class MRandomClass extends RandomClass
 with BooleanOpsExp
 with FunctionsExp
 with GenRandomBooleanOps
 { self =>
  override val codegen = new EmitHeadInternalFunctionAsClass with ScalaGenBooleanOps {
   val IR: self.type = self
  }
  val emitGraph = new GraphVizExport {
   override val IR: self.type = self
  }
 }

  def getCodeDescription(randomClass: RandomClass) = {
    randomClass.CodeDescriptor(100, 2, 20, 5, 20, 20 ,1 ,1 ,Map.empty)
  }
  //override lazy val desc: CodeDescriptor = CodeDescriptor(100, 2, 20, 5, 20, 20 , 1)


  def iniRandomC(): RandomClass = new MRandomClass()


}

