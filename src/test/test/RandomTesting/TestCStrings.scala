/*


package RandomTesting


import java.io.{PrintWriter, StringWriter}

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.clike._
import scala.reflect.Manifest
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util


object TestCStrings extends RandomTester {

  class MRandomClass extends RandomClass
    with BooleanOpsExp
    with PurePrimitiveOpsExpOpt
    with StringOpsExp
    with ImplicitOpsExp
    with FunctionsExp
    //with GenRandomBooleanOps
    with GenRandomPrimitiveOps
  {
    self =>
    override val codegen = new EmitHeadInternalFunctionAsCMain with scala.lms.targets.clike.CLikeGenPrimitiveOps  {
      val IR: self.type = self
    }
  }

  def getCodeDescription(randomClass: RandomClass) = randomClass.CodeDescriptor(
    max_nodes_per_block = 5,
    max_toplevel_args = 2,
    max_args = 3,
    max_nest_depth = 0,
    max_functions = 0,
    max_returns = 20, //ignored atm
    max_calls = 1,
    max_dynamic_calls = 1,
    max_opset_calls = Map.empty
  )

  def iniRandomC(): RandomClass = new MRandomClass()
}


*/
