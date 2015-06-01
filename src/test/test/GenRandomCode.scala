import scala.lms.BaseExp
import scala.lms.internal._
import scala.lms.ops.BooleanOpsExp
import scala.lms.targets.scalalike.{ScalaGenBooleanOps, EmitHeadInternalFunctionAsClass, ScalaCodegen, ScalaCompile}


trait GenRandomCode extends ScalaCompile{
  self =>

  val IR: BaseExp with InternalFunctionsExp with BooleanOpsExp

  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenBooleanOps{
    val IR: self.IR.type = self.IR
  }


}



trait RandomOps {


  def registerOps() = {

  }
}
