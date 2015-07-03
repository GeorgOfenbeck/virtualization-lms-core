package scala.lms
package targets
package scalalike

import java.io.PrintWriter

import scala.lms.internal._


trait ScalaCodegen extends GenericCodegen with Config {
  self =>
  val IR: BaseExp with InternalFunctionsExp
  import IR._

  var className: String = ""


  def emitSource[A,R](
                      f: Function1[A,R],
                      className: String,
                      out: PrintWriter)(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]) = {
    self.className = className //RF!
    /*if (emitString.IR == null)
      assert(false, "wtf?")*/
    val (source, esc) = self.emit("",f)(args, returns)
    out.println(source)
    out.flush()
    esc
  }


  def emitValDef(tp: TP[_], rhs: String): String = {
    val extra = if ((sourceinfo < 2) || tp.sym.pos.isEmpty) "" else {
      val context = tp.sym.pos(0)
      "      // " + relativePath(context.fileName) + ":" + context.line
    }
    "val " + quote(tp) + " = " + rhs + extra + "\n"
  }

  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
}


trait EmitHeadInternalFunctionAsClass extends ScalaCodegen {
  self =>

  var head: IR.TP[_] = null
  val staticData = Vector.empty[(IR.TP[_],Any)]
  var className: String

  override def emitNode(tp: self.IR.TP[_], acc: String,
               block_callback: (self.IR.Block,String) => String): String = tp.rhs match {
    case IR.InternalLambda(f,x,y,args,returns) => {
      if (head == null || head == tp) {
        head = tp
        if (y.res.size > 1)
          assert(false, "still need to implement multiy result unparsing")
        val stringheader =
          "/*****************************************\n"+
            "  Emitting Generated Code                  \n"+
            "*******************************************/\n" +
            "class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tag).mkString(",")+")")+
            " extends (("+x.map(a => remap(a.tag.mf)).mkString(", ")+")=>("+y.res.map(a => remap(IR.exp2tp(a).tag.mf)).mkString(", ") + ")) {" +
            "\ndef apply("+x.map(a => quote(a) + ":" + remap(a.tag.mf)).mkString(", ")+"): "+y.res.map(a => remap(IR.exp2tp(a).tag.mf)).mkString(", ")+" = {\n"

        val res = stringheader + block_callback(y,"") +
          "\n ("+ y.res.map(r => quote(r)).mkString(", ") +  ")\n" +
          "}" +
          "}" +
          "\n/*****************************************\n"+
          "  End of Generated Code                  \n"+
          "*******************************************/"
        res
      }
      else {
          "val " + quote(tp) + ": " +
          "(" + x.map(a => remap(a.tag.mf)).mkString(", ")+")=>("+y.res.map(a => remap(IR.exp2tp(a).tag.mf)).mkString(", ") + ") = " +
           "("+x.map(a => quote(a) + ":" + remap(a.tag.mf)).mkString(", ")+") =>{\n" +
          block_callback(y,"") +
          "\n ("+ y.res.map(r => quote(r)).mkString(", ") +  ")\n" +
          "}\n"

        //emitValDef(tp,string)
        //assert(false, "you are emitting code that has Internal Lambdas in the body - not handling this yet")
      }
    }
    case IR.InternalApply(f,arg,res) => {
      "val " + res.res.map(r => quote(r)).mkString(", ") + " = " + quote(f) + "(" + arg.map(r => quote(r)).mkString(", ") + ")\n"
    }
    case IR.ReturnArg(f,arg) => emitValDef(tp,quote(arg))
    case IR.ArgDef(id) => "" //args are handled in the according lambda
    case _ => super.emitNode(tp,acc,block_callback)
  }

}