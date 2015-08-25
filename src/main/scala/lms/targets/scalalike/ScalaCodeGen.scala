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

  var head: IR.TP[_] = null //RF - get rid of the state!
  val staticData = Vector.empty[(IR.TP[_],Any)]
  var className: String

  val tuplesize = 21

  override def emitSource[A,R](
                       f: Function1[A,R],
                       className: String,
                       out: PrintWriter)(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]) = {

    val res = super.emitSource(f,className,out)(args,returns)
    head = null
    res
  }

  def tupleaccesshelper(pos: Int, acc: String, lastele: Boolean): String = {
    if (pos < tuplesize)
      if (pos == 0 && lastele)
        acc //its not a tuple
      else
        acc + "._" + (pos + 1).toInt
    else{
      tupleaccesshelper(pos - tuplesize,acc + "._" + (tuplesize+1), lastele)
    }
  }
  def tupledeclarehelper(rest: Vector[String], acc: String): String = {
    if (rest.size <= tuplesize)
      acc + "(" + rest.mkString(",") + ")"
    else
    {
      val start = acc + "(" + rest.take(tuplesize).mkString(",") + ","
      tupledeclarehelper(rest.drop(tuplesize),start) + ")"
    }
  }


  def myhelp(tp: IR.TP[_]): String = {
    val rm:String = tp.rhs match {


      case IR.InternalLambda(f,x,y,args,returns) => {
        val av: Vector[String] = x.map(ele => remap(ele.tag.mf))
        val rv: Vector[String] = y.res.map(ele => myhelp(IR.exp2tp(ele)))
        val a = tupledeclarehelper(av, "")
        val r = tupledeclarehelper(rv, "")
        println("................")
        println(a + r)
        println("................")
        "scala.Function1[" + a + "," + r + "]"
      }
      case IR.ReturnArg(f,newsym,pos,tuple,last) => {
        if (tp.tag.toString.contains("Function"))
          println(".....")
        val newsymtp = IR.exp2tp(newsym)
        val applynodetp = IR.exp2tp(f)
        myhelp(newsymtp)
      }
      case IR.ArgDef(id) => {
        if (tp.tag.toString.contains("Function")){
          val funexp = tp.sym
          val aro = IR.funexp2StagedFunction.get(funexp)
          aro match{
            case Some(sf) =>{
              val args = sf.args.freshExps()
              val returns = sf.returns.freshExps()
              val av: Vector[String] = args.map(ele => myhelp(IR.exp2tp(ele)))
              val rv: Vector[String] = returns.map(ele => myhelp(IR.exp2tp(ele)))
              val a = tupledeclarehelper(av, "")
              val r = tupledeclarehelper(rv, "")
              "scala.Function1[" + a + "," + r + "]"
            }
            case None => {
              //val otp = IR.id2tp(id)
              println("Function - but not found in map")
              val r = myhelp(IR.id2tp(id))
              println(r)
              r
              //remap(tp.tag.mf)
            }

          }
        }
        else
          remap(tp.tag.mf)

      }



      case _ => {
        remap(tp.tag.mf)
      }
    }
    rm
  }

  override def emitNode(tp: self.IR.TP[_], acc: String,
               block_callback: (self.IR.Block,String) => String): String = tp.rhs match {
    case IR.InternalLambda(f,x,y,args,returns) => {
      val returntuple = tupledeclarehelper(y.res.map(a => myhelp(IR.exp2tp(a)) ),"")
      val restuple: Vector[String] = y.res.map(r => quote(r))
      val helper = if (x.size > 1) {
        x.zipWithIndex.map(a => {
          val (tp,index) = a
          val typ = remap(tp.tag.mf)
          "val " + quote(tp) + " : " + myhelp(tp) + " = helper" + tupleaccesshelper(index,"",index == x.size-1)
        }).mkString("\n")
      } else {
        //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
        "val " + quote(x.head) + " : " + myhelp(x.head) + " = helper\n"
      }
      val argtuple = tupledeclarehelper(x.map(a => myhelp(a)),"")

      if (head == null || head == tp) {
        head = tp
        /*if (y.res.size > 1)
          assert(false, "still need to implement multiy result unparsing")*/



        val stringheader =
          "/*****************************************\n"+
            "  Emitting Generated Code                  \n"+
            "*******************************************/\n" +
            "class "+className+(if (staticData.isEmpty) "" else "("+staticData.map(p=>"p"+quote(p._1)+":"+p._1.tag).mkString(",")+")")+
            " extends (("+ argtuple +")=>" + returntuple + ") {" +
            //"\ndef apply("+x.map(a => quote(a) + ":" + remap(a.tag.mf)).mkString(", ")+"): ("+returntuple+") = {\n"
            "\ndef apply( helper: "+ argtuple +"): ("+returntuple+") = {\n" + helper + "\n"


        val res = stringheader + block_callback(y,"") +
          "\n "+ tupledeclarehelper(restuple,"") +  "\n" +
          "}" +
          "}" +
          "\n/*****************************************\n"+
          "  End of Generated Code                  \n"+
          "*******************************************/"
        res
      }
      else {
          "val " + quote(tp) + ": " +
          "("+ argtuple +") => " + returntuple + " = " +
           "(helper: "+ argtuple+") =>{\n" + helper + "\n" +
          block_callback(y,"") +
            "\n "+ tupledeclarehelper(restuple,"") +  "\n" +
          "}\n"

        //emitValDef(tp,string)
        //assert(false, "you are emitting code that has Internal Lambdas in the body - not handling this yet")
      }
    }
    case IR.InternalApply(f,arg) => {
      //"val " + res.res.map(r => quote(r)).mkString(", ") + " = " + quote(f) + "(" + arg.map(r => quote(r)).mkString(", ") + ")\n"
      emitValDef(tp, " " + quote(f) + "(" + arg.map(r => quote(r)).mkString(", ") + ")\n")
    }
    case IR.ReturnArg(f,sym,posx,tuple,last) => {
      if (tuple) {
        //emitValDef(tp, quote(f) + "._" + (pos + 1).toInt)
        val start = "val " + quote(tp) + " = " + quote(f)
        tupleaccesshelper(posx,start,last) + "//returnarg  " + last + "\n"//RF
      }
      else
        emitValDef(tp,quote(f))
    }
    case IR.ArgDef(id) => {
      ""
    } //args are handled in the according lambda
    case IR.ConstDef(x) => "" //are handeled through remaps
    case _ => super.emitNode(tp,acc,block_callback)
  }

}