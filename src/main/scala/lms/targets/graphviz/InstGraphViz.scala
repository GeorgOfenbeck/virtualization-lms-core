package scala.lms
package targets
package graphviz

import scala.lms.internal._

import scala.lms.internal.FunctionsExp
import scala.lms.ops._
import scala.lms.targets.scalalike._

trait MyRange extends PurePrimitiveOpsExp with FunctionsExp with ImplicitOpsExp {

 case class RangeMap[T: Manifest](start: Exp[Int], end: Exp[Int], body: Exp[_ => _]) extends Def[IndexedSeq[T]]

 def range_map[T](s: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[T])(implicit tr: TypeRep[T], mf: Manifest[T]): Exp[IndexedSeq[T]] = {
  val lambda = doInternalLambda(block)(exposeRepFromRep[Int], exposeRepFromRep[T])
  val cc = RangeMap[T](s, end, lambda.exp)
  toAtom(cc)
 }

 case class ExpensiveF(x: Exp[Int], y: Exp[Int]) extends Def[Int]
 def expensive_pure_f(x: Exp[Int], y: Exp[Int]): Exp[Int] = ExpensiveF(x,y)

}

trait ScalaGenMyRange extends ScalaCodegen with EmitHeadInternalFunctionAsClass {
 val IR: MyRange

 import IR._

 override def emitNode(tp: TP[_], acc: String,
                       block_callback: (Block, String) => String): String = {
  tp.rhs match {
   case ExpensiveF(lhs,rhs) =>  emitValDef(tp, src"expensivedef($lhs,$rhs)")
   case RangeMap(start, end, body) => {
    val thenlambda = exp2tp(body)
    val rets: String = (thenlambda.rhs) match {
     case (InternalLambda(tf, tx, ty, targs, treturns)) => {
      val l1 = "val " + quote(tp) + " = for (" + quote(tx.head) + " <- " + quote(start) + " until " + quote(end) + " ) yield {\n" //TODO: fix me!
      val l2 = block_callback(ty, l1)
      val trestuple: Vector[String] = ty.res.map(r => quote(r))
      val l3: String = l2 + tupledeclarehelper(trestuple, "")
      val l4 = l3 + "\n}"
      l4 + "\n"
     }
     case _ => {
      assert(false, "got an if statment which does not contain lambdas for its branches")
      ""
     }
    }
    rets
   }
   case _ => super.emitNode(tp, acc, block_callback)
  }
 }
}

trait InstGraphVizExport {
 self =>
 val IR: BaseExp with FunctionsExp with MyRange

 type specCM = CodeMotion {
  val reifiedIR: ReificationPure {
   val IR: self.IR.type
  }}

 def emitDepGraph(file: String, landscape: Boolean = false) : String = ???

 def quote(x: Any) = "\""+x+"\""

 def emitDepGraphf[A,R]( f: Function1[A,R])(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]): (String,specCM) = {
  val reify = new ReifyPure {
   override val IR: self.IR.type = self.IR
  }
  val reification = reify.reifyProgram(f)(args, returns)
  val cm: specCM = CodeMotion(reification)
  (emitDepGraph(cm),cm)
 }



 def emitDepGraph(cm: specCM): String = {


  def emitNode(node: cm.EnrichedGraphNode): String = {
   val tp = cm.reifiedIR.id2tp(node.irdef)

   val nodestring = tp.rhs match {
    case IR.ExternalLambda(f,x,y,args,returns) => {
     tp.sym.id + " [label=" + quote(tp.sym.id + "\n,shape=box]"
    }
    case IR.InternalLambda(f,x,y,a,r) => tp.sym.id + "[label=InternalF\n,shape=box]"
    case _ => tp.sym.id + " [label=" + quote(tp.sym.id + " \\n " + tp.rhs + "\\n" + tp.tag.mf.toString()) +
      "\n,shape=box]"
   }


   val sucessorstring = node.successors.foldLeft(""){
    (acc,ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"s\"] "
   }
   val predecessorstring = node.predecessors.foldLeft(""){
    (acc,ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"p\"] "
   }

   val blockresids: Set[Int] = node.blocks.flatMap(v => v.res.map(res => res.id))
   val blockdepstring = blockresids.foldLeft(""){
    (acc,ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"b\"] "
   }

   val blockCMcontained: Set[Vector[Int]] = node.blocks.map(b => IR.block2tps(b).map(tp => tp.sym.id))

   val blockCMdepstring: String  = blockCMcontained.map(b => b.foldLeft(""){
    (acc,ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"cm\"] "
   }).mkString("\n")




   nodestring + sucessorstring + predecessorstring + blockdepstring + blockCMdepstring

  }
  //we emit the head node and all blocks
  //this is assuming that head node is always a function and everything else is contained within
  val lamdbaid: Int = cm.block_cache3.root.sym.id
  val check = cm.enriched_graph
  val head = emitNode(check(lamdbaid))



  val graphstring = cm.block_cache3.blockinfo.foldLeft(Vector.empty[String])( (acc,ele) => {
   val (block, blockinfo) = ele
   val blockres = blockinfo.children.foldLeft(Vector.empty[String]) {
    (iacc,iele) => {
     val (id, node) = iele
     iacc :+ emitNode(node)
    }
   }
   acc ++ blockres
  })



  "digraph G {\n" + head + "\n" + graphstring.mkString("\n") + "\n}"
 }












}
