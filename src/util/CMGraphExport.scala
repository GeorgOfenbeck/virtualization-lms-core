package util

import java.io.{StringWriter, PrintWriter}

import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.internal.{CodeMotion, Expressions}

/**
 * Georg Ofenbeck
 First created:
 * Date: 27/11/2014
 * Time: 11:14 
 */
trait CMGraphExport {
  val CM: CodeMotion
  import CM._
  import CM.reifiedIR.IR._
  var stream: PrintWriter = _



  def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try { body } finally { stream.flush; stream = save }
  }

  var edgestream: PrintWriter = null
  def quote(x: Any) = "\""+x+"\""

  def emitNode(sym: Exp[_], rhs: Def[Any]):Unit = {
    stream.println(quote(sym) + " [")
    stream.println("label=" + quote(sym + " \\n " + rhs))
    stream.println("shape=box")
    stream.println("]")
    //val deps = syms(rhs)
    val depsout = enriched_graph(sym.id).out


    edgestream.println("edge[style=bold];")
    for (dep <- depsout) {
      edgestream.println("\""+ sym + "\" -> \"Sym(" + dep + ")\"")
    }
    edgestream.println("edge[style=dotted];")
    val depsin = enriched_graph(sym.id).in
    for (dep <- depsin) {
      //stream.println("\"Sym(" + dep + ")\" -> \"" + sym + "\"")
      edgestream.println("\""+ sym + "\" -> \"Sym(" + dep + ")\"")
    }



    blocks(rhs) foreach { block =>
/*
      val resid: Int = block match {
        case Block(Sym(id)) => id
        case Block(Const(x)) => {
          println(block)
          ??? //we never have this in our code - //TODO - fix for common case
        }
      }
*/


      if (block_cache.blockinfo.contains(block)) // we have a block
      {
        stream.println("subgraph cluster_"+ sym.id + " {")
        stream.println("style=filled;")

        traverseStmsinBlock(block_cache.blockinfo(block))
        stream.println("}")
      }
      else
        ???


    }
  }

  def traverseStmsinBlock(binfo: BlockInfo) = {

    if (binfo.child_schedule.size > 1){
      stream.println("edge[style=dashed];")
      for (i <- 1 until binfo.child_schedule.size){
        val n1 = binfo.child_schedule(i-1)
        val n2 = binfo.child_schedule(i)
        stream.println("\"Sym(" + n1 + ")\" -> \"Sym(" + n2 + ")\"")
      }
    }
    binfo.child_schedule.foreach( stmid => {
      val node = enriched_graph(stmid)
      node.irdef map (id => exp2tp.get(Exp(id)).map(tp => traverseStm(tp)))
    })
  }


  def emitDepGraph( out: PrintWriter) {
    val block = CM.reifiedIR.rootblock
    val stringOutput = new StringWriter()
    val stringWriter = new PrintWriter(stringOutput)
    val stringOutput_edges = new StringWriter()
    edgestream = new PrintWriter(stringOutput_edges)
    withStream(stringWriter) {
      stream.println("digraph G {")
      //traverseBlock(block)
      val binfo = CM.block_cache.blockinfo(block)

      traverseStmsinBlock(binfo)
      //stream.println("}")
    }
    edgestream.println("}")

    edgestream.flush();
    out.write(stringOutput.toString);
    out.append(stringOutput_edges.toString);

    out.flush()
    out.close()

  }

  def traverseStm(stm: TP[_]) = {

    stm match {
      case TP(sym, rhs,tag) =>  emitNode(sym, rhs)
      case _ => ???
    }
  }
}




