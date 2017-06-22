package Ackerman

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.InvocationTargetException

class Ackerman(n: Int,
                m: Int ) extends Core( n,m) {





  def graphexport(path: String = "C:\\Phd\\git\\code\\SpiralSTarget\\", name: String = "Graph.dot") = {
    this.graphname = true
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream(path + name))
    //dumpCode (stream2)


    val ingt = new Stat(if (n < 0) sph() else n, if(m < 0) sph() else m)
    val (str, esc) = emitGraph.emitDepGraphf(ini(ingt))(exposeDyn(ingt), exposeRepFromRep[Int])
    stream2.println(str)
    stream2.flush()
    stream2.close()
    this.graphname = false
  }

  def codeexport(path: String = "C:\\Phd\\git\\code\\SpiralSTarget\\src\\main\\Ackerman.scala") = {
    //def codeexport(path: String = "F:\\Phd\\git\\code\\SpiralSTarget\\src\\main\\Test.scala") = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream(path))

    val ingt = new Stat(if (n < 0) sph() else n, if(m < 0) sph() else m)
    val esc = codegen.emitSource((ini(ingt)), "testClass", stream2)(exposeDyn(ingt), exposeRepFromRep[Int])
    stream2.println("\n}\n")

    stream2.println("\n}}\n")
    stream2.flush()
    stream2.close()
    println("dump code")
    // graphexport()
  }
}
