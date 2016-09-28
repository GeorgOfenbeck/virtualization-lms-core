

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport


trait WHT_Exp extends PurePrimitiveOpsExp with ImplicitOpsExp with FunctionsExp{

}

object FirstRecurse extends App{

  class MyDSLProgram extends WHT_Exp{
    self =>

    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }




    def WHT(n: Rep[Int]): Rep[Int] = {

      def g(x: Rep[Int]): Rep[Int] = {
        val f: (Rep[Int] => Rep[Int]) =  (y: Rep[Int]) => {
          val sfg = doLambda(g,true, true)
          sfg(y)
        }
        val sf = doLambda(f,true,true)
        sf(x)
      }

      def a(x: Rep[Int]): Rep[Int] = {
        val sf = doLambda(b,false, true)
        sf(x)
      }

      def b(x: Rep[Int]): Rep[Int] = {
        val sf = doLambda(a,false, true)
        sf(x)
      }


      //val res = doLambda(WHT,true)
      //res(n)
      a(n)
    }


    def graphvizexport() = {
      val (code, cm) = emitGraph.emitDepGraphf(WHT)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("WHT_recursion_step.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }
  }

  val dslprogram = new MyDSLProgram
  dslprogram.graphvizexport()







}
