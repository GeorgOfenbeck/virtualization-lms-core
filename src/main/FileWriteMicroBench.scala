/**
 * Georg Ofenbeck
 First created:
 * Date: 22/10/2015
 * Time: 14:01 
 */
object FileWriteMicroBench extends App{










  {
   val start = System.nanoTime()
   val stream = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Filespeedtest.txt")
   val writer = new java.io.PrintWriter(stream)
   val t: Vector[String] = (0 until 500000).foldLeft(Vector.empty[String]) {
    (acc, ele) => {
     acc ++ Vector("lkdfsjalfkjsalkfjlksjglkjdglkhjlkghdlkhg" + ele)
    }
   }
   t.map(e => writer.print(e))
   writer.flush()
   writer.close()
   stream.flush()
   stream.close()
   val stop = System.nanoTime()
   val time = (stop - start) / 1000000000.0;
   println(time)
  }
  {
   val start = System.nanoTime()
   val stream = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Filespeedtest1.txt")
   val writer = new java.io.PrintWriter(stream)
   val t: Vector[String] = (0 until 500000).foldLeft(Vector.empty[String]) {
    (acc, ele) => {
     acc :+ ("lkdfsjalfkjsalkfjlksjglkjdglkhjlkghdlkhg" + ele)
    }
   }
   t.map(e => writer.print(e))
   writer.flush()
   writer.close()
   stream.flush()
   stream.close()
   val stop = System.nanoTime()
   val time = (stop - start) / 1000000000.0;
   println(time)
  }











}
