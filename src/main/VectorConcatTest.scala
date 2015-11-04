/**
 * Georg Ofenbeck
 First created:
 * Date: 04/11/2015
 * Time: 15:38 
 */
object VectorConcatTest extends App{


 val deepth = 17



 {
  val stream = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Filespeedtest0.txt")
  val writer = new java.io.PrintWriter(stream)
  val start = System.nanoTime()
  def treerecurse(deepth: Int, idx: Int): (Vector[String],Int) = {
   if (deepth > 1)
   {
    val (l,idx1) = treerecurse(deepth - 1, idx)
    val (r,idx2) = treerecurse(deepth - 1, idx1)
    (l ++ r, idx2)
   }
   else
    (Vector("this is my teststring" + idx),idx + 1)
  }
  val (resv, residx) = treerecurse(deepth, 0)
  resv.map(e => writer.print(e))
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
  val sb = new StringBuilder

  val stream = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Filespeedtest1.txt")
  val writer = new java.io.PrintWriter(stream)
  def treerecurse(deepth: Int, idx: Int): Int = {
   if (deepth > 1)
   {
    val idx1 = treerecurse(deepth - 1, idx)
    val idx2 = treerecurse(deepth - 1, idx1)
    idx2
   }
   else
    writer.print("this is my teststring" + idx)
    idx + 1
  }
  val residx = treerecurse(deepth, 0)
  writer.flush()
  writer.close()
  stream.flush()
  stream.close()
  val stop = System.nanoTime()
  val time = (stop - start) / 1000000000.0;
  println(time)
 }

 {
  val stream = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Filespeedtest0.txt")
  val writer = new java.io.PrintWriter(stream)
  val start = System.nanoTime()
  def treerecurse(deepth: Int, idx: Int): (Vector[String],Int) = {
   if (deepth > 1)
   {
    val (l,idx1) = treerecurse(deepth - 1, idx)
    val (r,idx2) = treerecurse(deepth - 1, idx1)
    (l ++ r, idx2)
   }
   else
    (Vector("this is my teststring" + idx),idx + 1)
  }
  val (resv, residx) = treerecurse(deepth, 0)
  resv.map(e => writer.print(e))
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
  val sb = new StringBuilder

  val stream = new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\Filespeedtest1.txt")
  val writer = new java.io.PrintWriter(stream)
  def treerecurse(deepth: Int, idx: Int): Int = {
   if (deepth > 1)
   {
    val idx1 = treerecurse(deepth - 1, idx)
    val idx2 = treerecurse(deepth - 1, idx1)
    idx2
   }
   else
    writer.print("this is my teststring" + idx)
   idx + 1
  }
  val residx = treerecurse(deepth, 0)
  writer.flush()
  writer.close()
  stream.flush()
  stream.close()
  val stop = System.nanoTime()
  val time = (stop - start) / 1000000000.0;
  println(time)
 }

}
