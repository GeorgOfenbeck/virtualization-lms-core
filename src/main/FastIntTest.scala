import java.math.BigInteger

/**
  * Created by rayda on 21-Oct-16.
  */
object FastIntTest extends App {

  val l: Long = Long.MaxValue

  val bl: BigInt = BigInt.apply(l)
  val rl: BigInt = bl*bl*bl*bl*bl*bl*bl;
  val ll: BigInt = bl*3*bl-bl*bl*bl;

  val res = rl * ll
  println(res)
  println("finished")


}
