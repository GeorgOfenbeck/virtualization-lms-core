package Karatsuba


case class MyBigInt(val mag: Array[Int], val signum: Int) {
  /**
    * Throws an {@code ArithmeticException} if the {@code BigInteger} would be
    * out of the supported range.
    *
    * @throws ArithmeticException if { @code this} exceeds the supported range.
    */
  def checkRange() {
    if (mag.length > MyBigInt.MAX_MAG_LENGTH || mag.length == MyBigInt.MAX_MAG_LENGTH && mag(0) < 0) {
      //reportOverflow();
    }
  }
}

object MyBigInt {

  /**
    * The threshold value for using squaring code to perform multiplication
    * of a {@code BigInteger} instance by itself.  If the number of ints in
    * the number are larger than this value, {@code multiply(this)} will
    * return {@code square()}.
    */
  val MULTIPLY_SQUARE_THRESHOLD = 20;
  val ZERO: MyBigInt = MyBigInt(new Array[Int](0), 0);
  val LONG_MASK: Long = 0xffffffffL;
  val MAX_MAG_LENGTH = Integer.MAX_VALUE / Integer.SIZE + 1;
  val KARATSUBA_THRESHOLD = 80
  // (1 << 26)


  /**
    * Returns a copy of the input array stripped of any leading zero bytes.
    */
  def stripLeadingZeroBytes(a: Array[Byte]): Array[Int] = {
    var byteLength = a.length;
    var keep = 0;

    // Find first nonzero byte
    while (keep < byteLength && a(keep) == 0)
      keep = keep + 1


    // Allocate new array and copy relevant part of input array
    var intLength = ((byteLength - keep) + 3) >>> 2;
    val result = new Array[Int](intLength);
    var b = byteLength - 1;
    var i = intLength - 1;
    while (i >= 0) {
      result(i) = a(b) & 0xff;
      b = b - 1
      val bytesRemaining = b - keep + 1;
      val bytesToTransfer = Math.min(3, bytesRemaining);
      var j = 8;
      while (j <= (bytesToTransfer << 3)) {
        j = j + 8
        result(i) |= ((a(b) & 0xff) << j);
        b = b - 1
      }
      i = i - 1
    }
    return result;
  }

  /**
    * Takes an array a representing a negative 2's-complement number and
    * returns the minimal (no leading zero bytes) unsigned whose value is -a.
    */
  def makePositive(a: Array[Byte]): Array[Int] = {
    var keep: Int = 0
    var k: Int = 0
    val byteLength: Int = a.length;

    // Find first non-sign (0xff) byte of input

    while (keep < byteLength && a(keep) == -1)
      keep = keep + 1

    /* Allocate output array.  If all non-sign bytes are 0x00, we must
     * allocate space for one extra output byte. */
    k = keep
    while (k < byteLength && a(k) == 0)
      k = k + 1

    val extraByte: Int = if (k == byteLength) 1 else 0;
    val intLength: Int = ((byteLength - keep + extraByte) + 3) >>> 2;
    val result: Array[Int] = new Array[Int](intLength);

    /* Copy one's complement of input into output, leaving extra
     * byte (if it exists) == 0x00 */
    var b: Int = byteLength - 1;
    var i = intLength - 1

    while (i >= 0) {

      result(i) = a(b) & 0xff; //b--
      b = b - 1
      var numBytesToTransfer: Int = Math.min(3, b - keep + 1);
      if (numBytesToTransfer < 0)
        numBytesToTransfer = 0;
      var j: Int = 8
      while (j <= 8 * numBytesToTransfer) {
        result(i) |= ((a(b) & 0xff) << j);
        b = b - 1
        j += 8
      }

      // Mask indicates which bits must be complemented
      val mask: Int = -1 >>> (8 * (3 - numBytesToTransfer));
      result(i) = ~result(i) & mask;
      i = i - 1
    }

    // Add one to one's complement to generate two's complement
    i = result.length - 1
    var cont = true //
    while (i >= 0 && cont) {
      result(i) = ((result(i) & LONG_MASK) + 1).toInt;
      if (result(i) != 0)
        cont = false
      else
        i = i - 1
    }

    return result;
  }

  def apply(barray: Array[Byte]): MyBigInt = {
    if (barray.length == 0)
      throw new NumberFormatException("Zero length BigInteger");

    val (mag, signum) = if (barray(0) < 0) {
      (makePositive(barray), -1)
    } else {
      val mag = stripLeadingZeroBytes(barray);
      val signum = if (mag.length == 0) 0 else 1
      (mag, signum)
    }
    if (mag.length >= MAX_MAG_LENGTH) {
      //checkRange();
    }
    MyBigInt(mag, signum)
  }
}


object VanilaKaratsuba extends App {


  def multiply(me: MyBigInt, that: MyBigInt): MyBigInt = {
    import me._
    import MyBigInt._
    if (that.signum == 0 || signum == 0)
      return ZERO;

    val xlen: Int = me.mag.length;

    if (that == this && xlen > MULTIPLY_SQUARE_THRESHOLD) {
      return ??? //square();
    }

    val ylen = that.mag.length;

    if ((xlen < KARATSUBA_THRESHOLD) || (ylen < KARATSUBA_THRESHOLD)) {
      val resultSign = if (signum == that.signum) 1 else -1;
      if (that.mag.length == 1) {
        return ??? //multiplyByInt(mag, that.mag[ 0], resultSign);
      }
      if (mag.length == 1) {
        return ??? //multiplyByInt(that.mag, mag[ 0], resultSign);
      }
      result = multiplyToLen(mag, xlen, that.mag, ylen, null);
      result = trustedStripLeadingZeroInts(result);
      return new BigInteger(result, resultSign);
    } else {
      if ((xlen < TOOM_COOK_THRESHOLD) && (ylen < TOOM_COOK_THRESHOLD)) {
        return multiplyKaratsuba(this, that);
      } else {
        return multiplyToomCook3(this, that);
      }
    }
  }
}

def convert (before: java.math.BigInteger): MyBigInt = {
  MyBigInt (before.toByteArray)
}

  def multiply (me: java.math.BigInteger, that: java.math.BigInteger): java.math.BigInteger = {
  MyBigInt (me.toByteArray)

  import me._
  import java.math.BigInteger._

  if (that.signum == 0 || signum == 0)
  return java.math.BigInteger.ZERO;

  me
  /*
      val bi = BigInt.apply(me)
      bi.toByteArray

      me.signum()

      val xlen: Int = me.mag.length;

      if (that == this && xlen > MULTIPLY_SQUARE_THRESHOLD) {
        return square();
      }

      int ylen = that.mag.length;

      if ((xlen < KARATSUBA_THRESHOLD) || (ylen < KARATSUBA_THRESHOLD)) {
        int resultSign = signum == that.signum ? 1: - 1;
        if (that.mag.length == 1) {
          return multiplyByInt(mag, that.mag[ 0], resultSign);
        }
        if (mag.length == 1) {
          return multiplyByInt(that.mag, mag[ 0], resultSign);
        }
        int[] result = multiplyToLen(mag, xlen,
          that.mag, ylen, null);
        result = trustedStripLeadingZeroInts(result);
        return new BigInteger(result, resultSign);
      } else {
        if ((xlen < TOOM_COOK_THRESHOLD) && (ylen < TOOM_COOK_THRESHOLD)) {
          return multiplyKaratsuba(this, that);
        } else {
          return multiplyToomCook3(this, that);
        }
      }*/
}
}
