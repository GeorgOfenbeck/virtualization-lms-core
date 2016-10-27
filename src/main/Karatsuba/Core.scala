package Karatsuba


import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class Core extends KaratsubaHeader {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with sort.ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }

  case class MaybeSFunction[A[_], B[_], AB[_], C[_], D[_], CD[_]](f: Either[StagedFunction[DynKaratsubaHeader[A, B, AB, C, D, CD], Rep[MyBigInt]] , (DynKaratsubaHeader[A, B, AB, C, D, CD] => Rep[MyBigInt])]) {
    def apply(dyn: DynKaratsubaHeader[A, B, AB, C, D, CD]): Rep[MyBigInt] = f.fold(fa => fa(dyn), fb => fb(dyn))
  }

  object MaybeSFunction {
    def apply[A[_], B[_], AB[_], C[_], D[_], CD[_]](f: StagedFunction[DynKaratsubaHeader[A, B, AB, C, D, CD], Rep[MyBigInt]]): MaybeSFunction[A, B, AB, C, D, CD] = MaybeSFunction(Left(f))

    def apply[A[_], B[_], AB[_], C[_], D[_], CD[_]](f: DynKaratsubaHeader[A, B, AB, C, D, CD] => Rep[MyBigInt]): MaybeSFunction[A, B, AB, C, D, CD] = MaybeSFunction(Right(f))
  }


  def multiplya[A[_], B[_], AB[_], C[_], D[_], CD[_]](stat: StatKaratsubaHeader[A, B, AB, C, D, CD]): (DynKaratsubaHeader[A, B, AB, C, D, CD] => Rep[MyBigInt]) = {
    val stageme: (DynKaratsubaHeader[A, B, AB, C, D, CD] => Rep[MyBigInt]) = (dyn: DynKaratsubaHeader[A, B, AB, C, D, CD]) => {
      val f = multiply(stat)
      f(dyn)
    }
    stageme
  }

  def multiply[A[_], B[_], AB[_], C[_], D[_], CD[_]](stat: StatKaratsubaHeader[A, B, AB, C, D, CD]): MaybeSFunction[A, B, AB, C, D, CD] = {

    val sstring = stat.genSig()
    val exposarg: ExposeRep[DynKaratsubaHeader[A, B, AB, C, D, CD]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[MyBigInt]
    val stageme: (DynKaratsubaHeader[A, B, AB, C, D, CD] => Rep[MyBigInt]) = (dyn: DynKaratsubaHeader[A, B, AB, C, D, CD]) => {
      val mix = MixKaratsubaHeader(stat, dyn)
      import mix._
      import ab1._

      implicit val exposeCDRep = new ExposeRep[CD[Int]]{
        val freshExps: Unit => Vector[Exp[_]] = (u: Unit) => Vector(Arg[Int])
        val vec2t: Vector[Exp[_]] => CD[Int] = (in: Vector[Exp[_]]) => in.head.asInstanceOf[CD[Int]]
        val t2vec: CD[Int] => Vector[Exp[_]] = (in: CD[Int]) => Vector(evcd.toRep(in))
      }
      val KARATSUBA_THRESHOLD = 80

      val r0: Rep[MyBigInt] = evcd._if(evcd.or(evcd.equiv(cd1.fromA(asignum), evcd.const(0)), evcd.equiv(cd1.fromB(bsignum), evcd.const(0))), {
        zeroBigInt()
      }, {
        // but square case here

        val r1: Rep[MyBigInt] = evab._if(
          evab.or(evab.less(alength, evab.const(KARATSUBA_THRESHOLD)), evab.less(blength, evab.const(KARATSUBA_THRESHOLD))), {
            val resultSign = evcd._if(evcd.equiv(cd1.fromA(asignum),cd1.fromB(bsignum)), evcd.const(1), evcd.const(-1))
            val mag1 = BigIntMag(a)
            val mag2 = BigIntMag(b)
            evab._if(evab.equiv(blength, evab.const(1)), {
              //return multiplyByInt(mag, that.mag(0), resultSign)
              multiplyByInt(mag1,mag2,evcd.toRep(resultSign))
            }, {
              evab._if(evab.equiv(alength, evab.const(1)), {
                multiplyByInt(mag2,mag1,evcd.toRep(resultSign))
              }, {
                //var result: Array[Int] = multiplyToLen(mag, xlen, that.mag, ylen, null);
                val result = multiplyToLen(mag1,eva.toRep(alength),mag2,evb.toRep(blength))
                trustedStrip(result,evcd.toRep(resultSign))
              })
            })
          }, {
            val kf = multiplyKaratsuba(stat)
            kf(dyn)
          }
        )
        r1
      })
      r0
    }
    if (stat.inlineinfo.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynKaratsubaHeader[A, B, AB, C, D, CD], Rep[MyBigInt]] = doGlobalLambda(stageme, Some("multiply" + stat.genSig()), Some("multiply"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def multiplyKaratsuba[A[_], B[_], AB[_], C[_], D[_], CD[_]](stat: StatKaratsubaHeader[A, B, AB, C, D, CD]): MaybeSFunction[A, B, AB, C, D, CD] = {

    val sstring = stat.genSig()
    val exposarg: ExposeRep[DynKaratsubaHeader[A, B, AB, C, D, CD]] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[MyBigInt]
    val stageme: (DynKaratsubaHeader[A, B, AB, C, D, CD] => Rep[MyBigInt]) = (dyn: DynKaratsubaHeader[A, B, AB, C, D, CD]) => {
      val mix = MixKaratsubaHeader(stat, dyn)
      import mix._
      import ab1._

      val max = evab.max(ab1.fromA(alength), ab1.fromB(blength))
      val half = evab.div(evab.plus(max,evab.const(1)),evab.const(2))

      val xl = getLower(a,evab.toRep(half))
      val xh = getUpper(a,evab.toRep(half))
      val yl = getLower(b,evab.toRep(half))
      val yh = getUpper(b,evab.toRep(half))

      val p1 = {
        val a = xh
        val b = yh
        val amag = mag(a)
        val bmag = mag(b)
        val alength = length(amag)
        val blength = length(bmag)
        val asignum = signum(a)
        val bsignum = signum(b)

        val mixr = helpme[Rep,Rep,Rep,Rep,Rep,Rep](a,b,alength,blength,asignum,bsignum,inlineInfo)
        val (statless, dynless) = mixr.split()
        val lessf = multiply(statless)
        val res = lessf(dynless)
        res
      }
      val p2 = {
        val a = xl
        val b = yl
        val amag = mag(a)
        val bmag = mag(b)
        val alength = length(amag)
        val blength = length(bmag)
        val asignum = signum(a)
        val bsignum = signum(b)
        val mixr = helpme[Rep,Rep,Rep,Rep,Rep,Rep](a,b,alength,blength,asignum,bsignum,inlineInfo)
        val (statless, dynless) = mixr.split()
        val lessf = multiply(statless)
        val res = lessf(dynless)
        res
      }

      val xhxl = add(xh,xl)
      val yhyl = add(xh,xl)

      val p3 = {
        val a = xhxl
        val b = yhyl
        val amag = mag(a)
        val bmag = mag(b)
        val alength = length(amag)
        val blength = length(bmag)
        val asignum = signum(a)
        val bsignum = signum(b)

        val mixr = helpme[Rep,Rep,Rep,Rep,Rep,Rep](a,b,alength,blength,asignum,bsignum,inlineInfo)
        val (statless, dynless) = mixr.split()
        val lessf = multiply(statless)
        val res = lessf(dynless)
        res
      }

      karatsuba_rest(p1,p2,p3,evab.toRep(half),evc.toRep(asignum),evd.toRep(bsignum))

    }
    if (stat.inlineinfo.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynKaratsubaHeader[A, B, AB, C, D, CD], Rep[MyBigInt]] = doGlobalLambda(stageme, Some("Karatsuba" + stat.genSig()), Some("Karatsuba"))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

      def multiplyByInt(x: Array[Int], y: Int, sign: Int): MyBigInt = ???

  def multiplyToLen(x: Array[Int], xlen: Int, y: Array[Int], ylen: Int): Array[Int] = ???




  /*def multiply_start(a: Rep[MyBigInt], b: Rep[MyBigInt]): Rep[MyBigInt] = {
    val amag = mag(a)
    val bmag = mag(b)
    val alength = length(amag)
    val blength = length(bmag)
    val asignum = signum(a)
    val bsignum = signum(b)

    val newmix = MixKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](a,b,alength,blength,asignum,bsignum)
  }*/


  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\TestKaratsuba.scala"))
    stream2.println(codefrag)
    val ini: StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep] = StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](Const(-1), Const(-1), Const(-1),Const(-1),InlineInfo(false, 3, true, false))
    val esc = codegen.emitSource(multiplya(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[MyBigInt])
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

  val codefrag: String = "package Karatsuba\n\nimport java.math.BigInteger\n\n\ncase class MyBigInt(val mag: Array[Int], val signum: Int) {\n  self =>\n\n  private var tfirstNonzeroIntNum = 0\n  private var tbitLength = 0\n\n  /**\n    * Package private method to return bit length for an integer.\n    */\n  def bitLengthForInt(n: Int): Int = {\n    return 32 - java.lang.Integer.numberOfLeadingZeros(n);\n  }\n\n  /**\n    * Returns the number of bits in the minimal two's-complement\n    * representation of this BigInteger, <i>excluding</i> a sign bit.\n    * For positive BigIntegers, this is equivalent to the number of bits in\n    * the ordinary binary representation.  (Computes\n    * {@code (ceil(log2(this < 0 ? -this : this+1)))}.)\n    *\n    * @return number of bits in the minimal two's-complement\n    *         representation of this BigInteger, <i>excluding</i> a sign bit.\n    */\n  def bitLength(): Int = {\n    var n = tbitLength - 1;\n    if (n == -1) {\n      // bitLength not initialized yet\n      var m = mag;\n      var len = m.length;\n      if (len == 0) {\n        n = 0; // offset by one to initialize\n      } else {\n        // Calculate the bit length of the magnitude\n        var magBitLength = ((len - 1) << 5) + bitLengthForInt(mag(0));\n        if (signum < 0) {\n          // Check if magnitude is a power of two\n          var pow2 = (Integer.bitCount(mag(0)) == 1);\n          var i = 1\n          while (i < len && pow2) {\n            pow2 = (mag(i) == 0);\n            i = i + 1\n          }\n\n          n = if (pow2) magBitLength - 1 else magBitLength\n        } else {\n          n = magBitLength;\n        }\n      }\n      tbitLength = n + 1;\n    }\n    return n;\n  }\n\n\n  /**\n    * Returns the index of the int that contains the first nonzero int in the\n    * little-endian binary representation of the magnitude (int 0 is the\n    * least significant). If the magnitude is zero, return value is undefined.\n    */\n  def firstNonzeroIntNum(): Int = {\n    var fn = tfirstNonzeroIntNum - 2;\n    if (fn == -2) {\n      // firstNonzeroIntNum not initialized yet\n      fn = 0;\n\n      // Search for the first nonzero int\n      var mlen = mag.length;\n      var i = mlen - 1\n\n      while (i >= 0 && mag(i) == 0) i = i - 1\n      fn = mlen - i - 1;\n      tfirstNonzeroIntNum = fn + 2; // offset by two to initialize\n    }\n    return fn;\n  }\n\n\n  /* Returns an int of sign bits */\n  def signInt(): Int = {\n    return if (signum < 0) -1 else 0\n  }\n\n  /**\n    * Returns the specified int of the little-endian two's complement\n    * representation (int 0 is the least significant).  The int number can\n    * be arbitrarily high (values are logically preceded by infinitely many\n    * sign ints).\n    */\n  def getInt(n: Int): Int = {\n    if (n < 0)\n      return 0;\n    if (n >= mag.length)\n      return signInt();\n\n    val magInt = mag(mag.length - n - 1);\n\n    return if (signum >= 0) magInt else if (n <= firstNonzeroIntNum()) -magInt else ~magInt\n  }\n\n  /**\n    * Returns a byte array containing the two's-complement\n    * representation of this BigInteger.  The byte array will be in\n    * <i>big-endian</i> byte-order: the most significant byte is in\n    * the zeroth element.  The array will contain the minimum number\n    * of bytes required to represent this BigInteger, including at\n    * least one sign bit, which is {@code (ceil((this.bitLength() +\n     * 1)/8))}.  (This representation is compatible with the\n    * {@link #BigInteger(byte[]) (byte[])} constructor.)\n    *\n    * @return a byte array containing the two's-complement representation of\n    *         this BigInteger.\n    * @see #BigInteger(byte[])\n    */\n  def toByteArray(): Array[Byte] = {\n    val byteLen: Int = bitLength() / 8 + 1;\n    val byteArray = new Array[Byte](byteLen);\n\n    var i = byteLen - 1\n    var bytesCopied = 4\n    var nextInt = 0\n    var intIndex = 0\n    while (i >= 0) {\n      if (bytesCopied == 4) {\n        nextInt = getInt(intIndex)\n        intIndex = intIndex + 1\n        bytesCopied = 1;\n      } else {\n        nextInt >>>= 8;\n        bytesCopied = bytesCopied + 1;\n      }\n      byteArray(i) = nextInt.toByte;\n      i = i - 1\n    }\n    return byteArray;\n  }\n\n  /**\n    * Returns a BigInteger whose value is {@code (this << n)}.\n    * The shift distance, {@code n}, may be negative, in which case\n    * this method performs a right shift.\n    * (Computes <tt>floor(this * 2<sup>n</sup>)</tt>.)\n    *\n    * @param  n shift distance, in bits.\n    * @return { @code this << n}\n    * @see #shiftRight\n    */\n  def shiftLeft(n: Int): MyBigInt = {\n    if (signum == 0)\n      return MyBigInt.ZERO;\n    if (n > 0) {\n      return MyBigInt(MyBigInt.shiftLeft(mag, n), signum);\n    } else if (n == 0) {\n      return this;\n    } else {\n      // Possible int overflow in (-n) is not a trouble,\n      // because shiftRightImpl considers its argument unsigned\n      return shiftRightImpl(-n);\n    }\n  }\n\n  /**\n    * Returns a BigInteger whose value is {@code (this >> n)}. The shift\n    * distance, {@code n}, is considered unsigned.\n    * (Computes <tt>floor(this * 2<sup>-n</sup>)</tt>.)\n    *\n    * @param  n unsigned shift distance, in bits.\n    * @return { @code this >> n}\n    */\n  def shiftRightImpl(n: Int): MyBigInt = {\n    import MyBigInt._\n    val nInts = n >>> 5;\n    val nBits = n & 0x1f;\n    val magLen = mag.length;\n    var newMag: Array[Int] = null;\n\n\n    // Special case: entire contents shifted off the end\n    if (nInts >= magLen)\n      return if (signum >= 0) ZERO else negConst(1)\n\n    if (nBits == 0) {\n      val newMagLen = magLen - nInts;\n      newMag = java.util.Arrays.copyOf(mag, newMagLen);\n    } else {\n      var i = 0;\n      var highBits: Int = mag(0) >>> nBits;\n      if (highBits != 0) {\n        newMag = new Array[Int](magLen - nInts)\n        newMag(i) = highBits;\n        i = i + 1\n      } else {\n        newMag = new Array[Int](magLen - nInts - 1)\n      }\n\n      var nBits2 = 32 - nBits;\n      var j = 0;\n      while (j < magLen - nInts - 1) {\n        newMag(i) = (mag(j) << nBits2) | (mag(j) >>> nBits);\n        i = i + 1\n        j = j + 1\n      }\n    }\n\n    if (signum < 0) {\n      // Find out whether any one-bits were shifted off the end.\n      var onesLost = false;\n      var i = magLen - 1\n      var j = magLen - nInts;\n      while (i >= j && !onesLost) {\n        onesLost = (mag(i) != 0);\n        i = i - 1\n      }\n      if (!onesLost && nBits != 0)\n        onesLost = (mag(magLen - nInts - 1) << (32 - nBits)) != 0\n\n      if (onesLost)\n        newMag = javaIncrement(newMag);\n    }\n\n    return new MyBigInt(newMag, signum);\n  }\n\n  def javaIncrement(jval: Array[Int]): Array[Int] = {\n    var lastSum = 0;\n    var i = jval.length - 1\n    while (i >= 0 && lastSum == 0)\n      i = i - 1\n    jval(i) = jval(i) + 1\n    lastSum = jval(i)\n\n    if (lastSum == 0) {\n      val njval = new Array[Int](jval.length + 1)\n      njval(0) = 1;\n      njval\n    } else jval\n\n  }\n\n\n  /**\n    * Throws an {@code ArithmeticException} if the {@code BigInteger} would be\n    * out of the supported range.\n    *\n    * @throws ArithmeticException if { @code this} exceeds the supported range.\n    */\n  def checkRange() {\n    if (mag.length > MyBigInt.MAX_MAG_LENGTH || mag.length == MyBigInt.MAX_MAG_LENGTH && mag(0) < 0) {\n      //reportOverflow();\n    }\n  }\n\n  /**\n    * Returns a BigInteger whose value is {@code (-this)}.\n    *\n    * @return { @code -this}\n    */\n  def negate(): MyBigInt = {\n    return new MyBigInt(this.mag, -this.signum);\n  }\n\n  /**\n    * Returns a BigInteger whose value is the absolute value of this\n    * BigInteger.\n    *\n    * @return { @code abs(this)}\n    */\n  def abs(): MyBigInt = {\n    return if (signum >= 0) this else this.negate()\n  }\n\n\n  /**\n    * Returns a new BigInteger representing n lower ints of the number.\n    * This is used by Karatsuba multiplication and Karatsuba squaring.\n    */\n  def getLower(n: Int): MyBigInt = {\n    val len = mag.length;\n\n    if (len <= n) {\n      return abs();\n    }\n\n    val lowerInts: Array[Int] = new Array[Int](n);\n    System.arraycopy(mag, len - n, lowerInts, 0, n);\n\n    return MyBigInt(MyBigInt.trustedStripLeadingZeroInts(lowerInts), 1);\n  }\n\n  /**\n    * Returns a new BigInteger representing mag.length-n upper\n    * ints of the number.  This is used by Karatsuba multiplication and\n    * Karatsuba squaring.\n    */\n  def getUpper(n: Int): MyBigInt = {\n    val len: Int = mag.length;\n\n    if (len <= n) {\n      return MyBigInt.ZERO;\n    }\n\n    val upperLen = len - n;\n    val upperInts: Array[Int] = new Array[Int](upperLen)\n    System.arraycopy(mag, 0, upperInts, 0, upperLen);\n\n    return MyBigInt(MyBigInt.trustedStripLeadingZeroInts(upperInts), 1);\n  }\n\n  def multiply(that: MyBigInt): MyBigInt = MyBigInt.multiply(this, that)\n\n  /**\n    * Adds the contents of the int arrays x and y. This method allocates\n    * a new int array to hold the answer and returns a reference to that\n    * array.\n    */\n  def add(xp: Array[Int], yp: Array[Int]): Array[Int] = {\n    import MyBigInt._\n    // If x is shorter, swap the two arrays\n    val (x, y) = if (xp.length < yp.length) (yp, xp) else (xp, yp)\n\n    var xIndex = x.length;\n    var yIndex = y.length;\n    var result: Array[Int] = new Array[Int](xIndex);\n    var sum: Long = 0;\n    if (yIndex == 1) {\n      sum = (x(xIndex) & LONG_MASK) + (y(0) & LONG_MASK);\n      xIndex = xIndex - 1\n      result(xIndex) = sum.toInt\n    } else {\n      // Add common parts of both numbers\n      while (yIndex > 0) {\n        xIndex = xIndex - 1\n        yIndex - yIndex - 1\n        sum = (x(xIndex) & LONG_MASK) +\n          (y(yIndex) & LONG_MASK) + (sum >>> 32);\n        result(xIndex) = sum.toInt;\n      }\n    }\n    // Copy remainder of longer number while carry propagation is required\n    var carry: Boolean = (sum >>> 32) != 0\n    while (xIndex > 0 && carry) {\n      xIndex = xIndex - 1\n      (result(xIndex) = x(xIndex) + 1)\n      carry = (result(xIndex) == 0)\n    }\n\n    // Copy remainder of longer number\n    while (xIndex > 0)\n      result(xIndex) = x(xIndex)\n\n    // Grow result if necessary\n    if (carry) {\n      val bigger: Array[Int] = new Array[Int](result.length + 1)\n      System.arraycopy(result, 0, bigger, 1, result.length);\n      bigger(0) = 0x01;\n      return bigger;\n    }\n    return result;\n  }\n\n  /**\n    * Subtracts the contents of the second int arrays (little) from the\n    * first (big).  The first int array (big) must represent a larger number\n    * than the second.  This method allocates the space necessary to hold the\n    * answer.\n    */\n  def subtract(big: Array[Int], little: Array[Int]): Array[Int] = {\n    import MyBigInt._\n    var bigIndex = big.length;\n    var result: Array[Int] = new Array[Int](bigIndex)\n    var littleIndex = little.length;\n    var difference: Long = 0;\n\n    // Subtract common parts of both numbers\n    while (littleIndex > 0) {\n      bigIndex = bigIndex - 1\n      littleIndex = littleIndex - 1\n      difference = (big(bigIndex) & LONG_MASK) -\n        (little(littleIndex) & LONG_MASK) +\n        (difference >> 32);\n      result(bigIndex) = difference.toInt\n    }\n\n    // Subtract remainder of longer number while borrow propagates\n    var borrow: Boolean = (difference >> 32) != 0\n    while (bigIndex > 0 && borrow) {\n      bigIndex = bigIndex - 1\n      (result(bigIndex) = big(bigIndex) - 1)\n      borrow = (result(bigIndex) == -1);\n    }\n\n    // Copy remainder of longer number\n    while (bigIndex > 0) {\n      result(bigIndex) = big(bigIndex)\n    }\n\n    return result;\n  }\n\n\n  /**\n    * Returns a BigInteger whose value is {@code (this + val)}.\n    *\n    * @param  val value to be added to this BigInteger.\n    * @return { @code this + val}\n    */\n  def add(jval: MyBigInt): MyBigInt = {\n    import MyBigInt._\n    if (jval.signum == 0)\n      return this;\n    if (signum == 0)\n      return jval;\n    if (jval.signum == signum)\n      return MyBigInt(add(mag, jval.mag), signum);\n\n    val cmp = compareMagnitude(jval);\n    if (cmp == 0)\n      return ZERO;\n    var resultMag: Array[Int] = if (cmp > 0) subtract(mag, jval.mag) else subtract(jval.mag, mag)\n    resultMag = trustedStripLeadingZeroInts(resultMag);\n\n    return new MyBigInt(resultMag, if (cmp == signum) 1 else -1)\n  }\n\n  /**\n    * Returns a BigInteger whose value is {@code (this - val)}.\n    *\n    * @param  val value to be subtracted from this BigInteger.\n    * @return { @code this - val}\n    */\n  def subtract(jval: MyBigInt): MyBigInt = {\n    if (jval.signum == 0)\n      return this;\n    if (signum == 0)\n      return jval.negate();\n    if (jval.signum != signum)\n      return MyBigInt(add(mag, jval.mag), signum);\n\n    var cmp = compareMagnitude(jval);\n    if (cmp == 0)\n      return MyBigInt.ZERO;\n    var resultMag = if (cmp > 0) subtract(mag, jval.mag) else subtract(jval.mag, mag)\n    resultMag = MyBigInt.trustedStripLeadingZeroInts(resultMag);\n    return new MyBigInt(resultMag, if (cmp == signum) 1 else -1)\n  }\n\n\n  /**\n    * Compares the magnitude array of this BigInteger with the specified\n    * BigInteger's. This is the version of compareTo ignoring sign.\n    *\n    * @param val BigInteger whose magnitude array to be compared.\n    * @return -1, 0 or 1 as this magnitude array is less than, equal to or\n    *         greater than the magnitude aray for the specified BigInteger's.\n    */\n  def compareMagnitude(jval: MyBigInt): Int = {\n    import MyBigInt._\n    val m1 = mag;\n    val len1 = m1.length;\n    val m2 = jval.mag;\n    val len2 = m2.length;\n    if (len1 < len2)\n      return -1;\n    if (len1 > len2)\n      return 1;\n    var i = 0;\n    var a = -1\n    var b = 1\n    while (i < len1 && a != b) {\n      a = m1(i)\n      b = m2(i)\n      i = i + 1\n    }\n    if (a != b)\n      if ((a & LONG_MASK) < (b & LONG_MASK)) -1 else 1\n    else return 0;\n  }\n\n}\n\nobject MyBigInt {\n\n  /**\n    * The threshold value for using squaring code to perform multiplication\n    * of a {@code BigInteger} instance by itself.  If the number of ints in\n    * the number are larger than this value, {@code multiply(this)} will\n    * return {@code square()}.\n    */\n  val MULTIPLY_SQUARE_THRESHOLD = 20;\n  val ZERO: MyBigInt = MyBigInt(new Array[Int](0), 0);\n  val LONG_MASK: Long = 0xffffffffL;\n  val MAX_MAG_LENGTH = Integer.MAX_VALUE / Integer.SIZE + 1;\n  /**\n    * Initialize static constant array when class is loaded.\n    */\n  val MAX_CONSTANT: Int = 16\n  val negConst: Array[MyBigInt] = new Array[MyBigInt](MAX_CONSTANT + 1)\n  val KARATSUBA_THRESHOLD = 80\n  // (1 << 26)\n\n\n  /**\n    * Returns a copy of the input array stripped of any leading zero bytes.\n    */\n  def stripLeadingZeroBytes(a: Array[Byte]): Array[Int] = {\n    var byteLength = a.length;\n    var keep = 0;\n\n    // Find first nonzero byte\n    while (keep < byteLength && a(keep) == 0)\n      keep = keep + 1\n\n\n    // Allocate new array and copy relevant part of input array\n    var intLength = ((byteLength - keep) + 3) >>> 2;\n    val result = new Array[Int](intLength);\n    var b = byteLength - 1;\n    var i = intLength - 1;\n    while (i >= 0) {\n      result(i) = a(b) & 0xff;\n      b = b - 1\n      val bytesRemaining = b - keep + 1;\n      val bytesToTransfer = Math.min(3, bytesRemaining);\n      var j = 8;\n      while (j <= (bytesToTransfer << 3)) {\n        result(i) |= ((a(b) & 0xff) << j);\n        j = j + 8\n        b = b - 1\n      }\n      i = i - 1\n    }\n    return result;\n  }\n\n  /**\n    * Takes an array a representing a negative 2's-complement number and\n    * returns the minimal (no leading zero bytes) unsigned whose value is -a.\n    */\n  def makePositive(a: Array[Byte]): Array[Int] = {\n    var keep: Int = 0\n    var k: Int = 0\n    val byteLength: Int = a.length;\n\n    // Find first non-sign (0xff) byte of input\n\n    while (keep < byteLength && a(keep) == -1)\n      keep = keep + 1\n\n    /* Allocate output array.  If all non-sign bytes are 0x00, we must\n     * allocate space for one extra output byte. */\n    k = keep\n    while (k < byteLength && a(k) == 0)\n      k = k + 1\n\n    val extraByte: Int = if (k == byteLength) 1 else 0;\n    val intLength: Int = ((byteLength - keep + extraByte) + 3) >>> 2;\n    val result: Array[Int] = new Array[Int](intLength);\n\n    /* Copy one's complement of input into output, leaving extra\n     * byte (if it exists) == 0x00 */\n    var b: Int = byteLength - 1;\n    var i = intLength - 1\n\n    while (i >= 0) {\n\n      result(i) = a(b) & 0xff; //b--\n      b = b - 1\n      var numBytesToTransfer: Int = Math.min(3, b - keep + 1);\n      if (numBytesToTransfer < 0)\n        numBytesToTransfer = 0;\n      var j: Int = 8\n      while (j <= 8 * numBytesToTransfer) {\n        result(i) |= ((a(b) & 0xff) << j);\n        b = b - 1\n        j += 8\n      }\n\n      // Mask indicates which bits must be complemented\n      val mask: Int = -1 >>> (8 * (3 - numBytesToTransfer));\n      result(i) = ~result(i) & mask;\n      i = i - 1\n    }\n\n    // Add one to one's complement to generate two's complement\n    i = result.length - 1\n    var cont = true //\n    while (i >= 0 && cont) {\n      result(i) = ((result(i) & LONG_MASK) + 1).toInt;\n      if (result(i) != 0)\n        cont = false\n      else\n        i = i - 1\n    }\n\n    return result;\n  }\n\n  def apply(barray: Array[Byte]): MyBigInt = {\n    if (barray.length == 0)\n      throw new NumberFormatException(\"Zero length BigInteger\");\n\n    val (mag, signum) = if (barray(0) < 0) {\n      (makePositive(barray), -1)\n    } else {\n      val mag = stripLeadingZeroBytes(barray);\n      val signum = if (mag.length == 0) 0 else 1\n      (mag, signum)\n    }\n    if (mag.length >= MAX_MAG_LENGTH) {\n      //checkRange();\n    }\n    MyBigInt(mag, signum)\n  }\n\n  /**\n    * Returns a magnitude array whose value is {@code (mag << n)}.\n    * The shift distance, {@code n}, is considered unnsigned.\n    * (Computes <tt>this * 2<sup>n</sup></tt>.)\n    *\n    * @param mag magnitude, the most-significant int ({ @code mag[0]}) must be non-zero.\n    * @param  n  unsigned shift distance, in bits.\n    * @return { @code mag << n}\n    */\n  def shiftLeft(mag: Array[Int], n: Int): Array[Int] = {\n    val nInts: Int = n >>> 5;\n    val nBits: Int = n & 0x1f;\n    val magLen: Int = mag.length;\n    var newMag: Array[Int] = null;\n\n    if (nBits == 0) {\n      newMag = new Array[Int](magLen + nInts);\n      System.arraycopy(mag, 0, newMag, 0, magLen);\n    } else {\n      var i = 0;\n      var nBits2: Int = 32 - nBits;\n      var highBits: Int = mag(0) >>> nBits2;\n      if (highBits != 0) {\n        newMag = new Array[Int](magLen + nInts + 1)\n        newMag(i) = highBits;\n        i = i + 1\n      } else {\n        newMag = new Array[Int](magLen + nInts)\n      }\n      var j = 0\n      while (j < magLen - 1) {\n        newMag(i) = mag(j) << nBits | mag(j) >>> nBits2;\n        i = i + 1\n        j = j + 1\n      }\n      newMag(i) = mag(j) << nBits;\n    }\n    return newMag;\n  }\n\n\n  def multiplyByInt(x: Array[Int], y: Int, sign: Int): MyBigInt = {\n    if (Integer.bitCount(y) == 1) {\n      val t = MyBigInt(shiftLeft(x, Integer.numberOfTrailingZeros(y)), sign);\n      return t\n    }\n    val xlen: Int = x.length;\n    var rmag: Array[Int] = new Array[Int](xlen + 1)\n    var carry: Long = 0;\n    var yl: Long = y & LONG_MASK;\n    var rstart: Int = rmag.length - 1;\n    var i: Int = xlen - 1\n    while (i >= 0) {\n      val product: Long = (x(i) & LONG_MASK) * yl + carry;\n      rmag(rstart) = product.toInt\n      rstart = rstart - 1\n      carry = product >>> 32;\n      i = i - 1\n    }\n    if (carry == 0L) {\n      rmag = java.util.Arrays.copyOfRange(rmag, 1, rmag.length);\n    } else {\n      rmag(rstart) = carry.toInt\n    }\n    return MyBigInt(rmag, sign);\n  }\n\n\n  /**\n    * Multiplies int arrays x and y to the specified lengths and places\n    * the result into z. There will be no leading zeros in the resultant array.\n    */\n  def multiplyToLen(x: Array[Int], xlen: Int, y: Array[Int], ylen: Int, zp: Array[Int]): Array[Int] = {\n\n    val xstart: Int = xlen - 1;\n    val ystart: Int = ylen - 1;\n    var z: Array[Int] = zp\n\n    if (zp == null || zp.length < (xlen + ylen))\n      z = new Array[Int](xlen + ylen)\n\n    var carry: Long = 0;\n    var j = ystart\n    var k = ystart + 1 + xstart\n    while (j >= 0) {\n      val product: Long = (y(j) & LONG_MASK) *\n        (x(xstart) & LONG_MASK) + carry;\n      z(k) = product.toInt\n      carry = product >>> 32;\n      j = j - 1\n      k = k - 1\n    }\n    z(xstart) = carry.toInt\n\n    var i: Int = xstart - 1\n    while (i >= 0) {\n      carry = 0;\n      var j = ystart\n      var k = ystart + 1 + i;\n      while (j >= 0) {\n        val product: Long = (y(j) & LONG_MASK) *\n          (x(i) & LONG_MASK) +\n          (z(k) & LONG_MASK) + carry;\n        z(k) = product.toInt\n        carry = product >>> 32;\n        j = j - 1\n        k = k - 1\n      }\n      z(i) = carry.toInt\n      i = i - 1\n    }\n    return z;\n  }\n\n  /**\n    * Returns the input array stripped of any leading zero bytes.\n    * Since the source is trusted the copying may be skipped.\n    */\n  def trustedStripLeadingZeroInts(jval: Array[Int]): Array[Int] = {\n    val vlen: Int = jval.length;\n    var keep: Int = 0\n\n    // Find first nonzero byte\n    while (keep < vlen && jval(keep) == 0)\n      keep = keep + 1\n\n    return if (keep == 0) jval else java.util.Arrays.copyOfRange(jval, keep, vlen);\n  }\n\n  /**\n    * Multiplies two BigIntegers using the Karatsuba multiplication\n    * algorithm.  This is a recursive divide-and-conquer algorithm which is\n    * more efficient for large numbers than what is commonly called the\n    * \"grade-school\" algorithm used in multiplyToLen.  If the numbers to be\n    * multiplied have length n, the \"grade-school\" algorithm has an\n    * asymptotic complexity of O(n^2).  In contrast, the Karatsuba algorithm\n    * has complexity of O(n^(log2(3))), or O(n^1.585).  It achieves this\n    * increased performance by doing 3 multiplies instead of 4 when\n    * evaluating the product.  As it has some overhead, should be used when\n    * both numbers are larger than a certain threshold (found\n    * experimentally).\n    *\n    * See:  http://en.wikipedia.org/wiki/Karatsuba_algorithm\n    */\n  def multiplyKaratsuba(x: MyBigInt, y: MyBigInt): MyBigInt = {\n    val xlen = x.mag.length;\n    val ylen = y.mag.length;\n\n    // The number of ints in each half of the number.\n    val half: Int = (Math.max(xlen, ylen) + 1) / 2;\n\n    // xl and yl are the lower halves of x and y respectively,\n    // xh and yh are the upper halves.\n    val xl: MyBigInt = x.getLower(half);\n    val xh: MyBigInt = x.getUpper(half);\n    val yl: MyBigInt = y.getLower(half);\n    val yh: MyBigInt = y.getUpper(half);\n\n    val p1: MyBigInt = xh.multiply(yh);\n    // p1 = xh*yh\n    val p2: MyBigInt = xl.multiply(yl); // p2 = xl*yl\n\n    // p3=(xh+xl)*(yh+yl)\n    val p3: MyBigInt = xh.add(xl).multiply(yh.add(yl));\n\n    // result = p1 * 2^(32*2*half) + (p3 - p1 - p2) * 2^(32*half) + p2\n    val result: MyBigInt = p1.shiftLeft(32 * half).add(p3.subtract(p1).subtract(p2)).shiftLeft(32 * half).add(p2);\n\n    if (x.signum != y.signum) {\n      return result.negate();\n    } else {\n      return result;\n    }\n  }\n\n  def multiply(me: MyBigInt, that: MyBigInt): MyBigInt = {\n    import me._\n    if (that.signum == 0 || signum == 0)\n      return ZERO;\n\n    val xlen: Int = me.mag.length;\n\n    if (that == me && xlen > MULTIPLY_SQUARE_THRESHOLD) {\n      return ??? //square();\n    }\n\n    val ylen = that.mag.length;\n\n    if ((xlen < KARATSUBA_THRESHOLD) || (ylen < KARATSUBA_THRESHOLD)) {\n      val resultSign = if (signum == that.signum) 1 else -1;\n      if (that.mag.length == 1) {\n        return multiplyByInt(mag, that.mag(0), resultSign);\n      }\n      if (mag.length == 1) {\n        return multiplyByInt(that.mag, mag(0), resultSign);\n      }\n      var result: Array[Int] = multiplyToLen(mag, xlen, that.mag, ylen, null);\n      result = trustedStripLeadingZeroInts(result);\n      return new MyBigInt(result, resultSign);\n    } else {\n      return multiplyKaratsuba(me, that);\n    }\n  }\n}"

}

