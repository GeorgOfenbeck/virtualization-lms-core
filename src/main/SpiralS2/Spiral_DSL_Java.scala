package SpiralS2

trait JavaGenSpiral_DSL extends scala.lms.targets.javalike.JavaCodegen with scala.lms.targets.javalike.TupleHelper /*with EmitHeadInternalFunctionAsClass  */
{
  val IR: Spiral_DSL

  import IR._

  var delay: Vector[(TP[_], Vector[String], (Block, Vector[String]) => Vector[String])] = Vector.empty
  var delaynow: Boolean = false


  override def emitNode(tp: TP[_], acc: Vector[String],
                        block_callback: (Block, Vector[String]) => Vector[String]): Vector[String] = {
    val ma = tp.rhs match {
      case CompRe(x: Exp[Complex]) => Vector(emitValDef(tp, s"${quote(x)}.re"))
      case CompIm(x: Exp[Complex]) => Vector(emitValDef(tp, s"${quote(x)}.im"))
      case CompCreate(re: Exp[Double], im: Exp[Double]) => Vector(emitValDef(tp, "Complex(" + quote(re) + "," + quote(im) + ")"))
      case BaseCase(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " == 2 //check for base case"))
      case IsPrime(n: Exp[Int]) => Vector(emitValDef(tp, " false //put prime factor check here"))
      //case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](" + quote(n) + ") //buffer creation"))
      case VecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new ComplexVector(new Array[Complex](" + quote(n) + ")) //buffer creation"))
      case VecApply(vec: Exp[ComplexVector], i: Exp[Int]) => Vector(emitValDef(tp, "" + quote(vec) + "[" + quote(i) + "]"))
      case VecSame(x: Exp[ComplexVector], y: Exp[ComplexVector]) => Vector(emitValDef(tp, "" + quote(x)))
      case VecUpdate(vec: Exp[ComplexVector], i: Exp[Int], y: Exp[Complex]) => Vector(emitValDef(tp, "" + quote(vec) + ".update(" + quote(i) + "," + quote(y) + ")"))

      case DVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "new Array[Double](2*" + quote(n) + ") //buffer creation"))
      case DVecApply(vec: Exp[Array[Double]], i: Exp[Int]) => Vector(emitValDef(tp, "" + quote(vec) + "[" + quote(i) + "]"))
      case DVecSame(x: Exp[Array[Double]], y: Exp[Array[Double]]) => Vector(emitValDef(tp, "" + quote(x)))
      case DVecUpdate(vec: Exp[Array[Double]], i: Exp[Int], y: Exp[Double]) => Vector(s"${quote(vec)}[ ${quote(i)} ] = ${quote(y)};\n") ++ Vector(emitValDef(tp,s"${quote(vec)}") )
      case IVecAddStride(v: Exp[Vector[Int]], y: Exp[Int], b: Exp[Int]) => Vector(emitValDef(tp, quote(v) + ".dropRight(1) :+ " + quote(v) + ".last / " + quote(y) + "/// ADD STride " + quote(b)))
      case IVecCreate(n: Exp[Int]) => Vector(emitValDef(tp, "Vector.empty[Int] //creating vector with " + quote(n)))
      case IVecAppend(v: Exp[Vector[Int]], y: Exp[Int]) => Vector(emitValDef(tp, quote(v) + " :+ " + quote(y)))
      case IVecApply(vec, i) => Vector(emitValDef(tp, quote(vec) + "(" + quote(i) + ")"))
      case IVecZipMagic(r, s) => Vector(emitValDef(tp, "Vector(" + quote(r) + ".headOption.getOrElse(0) * " + quote(s) + ".headOption.getOrElse(0)) ++ " + quote(r) + ".tail.zipAll(" + quote(s) + ".tail,0,0).map(p => p._1 + " + quote(r) + ".headOption.getOrElse(0) * p._2)"))
      case IVecMult(b, s, l) => Vector(emitValDef(tp, " VectorMult(" + quote(b) + "," + quote(s) + "," + quote(l) + ")"))
      case IVecFirstorZero(v) => Vector(emitValDef(tp, quote(v) + ".headOption.getOrElse(0)"))
      case IVecUpRank(v) => Vector(emitValDef(tp, "Vector(" + quote(v) + ".head, 0) ++ " + quote(v) + ".tail"))
      case Int_Eq(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " == " + quote(rhs)))
      case Plus(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " + " + quote(rhs)))
      case Minus(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " - " + quote(rhs)))
      case Times(lhs, rhs) => Vector(emitValDef(tp, quote(lhs) + " * " + quote(rhs)))
      //case Divide(lhs,rhs) => Vector(emitValDef(tp, quote(lhs) + " / " + quote(rhs)))
      case Twiddle_Apply(vec: Exp[ComplexVector], size: Exp[Int], n: Exp[Int], d: Exp[Int], k: Exp[Int]) => Vector(emitValDef(tp, " Twiddle(" + quote(vec) + "," + quote(n) + "," + quote(d) + "," + quote(k) + ")"))
      case Twiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + ")"))
      case Twiddle_Apply_Index_Store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle.store(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + ")"))
      case Twiddle_Load(i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle.load()"))

      case DTwiddle_Apply_Index(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) => {
        val t = if (re) {
          ".re"
        } else {
          ".im"
        }
        Vector(emitValDef(tp, " Twiddle(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + ")" + t))
      }
      case DTwiddle_Apply_Index_Store(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) => {
        Vector(emitValDef(tp, " Twiddle.dstore(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + "," + re + ")"))
      }

      case DTwiddle_Apply_Index_Load(n: Exp[Int], d: Exp[Int], k: Exp[Int], i: Exp[Int], re: Boolean) => {
        Vector(emitValDef(tp, " Twiddle.dload(" + quote(n) + "," + quote(d) + "," + quote(k) + "," + quote(i) + "," + (if(re) "0" else "1") + ")"))
      }

      case DTwiddle_Load(i: Exp[Int]) => Vector(emitValDef(tp, " Twiddle.dload()"))


      //case Radix(n: Exp[Int]) => Vector(emitValDef(tp, quote(n) + " / 2 //stupid radix choice placeholder"))
      case Lookupid2lid(n: Exp[Int]) => Vector(emitValDef(tp, s"Settings.id2ids.getOrElse(${quote(n)},(-99,-99))._1"))
      case Lookupid2rid(n: Exp[Int]) => Vector(emitValDef(tp, s"Settings.id2ids.getOrElse(${quote(n)},(-99,-99))._2"))
      case Lookupsize2id(n: Exp[Int]) => Vector(emitValDef(tp, s"Settings.size2id.getOrElse(${quote(n)},-99)"))
      case Radix(l: Exp[Int]) => Vector(emitValDef(tp, "Settings.id2radix.getOrElse(" + quote(l) + ",2)"))

      case Twid(l: Exp[List[Int]]) => Vector(emitValDef(tp, "{val t = Settings.decompchoice.getOrElse(" + quote(l) + ",{ val t: (Int,Boolean,Boolean) = ???; \nt})._3\nif(t) 1 else 0}\n"))
      case ListAdd(l: Exp[List[Int]], i: Exp[Int]) => Vector(emitValDef(tp, quote(l) + ":+ " + quote(i)))

      case SumFold(till: Exp[Int], parllel: Boolean, ini: Exp[ComplexVector], loopvar: Exp[Int], acc: Exp[ComplexVector], body) => {
        val bodylambda = exp2tp(body)
        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            //val l1 = "val " + quote(tp) + " = ("+ quote(loopvar) + " <- 0 until "+ quote(till) + ").foldLeft(Vector.empty) {\n "
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
              //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val argtuple = tupledeclarehelper(tx.map(a => remap(a.tag)), "")
            val l1 = if (parllel)
              "val " + quote(tp) + " = (0 until " + quote(till) + ").par.foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
            else
              "val " + quote(tp) + " = (0 until " + quote(till) + ").foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
            val l10 = l1 + "\n" + helper + "\n"
            val l2 = block_callback(ty, Vector(l10))
            val trestuple: Vector[String] = ty.res.map(r => quote(r))
            val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
            val l4 = l3 + "\n})\n"
            l4
          })
          case _ => {
            assert(false, "got an SumLoop statment which does not contain a lambda")
            Vector.empty
          }
        }
        rets
      }

      case SumFoldX(till: Exp[Int], parllel: Option[Int], ini: Exp[_], out: Exp[_], loopvar: Exp[Int], acc: Exp[ComplexVector], body) => {
        val bodylambda = exp2tp(body)
        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            //val l1 = "val " + quote(tp) + " = ("+ quote(loopvar) + " <- 0 until "+ quote(till) + ").foldLeft(Vector.empty) {\n "
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
              //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val argtuple = tupledeclarehelper(tx.map(a => remap(a.tag)), "")
            val l1 = parllel.fold[String]("val " + quote(tp) + " = (0 until " + quote(till) + ").foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n")(nrthreads => {
              //"val " + quote(tp) + " = (0 until " + quote(till) + ").par.foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
              "val " + quote(tp) + " = Twiddle.parloop(" + quote(till) + "," + nrthreads + "," + quote(ini) + "," + quote(out) + ",(x: (SpiralS2.ComplexVector,Int)) => {\n        val helper = x\n "
            })


            val l10 = l1 + "\n" + helper + "\n"
            val l2 = block_callback(ty, Vector(l10))
            val trestuple: Vector[String] = ty.res.map(r => quote(r))
            val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
            val l4 = l3 + "\n})\n"
            l4
          })
          case _ => {
            assert(false, "got an SumLoop statment which does not contain a lambda")
            Vector.empty
          }
        }
        rets
      }


      case SigmaLoop(till: Exp[Int], parallel: Option[Int], in: Exp[_], out: Exp[_], exparg: ExposeRep[_], body) => {
        val bodylambda = exp2tp(body)


        val rets: Vector[String] = bodylambda.rhs match {
          case InternalLambda(tf, tx, ty, thot, targs, treturns) => Vector({
            //val l1 = "val " + quote(tp) + " = ("+ quote(loopvar) + " <- 0 until "+ quote(till) + ").foldLeft(Vector.empty) {\n "
            val helper = if (tx.size > 1) {
              tx.zipWithIndex.map(a => {
                val (tp, index) = a
                val typ = remap(tp.tag.mf)
                "val " + quote(tp) + " : " + remap(tp.tag) + " = helper" + tupleaccesshelper(index, "", index == tx.size - 1)
              }).mkString("\n")
            } else {
              //"val " + quote(x.head) + " : " + remap(x.head.tag.mf) + " = helper\n"
              "val " + quote(tx.head) + " : " + remap(tx.head.tag) + " = helper\n"
            }
            val argtuple = tupledeclarehelper(tx.map(a => remap(a.tag)), "")

            parallel.fold[String]({
              val l1 = s"val ${quote(tp)} = {for(lc <- 0 until ${quote(till)}){\n val helper = (lc,${quote(in)},${quote(out)})\n "
              val l10 = l1 + "\n" + helper + "\n"
              val l2 = block_callback(ty, Vector(l10))
              val trestuple: Vector[String] = ty.res.map(r => quote(r))
              val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
              val l4 = l3 + s"\n};\n${quote(out)} }\n"
              l4
            })(nrthreads => {
              val l1 = s"val ${quote(tp)} = Twiddle.parloop(${quote(till)},$nrthreads,${quote(in)},${quote(out)},(lc: Int) => {\n val helper = (lc,${quote(in)},${quote(out)})\n "
              val l10 = l1 + "\n" + helper + "\n"
              val l2 = block_callback(ty, Vector(l10))
              val trestuple: Vector[String] = ty.res.map(r => quote(r))
              val l3: String = l2.mkString("") + tupledeclarehelper(trestuple, "")
              val l4 = l3 + s"\n}) \n"
              l4
            })


            /*
                        val l1 = parllel.fold[String]("val " + quote(tp) + " = (0 until " + quote(till) + ").foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n")(nrthreads => {
                          //"val " + quote(tp) + " = (0 until " + quote(till) + ").par.foldLeft( " + quote(ini) + " )(\n  (acc,ele) => {\n val helper = (acc,ele)\n"
                          "val " + quote(tp) + " = Twiddle.parloop(" + quote(till) + "," + nrthreads + "," + quote(ini) + "," + quote(out) + ",(x: (SpiralS2.ComplexVector,Int)) => {\n        val helper = x\n "
                        })*/


          })
          case _ => {
            assert(false, "got an SumLoop statment which does not contain a lambda")
            Vector.empty
          }
        }
        rets
      }


      case _ => {

        super.emitNode(tp, acc, block_callback)
      }
    }
    ma
  }
}