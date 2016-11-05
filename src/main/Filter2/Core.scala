package Filter2



import org.scala_lang.virtualized.SourceContext

import scala.lms._
import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

class Core extends FilterHeader {
  self =>
  val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }
  override val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenPrimitivOps with sort.ScalaGenSort_DSL with ScalaGenBooleanOps with ScalaGenIfThenElse {
    val IR: self.type = self
  }

  case class MaybeSFunction(f: Either[StagedFunction[DynFilterHeader, Rep[ImageH]], DynFilterHeader => Rep[ImageH]]) {
    def apply(dyn: DynFilterHeader): Rep[ImageH] = f.fold(fa => fa(dyn), fb => fb(dyn))
  }

  object MaybeSFunction {
    def apply(f: StagedFunction[DynFilterHeader, Rep[ImageH]]): MaybeSFunction = MaybeSFunction(Left(f))

    def apply(f: (DynFilterHeader => Rep[ImageH])): MaybeSFunction = MaybeSFunction(Right(f))
  }

  def compute_type_symetrie(stat: StatFilterHeader):StatFilterHeader = {
    //val presort =
    ???
  }


  def multiply_core(stat: StatFilterHeader): MaybeSFunction = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._
      val iterations_x = image.xsize.ev.div(image.xsize.a,image.xsize.ev.const(blocking.blockingx)) //dyn
      val iterations_y = image.ysize.ev.div(image.ysize.a,image.ysize.ev.const(blocking.blockingy)) //dyn

      val rest_x = image.xsize.ev.mod(image.xsize.a,image.xsize.ev.const(blocking.blockingx)) //dyn
      val rest_y = image.ysize.ev.mod(image.ysize.a,image.ysize.ev.const(blocking.blockingy)) //dyn

      val itblockedx = blocking.blockingx / blocking.unrollx
      val itblockedy = blocking.blockingy / blocking.unrolly


      val xe = image.xsize.ev
      val xzero = xe.const(0)
      val xrange = xe.unt(xzero,iterations_x)
      xe.rangefold(xrange,image_out,exposeret)({
          case (arrayx, i) => {
            val ye = image.ysize.ev
            val yzero = ye.const(0)
            val yrange = ye.unt(yzero,iterations_y)
            ye.rangefold(yrange,arrayx,exposeret)({
              case (arrayy, j) => {
                val iirange = cRep.unt(cRep.const(0),cRep.const(itblockedx))
                cRep.rangefold(iirange,arrayy,exposeret)({
                  case (arrayii, ii) => {
                    val jjrange = cRep.unt(cRep.const(0),cRep.const(itblockedy))
                    cRep.rangefold(jjrange,arrayii,exposeret)({
                      case (arrayjj, jj) => {

                        val s1 = xe.gtimes(i,xe.const(blocking.blockingx))
                        val s2 = gentimes(ii,Const(blocking.unrollx))
                        val xoffset = genplus(xe.toRep(s1),s2)


                        val iiirange = cNoRep.unt(cNoRep.const(0),cNoRep.const(blocking.unrollx))
                        cNoRep.rangefold(iiirange,arrayjj,exposeret)({
                          case (arrayiii, iii) => {

                            val sy1 = ye.gtimes(j,ye.const(blocking.blockingy))
                            val sy2 = gentimes(jj,Const(blocking.unrolly))
                            val yoffset = genplus(ye.toRep(sy1),sy2)


                            val jjjrange = cNoRep.unt(cNoRep.const(0),cNoRep.const(blocking.unrolly))
                            cNoRep.rangefold(jjjrange,arrayiii,exposeret)({
                              case (arrayjjj, jjj) => {
                                val xindex = int_plus(xoffset,Const(iii))
                                val yindex = int_plus(yoffset,Const(jjj))


                                val ina = getImage[Int](image_in, int_minus(xindex,Const(1)), int_minus(yindex,Const(1)))
                                val inb = getImage[Int](image_in, Const(0), int_minus(yindex,Const(1)))
                                val inc = getImage[Int](image_in, int_plus(xindex,Const(1)), int_minus(yindex,Const(1)))
                                val ind = getImage[Int](image_in, int_minus(xindex,Const(1)), Const(0))
                                val ine = getImage[Int](image_in, Const(0), Const(0))
                                val inf = getImage[Int](image_in, int_plus(xindex,Const(1)), Const(0))
                                val ing = getImage[Int](image_in, int_minus(xindex,Const(1)), int_plus(yindex,Const(1)))
                                val inh = getImage[Int](image_in, Const(0), int_plus(yindex,Const(1)))
                                val ini = getImage[Int](image_in, int_plus(xindex,Const(1)), int_plus(yindex,Const(1)))


                                val a1 = {
                                  import matrix.r1.c1._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(ina),ev.toRep(a)))
                                }
                                val b1 = {
                                  import matrix.r1.c2._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(inb),ev.toRep(a)))
                                }
                                val c1 = {
                                  import matrix.r1.c3._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(inc),ev.toRep(a)))
                                }
                                val d1 = {
                                  import matrix.r2.c1._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(ind),ev.toRep(a)))
                                }
                                val e1 = {
                                  import matrix.r2.c2._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(ine),ev.toRep(a)))
                                }
                                val f1 = {
                                  import matrix.r2.c3._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(inf),ev.toRep(a)))
                                }
                                val g1 = {
                                  import matrix.r3.c1._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(ing),ev.toRep(a)))
                                }
                                val h1 = {
                                  import matrix.r3.c2._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(inh),ev.toRep(a)))
                                }
                                val i1 = {
                                  import matrix.r3.c3._
                                  implicit val nev = evnum
                                  implicit val tev = evtyp
                                  toDouble(gentimes[T](fromInt(ini),ev.toRep(a)))
                                }

                                val t1 = genplus(a1,b1)

                                val sum = Vector(a1, b1, c1, d1, e1, f1, g1, h1, i1).foldLeft(Const(implicitly[Numeric[Double]].zero))((acc, ele) =>
                                  {

                                    genplus(acc, ele)
                                  })
                                val out = setImage(arrayjjj, xindex, yindex, toInt(sum))

                                out

                                //setImage(arrayjjj,xindex,yindex,Const(0))
                              }
                            })
                          }
                        })


                      }
                    })
                  }
                })
                //setImage(arrayy,xe.toRep(i),ye.toRep(j),Const(0))
              }
            })
          }
        })
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynFilterHeader, Rep[ImageH]] = doGlobalLambda(stageme, Some("FilterMultCore" + stat.genSig()), Some("FilterMultCore" + stat.genSig()))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def multiply(stat: StatFilterHeader): MaybeSFunction = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._


      val nmix = mix.cpy(image_in = image_out, image_out = image_in)
      val (nstat,ndyn) = nmix.split()
      val f = multiply_core(nstat)
      f(ndyn)
    }
    if (stat.inline.inline) {
      MaybeSFunction(stageme)
    } else {
      val t: StagedFunction[DynFilterHeader, Rep[ImageH]] = doGlobalLambda(stageme, Some("FilterMult" + stat.genSig()), Some("FilterMult" + stat.genSig()))(exposarg, exposeret)
      MaybeSFunction(t)
    }
  }

  def entry(stat: StatFilterHeader): (DynFilterHeader => Rep[ImageH]) = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._
      val (nstat,ndyn) = mix.split()
      val f = multiply(nstat)
      val x = f(ndyn)
      x
    }
    stageme
  }

  def codeexport() = {
    val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("C:\\Phd\\git\\code\\deleteme\\src\\main\\TestFilter.scala"))
    stream2.println(codefrag)
    val inline = InlineInfo(false, 10, true, false, true, 0)
    val ini: StatFilterHeader = StatFilterHeader[Int,Int,Int,Int,Int,Int,Int,Int,Int](inlineInfo = inline)
    //val ini = StatFilterHeader[Int, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep](Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), inline)
    //val ini: StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep] = StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](Const(-1), Const(-1), Const(-1),Const(-1))
    val esc = codegen.emitSource(entry(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[ImageH])
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

  val codefrag = "\npackage Filter2\n\nclass ImageH(val ele: Int){\n  def get(x: Int, y: Int): Int = ele\n  def set(x: Int, y: Int, p: Int) = this\n}\n"


}