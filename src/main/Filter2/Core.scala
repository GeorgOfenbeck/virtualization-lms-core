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

  def compute_type_symetrie(stat: StatFilterHeader): StatFilterHeader = {
    //val presort =
    ???
  }


  def filter_core(mix: MixFilterHeader, xindex: Exp[Int], yindex: Exp[Int], outimg: Rep[ImageH]): Rep[ImageH] = {
    import mix._

    val ina: Rep[Int] = {
      import matrix.r1.c1._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_minus(xindex, Const(1)), int_minus(yindex, Const(1)))
      }
    }
    val inb: Rep[Int] = {
      import matrix.r1.c2._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, Const(0), int_minus(yindex, Const(1)))
      }
    }
    val inc: Rep[Int] = {
      import matrix.r1.c3._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_plus(xindex, Const(1)), int_minus(yindex, Const(1)))
      }
    }
    val ind: Rep[Int] = {
      import matrix.r2.c1._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_minus(xindex, Const(1)), Const(0))
      }
    }
    val ine: Rep[Int] = {
      import matrix.r2.c2._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, Const(0), Const(0))
      }
    }
    val inf: Rep[Int] = {
      import matrix.r2.c3._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_plus(xindex, Const(1)), Const(0))
      }
    }
    val ing: Rep[Int] = {
      import matrix.r3.c1._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_minus(xindex, Const(1)), int_plus(yindex, Const(1)))
      }
    }
    val inh: Rep[Int] = {
      import matrix.r3.c2._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, Const(0), int_plus(yindex, Const(1)))
      }
    }
    val ini: Rep[Int] = {
      import matrix.r3.c3._
      val z = evnum.zero
      a match {
        case `z` => Const(0)
        case _ => getImage[Int](image_in, int_plus(xindex, Const(1)), int_plus(yindex, Const(1)))
      }
    }


    /*int rgb = inPixels[ioffset+ix];
    a += f * ((rgb >> 24) & 0xff);
    r += f * ((rgb >> 16) & 0xff);
    g += f * ((rgb >> 8) & 0xff);
    b += f * (rgb & 0xff);*/

    def short(o: OneEntry, in: Exp[Int]) = {
      import o._
      implicit val nev = evnum
      implicit val tev = evtyp
      val pixel = in
      val pa = pixelgetAlpha(pixel)
      val pr = pixelgetRed(pixel)
      val pg = pixelgetGreen(pixel)
      val pb = pixelgetBlue(pixel)
      val ra = toDouble(gentimes[T](fromInt(pa), ev.toRep(a)))
      val rr = toDouble(gentimes[T](fromInt(pr), ev.toRep(a)))
      val rg = toDouble(gentimes[T](fromInt(pg), ev.toRep(a)))
      val rb = toDouble(gentimes[T](fromInt(pb), ev.toRep(a)))
      (ra, rr, rg, rb)
    }

    val (aa, ar, ag, ab) = short(matrix.r1.c1, ina)
    val (ba, br, bg, bb) = short(matrix.r1.c2, inb)
    val (ca, cr, cg, cb) = short(matrix.r1.c3, inc)
    val (da, dr, dg, db) = short(matrix.r2.c1, ind)
    val (ea, er, eg, eb) = short(matrix.r2.c2, ine)
    val (fa, fr, fg, fb) = short(matrix.r2.c3, inf)
    val (ga, gr, gg, gb) = short(matrix.r3.c1, ing)
    val (ha, hr, hg, hb) = short(matrix.r3.c2, inh)
    val (ia, ir, ig, ib) = short(matrix.r3.c3, ini)


    val suma = Vector(aa, ba, ca, da, ea, fa, ga, ha, ia).foldLeft(Const(implicitly[Numeric[Double]].zero))((acc, ele) => {
      genplus(acc, ele)
    })
    val sumr = Vector(ar, br, cr, dr, er, fr, gr, hr, ir).foldLeft(Const(implicitly[Numeric[Double]].zero))((acc, ele) => {
      genplus(acc, ele)
    })
    val sumg = Vector(ag, bg, cg, dg, eg, fg, gg, hg, ig).foldLeft(Const(implicitly[Numeric[Double]].zero))((acc, ele) => {
      genplus(acc, ele)
    })
    val sumb = Vector(ab, bb, cb, db, eb, fb, gb, hb, ib).foldLeft(Const(implicitly[Numeric[Double]].zero))((acc, ele) => {
      genplus(acc, ele)
    })

    val outPixel = combinePixel(suma, sumr, sumg, sumb)

    val out = setImage(outimg, xindex, yindex, outPixel)
    out
    //setImage(arrayjjj,xindex,yindex,Const(0))


    /*
                  int rgb = inPixels[ioffset+ix];
a += f * ((rgb >> 24) & 0xff);
r += f * ((rgb >> 16) & 0xff);
g += f * ((rgb >> 8) & 0xff);
b += f * (rgb & 0xff);
     */
  }


  def multiply_core(stat: StatFilterHeader): MaybeSFunction = {
    val exposarg: ExposeRep[DynFilterHeader] = exposeDynHeader(stat)
    implicit val exposeret = exposeRepFromRep[ImageH]
    val stageme: (DynFilterHeader => Rep[ImageH]) = (dyn: DynFilterHeader) => {

      val mix = MixFilterHeader(stat, dyn)
      import mix._
      val iterations_x = image.xsize.ev.div(image.xsize.a, image.xsize.ev.const(blocking.blockingx)) //dyn
      val iterations_y = image.ysize.ev.div(image.ysize.a, image.ysize.ev.const(blocking.blockingy)) //dyn

      val rest_x = image.xsize.ev.mod(image.xsize.a, image.xsize.ev.const(blocking.blockingx)) //dyn
      val rest_y = image.ysize.ev.mod(image.ysize.a, image.ysize.ev.const(blocking.blockingy)) //dyn

      val itblockedx = blocking.blockingx / blocking.unrollx
      val itblockedy = blocking.blockingy / blocking.unrolly


      val xe = image.xsize.ev
      val xzero = xe.const(0)
      val xrange = xe.unt(xzero, iterations_x)
      xe.rangefold(xrange, image_out, exposeret)({
        case (arrayx, i) => {
          val ye = image.ysize.ev
          val yzero = ye.const(0)
          val yrange = ye.unt(yzero, iterations_y)
          ye.rangefold(yrange, arrayx, exposeret)({
            case (arrayy, j) => {
              val iirange = cRep.unt(cRep.const(0), cRep.const(itblockedx))
              cRep.rangefold(iirange, arrayy, exposeret)({
                case (arrayii, ii) => {
                  val jjrange = cRep.unt(cRep.const(0), cRep.const(itblockedy))
                  cRep.rangefold(jjrange, arrayii, exposeret)({
                    case (arrayjj, jj) => {

                      val s1 = xe.gtimes(i, xe.const(blocking.blockingx))
                      val s2 = gentimes(ii, Const(blocking.unrollx))
                      val xoffset = genplus(xe.toRep(s1), s2)


                      val iiirange = cNoRep.unt(cNoRep.const(0), cNoRep.const(blocking.unrollx))
                      cNoRep.rangefold(iiirange, arrayjj, exposeret)({
                        case (arrayiii, iii) => {

                          val sy1 = ye.gtimes(j, ye.const(blocking.blockingy))
                          val sy2 = gentimes(jj, Const(blocking.unrolly))
                          val yoffset = genplus(ye.toRep(sy1), sy2)


                          val jjjrange = cNoRep.unt(cNoRep.const(0), cNoRep.const(blocking.unrolly))
                          cNoRep.rangefold(jjjrange, arrayiii, exposeret)({
                            case (arrayjjj, jjj) => {
                              val xindex = int_plus(xoffset, Const(iii))
                              val yindex = int_plus(yoffset, Const(jjj))
                              filter_core(mix, xindex, yindex, arrayjjj)
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
      val (nstat, ndyn) = nmix.split()
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
      val (nstat, ndyn) = mix.split()
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
    val ini: StatFilterHeader = StatFilterHeader[Int, Int, Int, Int, Int, Int, Int, Int, Int](inlineInfo = inline)
    //val ini = StatFilterHeader[Int, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep, Rep](Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), Const(-1), inline)
    //val ini: StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep] = StatKaratsubaHeader[Rep,Rep,Rep,Rep,Rep,Rep](Const(-1), Const(-1), Const(-1),Const(-1))
    val esc = codegen.emitSource(entry(ini), "testClass", stream2)(exposeDynHeader(ini), exposeRepFromRep[ImageH])
    stream2.println("\n}\n")
    stream2.flush()
    stream2.close()
  }

  val codefrag = "\npackage Filter2\n\nclass ImageH(val ele: Int){\n  def get(x: Int, y: Int): Int = ele\n  def set(x: Int, y: Int, p: Int) = this\n}\n"


}