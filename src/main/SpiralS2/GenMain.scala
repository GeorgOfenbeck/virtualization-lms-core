package SpiralS2

import scife.enumeration.showcase.SciFETest


object GenMain extends App {

  val breakdown_enum = SciFETest.getBreakdown()


  val dft64s = breakdown_enum(64)

  val dsl = new Core
  dsl.codeexport()
}

