

import ch.ethz.spirals.dsls._
val wht4 = WHT(4) //Walsh–Hadamard transform size 4
wht4.toString() //
wht4.toMatrix().toString
MathUtilities.printm(wht4.toMatrix())
val f2 = F_2()
val i2 = I(2)
val wht_composed_1 = MathUtilities.kronecker(f2.toMatrix(),i2.toMatrix())
MathUtilities.printm(wht_composed_1)
val wht_composed_2 = MathUtilities.kronecker(i2.toMatrix(),f2.toMatrix())
MathUtilities.printm(wht_composed_2)
val wht_composed_complete = wht_composed_1.multiply(wht_composed_2)
MathUtilities.printm(wht_composed_complete)
