package edu.kzk.reflection



object RuntimeRelfectionApp extends App {
  // Import runtime universe
  import scala.reflect.runtime.{ universe => ru }

  val b: Boolean = true

  case class Person(name: String)

  // Get mirror
  var m = ru.runtimeMirror(getClass().getClassLoader())

  // Get Person class symbol
  val classPerson = ru.typeOf[Person].typeSymbol.asClass
  //val classPerson = m.staticClass("edu.kzk.reflection.RuntimeRelfectionApp.Person")

  // Get class mirror
  val cm = m.reflectClass(classPerson)

  // Get constructor method symbol
  val ctor = ru.typeOf[Person].decl(ru.termNames.CONSTRUCTOR).asMethod

  // Get constructor
  val ctorm = cm.reflectConstructor(ctor);

  // Call constructor
  val person = ctorm("Mike")
  println(person)
  println()
}




object RuntimeRelfectionApp2 extends App {
  // Import runtime universe
  import scala.reflect.runtime.{ universe => ru }

  case class Purchase(name: String, orderNumber: Int, var shipped: Boolean)
  val purchase = Purchase("Jeff Lebowski", 23819, false);

  // Get mirror
  val m = ru.runtimeMirror(getClass().getClassLoader())

  // Get term symbol (have to newTermName)
  val shippingTermSymb = ru.typeOf[Purchase].decl(ru.TermName("shipped")).asTerm

  // Get instance mirror
  val im = m.reflect(purchase)

  // Get field mirror
  val shippingFieldMirror = im.reflectField(shippingTermSymb);

  // Get field
  println(shippingFieldMirror.get)

  // Get field
  shippingFieldMirror.set(true)
  println(shippingFieldMirror.get)

  /*
   * Reification using classTag
   */
  import scala.reflect._
  val ct = classTag[String]
  val x = ct.runtimeClass.newInstance() + "This is relfected instance";
  println(x);
}

object Boolreflect extends App{
  // Import runtime universe
  import scala.reflect.runtime.{ universe => ru }

  val m = ru.runtimeMirror(getClass.getClassLoader())

  // Get Person class symbol
  val classBoolean = ru.typeOf[Boolean].typeSymbol.asClass

  // Get class mirror
  val cm = m.reflectClass(classBoolean)



  ru.typeOf[Boolean].decls.map(d => {
    println()
    if (d.isMethod) {
      val m = d.asMethod
      println(m.toString)
      println(m.toString.drop(7))
      println(m.name)
      println(m.fullName)
    }
  })

  ru.typeOf[Int].decls.map(d => {
    println()
    if (d.isMethod) {
      val m = d.asMethod
      println(m.toString)
      println(m.toString.drop(7))
      println(m.name)
      println(m.fullName)
    }
  })

  ru.typeOf[Double].decls.map(d => {
    println()
    if (d.isMethod) {
      val m = d.asMethod
      println(m.toString)
      println(m.toString.drop(7))
      println(m.name)
      println(m.fullName)
    }
  })

  ru.typeOf[Boolean].decls.map(d => {
    println()
    if (d.isMethod) {
      val m = d.asMethod
      println(m.toString)
      println(m.toString.drop(7))
      println(m.name)
      println(m.fullName)
    }
  })





  // Get constructor method symbol
  val ctor = ru.typeOf[Boolean].decl(ru.termNames.CONSTRUCTOR).asMethod


  val b: Boolean = true

  b.!=()


  // Get constructor
  val ctorm = cm.reflectConstructor(ctor);

  // Call constructor
  val person = ctorm("Mike")
  println(person)
  println()
}