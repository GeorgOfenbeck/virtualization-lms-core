/*
/**
 * Georg Ofenbeck
 First created:
 * Date: 03/09/2014
 * Time: 16:47 
 */


import org.scalatest.FunSpec

import scala.virtualization.lms.common._

class ToyLMS extends FunSpec {


  describe("checking it out") {



    import scala.reflect.runtime.{universe=>ru}


    case class Sym[+T:ru.TypeTag](val id: Int)
    def fresh[T: ru.TypeTag]: Sym[T] = Sym[T](0)
    /*case class Cont[+T] (val x: T)(implicit tag: TypeTag[T]){
      def mytype() = tag
    }*/

    case class Cont[+T: ru.TypeTag] (val x: T){
      def mytype() = ru.typeOf[T]
    }


    class dummy[T: ru.TypeTag]

    def bla[T](x: Cont[T] )(implicit tag: ru.TypeTag[T]): Sym[T] = {
      println(ru.typeOf[T])
      fresh[T]

    }

    def bla2[T: ru.TypeTag](): Sym[T] = {
      val clazz = ru.typeTag[T].mirror.runtimeClass(ru.typeOf[T])
      clazz.newInstance.asInstanceOf[T]
      fresh[T]
    }


    def lkdfjlaskjfd[longtype]() = {
      x: longtype
    }


    def newInstance[T: ru.TypeTag] = {

    }

    class A
    class B extends A
    val e1 = Cont("hello")
    val e2 = Cont(3.0)

    //val e3 = Cont(new A)
    //val e4 = Cont(new B)
    val x = List(e1,e2 ) //,e3,e4)



    for (i <- x){
      i match {
        case Cont(s: String) => println(s)
        case Cont(a: B) => println("one B")
        case Cont(a: A) => println("one A")
        case _ =>
      }
    }

    def f(args: Cont[Any]*) = {
      for (i <- args)
      {
        println("...")
        println(i.mytype())

        val m = ru.runtimeMirror(getClass.getClassLoader)
        val t1 = i.mytype()

        /*bla
        val classPerson = ru.typeOf[dummy].typeSymbol.asClass
        val cm =  m.reflectClass(classPerson)
        val ctor = t1.declaration(ru.nme.CONSTRUCTOR).asMethod
        val ctorm = cm.reflectConstructor(ctor)
        ctorm()*/

        val uff = t1.typeConstructor
        val kj = fresh[uff]
        val x = bla(i)


        x match {
          case y: Sym[String] => println("sym of string")
          case y: Sym[B] => println("sym one B")
          case y: Sym[A] => println("sym one A")
        }
        println(x)
      }
    }


    f(e1,e2)//,e3,e4)


/*
    trait DSL extends PrimitiveOpsExp {


      def simpleplus(a: Rep[Double], b: Rep[Double]): Rep[Double] = {

        infix_+(a, b)
      }

    }

    class mydsl extends DSL
    val foo = new mydsl


    val a = foo.Const(10.0)
    val b = foo.Const(10.0)

    foo.simpleplus(a, b) //stage here
*/


  }
}
*/
