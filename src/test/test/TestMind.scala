/*
package test

import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks


import org.scalacheck._
import Gen._

import scala.annotation.tailrec


class TestMind extends PropSpec with PropertyChecks {

  trait Tree[T]{
    def size: Int
  }
  case class Leaf[T](item: T) extends Tree[T] { def size = 1}
  case class Node[T](children: List[Tree[T]]) extends Tree[T] { def size = children.map(_.size).sum }




  def genTree[T](genT: Gen[T]): Gen[Tree[T]] = lzy {oneOf(genLeaf(genT), genNode(genT))}
  def genLeaf[T](genT: Gen[T]): Gen[Leaf[T]] = genT map (Leaf(_))
  def genNode[T](genT: Gen[T]): Gen[Node[T]] = listOf(genTree(genT)).map( bla => Node(bla))




  def genRandomSize(): Gen[Int] = {
    for {
      size <- Gen.choose(1,5)
    }
      yield size
  }

  def genRandomV(size: Int): Gen[Vector[Int]] = Gen.containerOfN[Vector,Int](size,Gen.chooseNum(-50,50))

  def genRandomV(): Gen[Vector[Int]] = {
    for {
      size <- genRandomSize()
      v <- genRandomV(size)
    } yield v
  }

  def plusV(v: Vector[Int]): Gen[Vector[Int]] = {
    val sum = v.foldLeft(0)((acc,ele) => {
      if (ele > 0) acc + ele else acc
    })
    val const: Vector[Int] = v :+ sum
    const
  }

  def genRandomVP(): Gen[Vector[Int]] = genRandomV().flatMap( v => plusV(v))


  case class FNest(syms: Vector[cTP], f: Vector[cTP] => Vector[cTP])

  type GenTypes = String
  //type myT = String
  case class cTP(sym: Any, tag: GenTypes)
  def genRandomcTP(): Gen[cTP] = Gen.oneOf("Boolean","Int").map( choice => cTP(null,choice))
  def genRandomcTPV(size:Int): Gen[Vector[cTP]] = Gen.containerOfN[Vector,cTP](size,genRandomcTP())
  def genRandomcTPV(): Gen[Vector[cTP]] = genRandomSize().flatMap(size => genRandomcTPV(size))


  def pluscTP(v: Vector[cTP]) = {
    var ints = 0
    var bools = 0
    for( e <- v){
      if (e.tag == "Boolean")
        bools = bools + 1
      if (e.tag == "Int")
        ints = ints + 1
    }
    val newele = if (ints > bools) cTP(null,"Int") else cTP(null,"Boolean")
    v :+ newele
  }

  def rpluscTP(v: Vector[cTP]) = {
    for {
      choice <- Gen.oneOf(cTP(null,"Int") , cTP(null,"Boolean"))
    } yield v :+ choice
  }


  def genRandomcTPVP(): Gen[Vector[cTP]] = {
    for {
      ini <- genRandomcTPV()
      inip <- rpluscTP(ini)
    } yield (inip)
  }


  def randIndex(fnest: FNest): Gen[Int] = {
    for {
      max <- Gen.chooseNum(0,fnest.syms.size)
    } yield max
  }



  case class Op(name: String, desc: OpDescription)
  type AvailOps = Map[Set[GenTypes], Op]
  type AvailUniqueTypes = Set[GenTypes]
  type AvailTypeTuples = Set[AvailUniqueTypes]
  case class OpDescription(args: Vector[GenTypes],
                           returns:Vector[GenTypes],
                           rf: Vector[Any] => Vector[Any])


  val negate: AvailOps = {
    val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
      val t = x.head.asInstanceOf[Boolean]
      Vector(!t)
    }
    val sf: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
      val t = x.head.asInstanceOf[Boolean]
      Vector(!t)
    }
    val op = OpDescription(Vector("Boolean"),Vector("Boolean"),f)
    Map(Set("Boolean") -> Op("boolean_negate", op))
  }

  val plus: AvailOps = {
    val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
      val a = x.head.asInstanceOf[Int]
      val b = x.tail.head.asInstanceOf[Int]
      Vector(a+b)
    }
    val op = OpDescription(Vector("Int", "Int"),Vector("Int"),f)
    Map(Set("Int") -> Op("int_plus",op))
  }

  val allops = plus ++ negate

  def filterOps(availTypes: AvailUniqueTypes): AvailOps = {
    val currops = allops.filter( x => x._1.subsetOf(availTypes))
    currops
  }


  def chooseRandOp(fnest: FNest) : Gen[Op] = {
    val availTypes: AvailUniqueTypes = fnest.syms.map(e => e.tag).toSet
    val availOps = filterOps(availTypes)
    for {
      randomop <- Gen.oneOf(availOps.toSeq)
    } yield randomop._2
  }


  def randAssign(fNest: FNest, targetType: GenTypes): Gen[Int] = {
    val possible_targets = fNest.syms.map(e => e.tag).zipWithIndex.filter(e => e._1 == targetType)
    for {
      choice <- Gen.oneOf(possible_targets)
    } yield choice._2
  }


  def assignRandOp(fnest: FNest): Gen[FNest] = {
    for {
      op <- chooseRandOp(fnest)
      assign <- Gen.sequence[Vector[Int],Int](op.desc.args.map(arg => randAssign(fnest,arg) ))
    } yield {
      val f: (Vector[cTP] => Vector[cTP]) = (in: Vector[cTP]) => {
        val argsyms: Vector[Any] = assign.foldLeft(Vector.empty[Any]){
          (acc,ele) => {
            acc :+ in(ele).sym
          }
        }
        val ret: Vector[Any] = op.desc.rf(argsyms)
        val retctp: Vector[cTP] = ret.zipWithIndex.map(e => cTP(e._1,op.desc.returns(e._2)))
        in ++ retctp
      }
      FNest(fnest.syms ++ op.desc.returns.map(e => cTP(null,e)), f)
    }
  }


  def nSteps(n: Int): Gen[Vector[FNest]] = {
    for {
      ini <- genRandomcTPV()
      tail <- nTailSteps(n,Vector(FNest(ini,null)))
      //first <- assignRandOp(FNest(ini,null))
    } yield tail
  }

  def nTailSteps(n: Int, ini: Vector[FNest]): Gen[Vector[FNest]] = {
    if (n > 0)
    {
      for {
        next <- assignRandOp(ini.last)
        tail <- nTailSteps(n-1,ini :+ next)
      } yield tail
    }
    else
    {
      ini
    }
  }



  def oneStep(): Gen[Vector[FNest]] = {
    for {
      ini <- genRandomcTPV()
      first <- assignRandOp(FNest(ini,null))
    } yield Vector(FNest(ini,null), first)
  }

  def chainHead(v: Vector[FNest]): (Vector[cTP] => Vector[cTP]) = {
    if (v.tail.isEmpty)
      assert(false, "seems there are no instructions")
    val f: Vector[cTP] => Vector[cTP] = (in: Vector[cTP]) => {
      chain(v.tail,in)
    }
    f
  }

  @tailrec
  private def chain(v: Vector[FNest],res: Vector[cTP]): Vector[cTP] =
    if (v.isEmpty) res else chain(v.tail,v.head.f(res))



  def genInt(): Gen[Int] = Arbitrary.arbitrary[Int]
  def genBool(): Gen[Boolean] = Arbitrary.arbitrary[Boolean]

  def genRandArgs(v: Vector[cTP]): Gen[Vector[cTP]] = {
    val t = v.map (e => e.tag match {
      case "Boolean" => genBool()
      case "Int" => genInt()
      case _ => {
        assert(false, "seems we dont match all possible types")
        genInt()
      }
    })
    for {
      ge <- Gen.sequence[Vector[AnyVal],AnyVal](t)
    } yield ge.zipWithIndex.map(e => cTP(e._1,v(e._2).tag))
  }




  println("hello dude")

  println(genRandomVP().sample)
  println(genRandomcTPVP().sample)

  val test1 = nSteps(10).sample.get
  val inisyms = test1.head.syms
  val rargs = genRandArgs(inisyms).sample.get

  val callstack: (Vector[cTP] => Vector[cTP]) = chainHead(test1)
  println("args!")
  println(rargs)
  println("args + result")
  println(callstack(rargs))
}
*/
