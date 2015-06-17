package test

import scala.annotation.tailrec
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.lms.internal._


case class CodeDescriptor(
                           nodes_per_block: Int,
                           max_args: Int
                           )

trait GenRandomOps extends ExposeRepBase{

  case class cTP(sym: Any, tag: GenTypes)

  type GenTypes = String

  type AvailOps = Map[Set[GenTypes], Op]
  type AvailUniqueTypes = Set[GenTypes]
  type AvailTypeTuples = Set[AvailUniqueTypes]


  case class Op(name: String, desc: OpDescription)


  case class OpDescription(args: Vector[GenTypes],
                           returns: Vector[GenTypes],
                           rf: Vector[Any] => Vector[Any],
                           sf: Vector[Any] => Vector[Any])


  case class Instructions(val syms: Vector[cTP])

  case class FNest(syms: Vector[cTP], f: Vector[cTP] => Vector[cTP], sf: Vector[cTP] => Vector[cTP])

  def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = availTypes

  def ops(availOps: AvailOps): AvailOps = availOps

  lazy val allops = ops(Map.empty)

  def filterOps(availTypes: AvailUniqueTypes): AvailOps = {
    val currops = allops.filter(x => x._1.subsetOf(availTypes))
    currops
  }

  def findAllIndex(targettype: GenTypes, options: Vector[GenTypes]): Vector[Int] = {
    options.zipWithIndex.filter(p => p._1 == targettype).map(e => e._2)
  }

  //create a Vector of random cTPs
  //its a vector cause we might require a certain type combination so that our ops work
  //e.g. if we only have the plus(l: T, r: Q) operation we require that we have a T and a Q in the args
  def genArg(): Gen[Vector[cTP]] = for {
    typechoice <- Gen.oneOf(supported_types(Set.empty).toSeq)
  } yield typechoice.map(c => cTP(null, c)).toVector


  //creates n Args - since a single "arg" might be a tuple of args we simple sample n tuples and then only consider the first
  //m args such that the total size is smaller then n
  def genArgs(size: Int): Gen[Vector[cTP]] =
    for {
      args <- Gen.containerOfN[Vector, Vector[cTP]](size, genArg)
    } yield {
      args.foldLeft(Vector.empty[cTP]) {
        (acc, ele) => {
          val p1 = acc ++ ele
          if (p1.size > size)
            acc
          else
            p1
        }
      }
    }

  def genOp(fnest: FNest): Gen[Op] = {
    val availTypes: AvailUniqueTypes = fnest.syms.map(e => e.tag).toSet
    val availOps = filterOps(availTypes)
    for {
      randomop <- Gen.oneOf(availOps.toSeq)
    } yield randomop._2
  }

  def genAssignment(fNest: FNest, targetType: GenTypes): Gen[Int] = {
    val possible_targets = fNest.syms.map(e => e.tag).zipWithIndex.filter(e => e._1 == targetType)
    for {
      choice <- Gen.oneOf(possible_targets)
    } yield choice._2
  }

  def genFNest(fnest: FNest): Gen[FNest] = {
    for {
      op <- genOp(fnest)
      assign <- Gen.sequence[Vector[Int], Int](op.desc.args.map(arg => genAssignment(fnest, arg)))
    } yield {
      val f: (Vector[cTP] => Vector[cTP]) = (in: Vector[cTP]) => {
        val argsyms: Vector[Any] = assign.foldLeft(Vector.empty[Any]) {
          (acc, ele) => {
            acc :+ in(ele).sym
          }
        }
        val ret: Vector[Any] = op.desc.rf(argsyms)
        val retctp: Vector[cTP] = ret.zipWithIndex.map(e => cTP(e._1, op.desc.returns(e._2)))
        in ++ retctp
      }
      val sf: (Vector[cTP] => Vector[cTP]) = (in: Vector[cTP]) => {
        val argsyms: Vector[Any] = assign.foldLeft(Vector.empty[Any]) {
          (acc, ele) => {
            acc :+ in(ele).sym
          }
        }
        val ret: Vector[Any] = op.desc.sf(argsyms)
        val retctp: Vector[cTP] = ret.zipWithIndex.map(e => cTP(e._1, op.desc.returns(e._2)))
        in ++ retctp
      }
      FNest(fnest.syms ++ op.desc.returns.map(e => cTP(null, e)), f,sf)
    }
  }

  def genCode(desc: CodeDescriptor): Gen[Vector[FNest]] = {
    for {
      ini <- genArgs(desc.max_args)
      tail <- genNodes(desc, Vector(FNest(ini, null, null)))
    } yield tail
  }

  def genNodes(desc: CodeDescriptor, ini: Vector[FNest]): Gen[Vector[FNest]] = {
    if (desc.nodes_per_block > 0) {
      for {
        next <- genFNest(ini.last)
        tail <- genNodes(desc.copy(nodes_per_block = desc.nodes_per_block - 1), ini :+ next)
      } yield tail
    }
    else ini
  }






  def chainHeadsf(v: Vector[FNest]): (Vector[cTP] => Vector[cTP]) = {
    if (v.tail.isEmpty)
      assert(false, "seems there are no instructions")
    val f: Vector[cTP] => Vector[cTP] = (in: Vector[cTP]) => {
      chainsf(v.tail, in)
    }
    f
  }
  @tailrec
  private def chainsf(v: Vector[FNest], res: Vector[cTP]): Vector[cTP] =
    if (v.isEmpty) res else chainsf(v.tail, v.head.f(res))


  def chainHeadf(v: Vector[FNest]): (Vector[cTP] => Vector[cTP]) = {
    if (v.tail.isEmpty)
      assert(false, "seems there are no instructions")
    val f: Vector[cTP] => Vector[cTP] = (in: Vector[cTP]) => {
      chainf(v.tail, in)
    }
    f
  }
  @tailrec
  private def chainf(v: Vector[FNest], res: Vector[cTP]): Vector[cTP] =
    if (v.isEmpty) res else chainf(v.tail, v.head.f(res))


  def genTypeInstance(targetcTP: cTP): Gen[cTP] =
    targetcTP.tag match {
      case "Boolean" => for {
        choice <- Gen.oneOf(true, false)
      } yield targetcTP.copy(sym = choice)
      case "Int" => for {
        choice <- Arbitrary.arbitrary[Int]
      } yield targetcTP.copy(sym = choice)
      case _ => {
        assert(false, "seems we are missing a type instance generator for type " + targetcTP.tag)
        ???
      }
    }

  def genArgInstances(v: Vector[cTP]): Gen[Vector[cTP]] = {
    val t = v.map(e => {
      for {
        instance <- genTypeInstance(e)
      } yield instance
    })
    for {
      ge <- Gen.sequence[Vector[cTP], cTP](t)
    } yield ge //.zipWithIndex.map(e => cTP(e._1,v(e._2).tag))
  }




  def genExposeRep(v: Vector[cTP]): ExposeRep[Vector[cTP]] = {
    new ExposeRep[Vector[cTP]]{
      val freshExps = (u: Unit) => Vector.empty
      val vec2t = (v: Vector[Exp[_]]) => Vector.empty
      val t2vec = (x: Vector[cTP]) => Vector.empty
    }
  }

}


/*def chooseRandomParam(targettype: GenTypes, options: Vector[GenTypes]): Gen[Int] = {
  val tmatches = findAllIndex(targettype, options)
  Gen.oneOf(tmatches).map(choice => choice)
}

def chooseParams(avInputs: Vector[GenTypes], op: Op): Gen[Vector[Int]] = {
  val mapping = op._2.args.foldLeft(Vector.empty[Gen[Int]]) {
    (acc, ele) => {
      acc :+ chooseRandomParam(ele, avInputs)
    }
  }
  sequence(mapping)
  ???
}


def genNextNode(avInputs: Vector[GenTypes], op: Op): Gen[Vector[GenTypes]] = {
  val randomparas = chooseParams(avInputs, op)

  /*  randomparas.map(x => {
     op._2.rf()
    })*/
  ???
}


def genTypes(max: Int): Gen[Vector[GenTypes]] = {
  for {
    longlist <- Gen.listOfN(max, genTypeTuple())
  } yield {
    val cc = longlist.foldLeft(Vector.empty[GenTypes])((acc, ele) => {
      if (acc.length + ele.size > max)
        acc
      else {
        val t1: Vector[GenTypes] = ele.toVector
        val t: Vector[GenTypes] = acc ++ t1
        t
      }
    })
    cc
  }
}

def genTypeTuple(): Gen[Set[GenTypes]] = {
  val all = supported_types(Set.empty)
  val t = for {
    i <- Gen.oneOf(all.toSeq)
  } yield i
  t
}


def createInitalTP(args: Vector[GenTypes]): Vector[cTP] = for (a <- args) yield cTP(null, a)

def GenBase(): Gen[FNest] = {
  val regular: (Option[FNest] => (Instructions => Instructions)) = (body: Option[FNest]) => {
    val f: Instructions => Instructions = (in: Instructions) => {
      println("doing stuff - base case")
      in
    }
    f
  }
  FNest(None, regular)
}

def GenRandomInstr(nr: Int): Gen[FNest] = {

  for {
    recurse <- if (nr > 0) GenRandomInstr(nr - 1) else GenBase()
  }
    yield {
      val regular: (Option[FNest] => (Instructions => Instructions)) = (body: Option[FNest]) => {
        val f: Instructions => Instructions = (in: Instructions) => {
          println("doing stuff " + nr)
          in
        }
        f
      }
      FNest(Some(recurse), regular)
    }
}

//createInitalTP(args)
def GenF(args: Vector[GenTypes]): Gen[FNestRoot] = {
  for {
    instr <- GenRandomInstr(5)
  }
    yield {
      val empty = Instructions(Vector.empty)
      val regular: (FNest => (Instructions => Instructions)) = (body: FNest) => {
        val f: Instructions => Instructions = (in: Instructions) => {
          val ini = empty.copy(syms = in.syms)
          val body = instr.regularf(ini)
          body
        }
        f
      }
      FNestRoot(instr, regular)
    }
}*/



