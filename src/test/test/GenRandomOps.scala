package test

import scala.annotation.tailrec
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.lms.internal._

class FunctionMarker

case class CodeDescriptor(
                           max_nodes_per_block: Int,
                           max_toplevel_args: Int,
                           max_args: Int,
                           max_nest_depth: Int,
                           max_functions: Int,
                           max_returns: Int,
                           max_calls: Int,
                           cur_nodes_per_block: Int = 0,
                           cur_nest_depth: Int = 0
                           )

trait GenRandomOps extends ExposeRepBase with FunctionsExp{

  case class cTP(sym: Any, tag: GenTypes)
  var funexp2StagedFunction: Map[Exp[_], StagedFunction[_,_]] = Map.empty
  var dynOpNrTimesUsed: Map[Op,Int] = Map.empty

  class Wildcard1 //using this for generic Types e.g. foo[T](x : T)
  class Wildcard2 //using this for generic Types e.g. foo[T](x : T)
  class Wildcard3 //using this for generic Types e.g. foo[T](x : T)
  class Wildcard4 //using this for generic Types e.g. foo[T](x : T)
  class Wildcard5 //using this for generic Types e.g. foo[T](x : T)
  class Wildcard6 //using this for generic Types e.g. foo[T](x : T)
  class Wildcard7 //using this for generic Types e.g. foo[T](x : T)

  implicit def mani2cTPType(mf: Manifest[_]): cTPType = cTPType(mf)
  case class cTPType(mf: Manifest[_],dynTags: Option[ Unit => (Vector[TypeRep[_]],Vector[TypeRep[_]])] = None)

  type GenTypes = cTPType

  type AvailOps = Map[Set[GenTypes], Set[Op]]
  type AvailUniqueTypes = Set[GenTypes]
  type AvailTypeTuples = Set[AvailUniqueTypes]


  case class Op(name: String, desc: OpDescription, instanciate: Option[Vector[Int] => Op] = None)
  case class OpDescription(args: Vector[GenTypes],
                           returns: Vector[GenTypes],
                           rf: Vector[Any] => Vector[Any],
                           sf: Vector[Any] => Vector[Any],
                           localfidx: Option[Int] )


  case class Instructions(val syms: Vector[cTP])
  case class FNest(syms: Vector[cTP], f: Vector[cTP] => Vector[cTP], sf: Vector[cTP] => Vector[cTP], localfs: AvailOps)

  def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = availTypes

  def ops(availOps: AvailOps): AvailOps = availOps

  def registerOp(newop: Op, sofar: AvailOps): AvailOps = {
    val uniqueArgs = newop.desc.args.toSet
    val withoutWildcards = uniqueArgs.filter(t => !isWildCard(t))
    val entrysofar = sofar.get(withoutWildcards)
    val newentry = if (entrysofar.isDefined) entrysofar.get + newop else Set(newop)
    sofar + (withoutWildcards -> newentry)
  }

  lazy val allops = ops(Map.empty)


  /**
   * This takes the list of all available ops and filters out those that can be called with the current set of variables
   * @param availTypes the types of the currently existing variables
   * @return returns a Map of all Ops that could be called
   */
  def filterOps(availTypes: AvailUniqueTypes, fNest: FNest): AvailOps = {
    val fixedops = allops.filter(x => x._1.subsetOf(availTypes))
    val dynops = fNest.localfs.filter(x => x._1.subsetOf(availTypes))
    val currops = fixedops ++ dynops.map{ case (k,v) => {
      val other: Set[Op] = fixedops.getOrElse(k,Set.empty[Op])
      val value: Set[Op] = (v ++ other)
      k -> value
      }
    }
    currops
  }

  def findAllIndex(targettype: GenTypes, options: Vector[GenTypes]): Vector[Int] = {
    options.zipWithIndex.filter(p => p._1 == targettype).map(e => e._2)
  }


  /**
   * create a Vector of random cTPs
   * its a vector cause we might require a certain type combination so that our ops work
   * e.g. if we only have the plus(l: T, r: Q) operation we require that we have a T and a Q in the args
   * @return
   */
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

  //override in traits that would nest to check if its a valid op in the context
  def filterNestDepth(desc: CodeDescriptor, fnest: FNest, op: Op): Boolean = {
    true
  }


  def genOp(desc: CodeDescriptor, fnest: FNest): Gen[Op] = {
    val availTypes: AvailUniqueTypes = fnest.syms.map(e => e.tag).toSet
    val availOps = filterOps(availTypes, fnest)
    val flattened = availOps.flatMap(opset => opset._2)
    val nestcheck = flattened.filter(p => {
      filterNestDepth(desc,fnest,p) &&
      (dynOpNrTimesUsed.get(p).isEmpty || dynOpNrTimesUsed.get(p).get < desc.max_calls) //checking that a dynamic function is only called max_calls times
    })
    for {
      randomop <- Gen.oneOf(nestcheck.toSeq)
    } yield {
      val t = randomop
      if(t.desc.localfidx.isDefined) //if we selected a dynamic function - increase the call counter
        dynOpNrTimesUsed = {
          if (dynOpNrTimesUsed.get(t).isDefined)
            dynOpNrTimesUsed + (t -> (dynOpNrTimesUsed(t) + 1) )
          else
            dynOpNrTimesUsed + (t ->  1)
        }
      t
    }
  }


  private def isWildCard(targetType: GenTypes): Boolean = {
      targetType.mf == manifest[Wildcard1] ||
      targetType.mf == manifest[Wildcard2] ||
      targetType.mf == manifest[Wildcard3] ||
      targetType.mf == manifest[Wildcard4] ||
      targetType.mf == manifest[Wildcard5] ||
      targetType.mf == manifest[Wildcard6] ||
      targetType.mf == manifest[Wildcard7]
  }

  private def WildCardNr(targetType: GenTypes): Int = {
    if (targetType.mf == manifest[Wildcard1]) 1
    else if(targetType.mf == manifest[Wildcard2]) 2
    else if(targetType.mf == manifest[Wildcard3]) 3
    else if(targetType.mf == manifest[Wildcard4]) 4
    else if(targetType.mf == manifest[Wildcard5]) 5
    else if(targetType.mf == manifest[Wildcard6]) 6
    else if(targetType.mf == manifest[Wildcard7]) 7
    else { assert(false, "invalid wildcard")
    0
    }
  }


  private def checkTypeCompatibility(ctype: GenTypes, targetType: GenTypes): Boolean = {
    ctype == targetType || isWildCard(targetType)
  }

  def genAssignment(fNest: FNest, targetType: GenTypes): Gen[Int] = {
    val possible_targets = fNest.syms.map(e => e.tag).zipWithIndex.filter(e => checkTypeCompatibility(e._1,targetType))
    for {
      choice <- Gen.oneOf(possible_targets)
    } yield choice._2
  }



  //this checks if the op we randomly selected the creation of a function literal and then replaces the placeholder with an actual symbol
  //we do this cause we want the function to only take parameters that are also currently available to increase the liklyhood of it being used
  //the actual implemention is within the GenRandomFunctions trait
  def createFunction(desc: CodeDescriptor, op: Op, fNest: FNest): Gen[(Op,Option[(Vector[cTP],Vector[cTP])])] = {
    (op,None)
  }


  def removeWildCards(op: Op, fNest: FNest): Op = {
    val (wassign, newargs) = op.desc.args.foldLeft((Map.empty[GenTypes,GenTypes],Vector.empty[GenTypes]))((acc,ele) => {
      val (wcards,assigns) = acc
      if (isWildCard(ele)){ //to we deal with a wild car
        val wassin = wcards.get(ele)
        if (wassin.isDefined) //if so - did we assign a type to it already
        {
          val assignedwildcard: GenTypes = wcards(ele)
          (wcards,assigns :+ assignedwildcard) //use the assigned type
        }
        else{
          val assignedwildcard: GenTypes = genAssignment(fNest,ele).map(symnr => fNest.syms(symnr).tag).sample.get
          //not so nice to sample here....
          (wcards + (ele -> assignedwildcard),assigns :+ assignedwildcard) //use the assigned type
        }
      }
      else
      {
        (wcards,assigns :+ ele)
      }
    })
    //at this point we removed all Wildcards in the Args - now we also need to assign them in the returns
    val rassigns = op.desc.returns.foldLeft(Vector.empty[GenTypes])((acc,ele) => {
      if (isWildCard(ele)){ //to we deal with a wild car
      val wassin = wassign.get(ele)
        if (wassin.isDefined)        {
          val assignedwildcard: GenTypes = wassign(ele)
          acc :+ assignedwildcard //use the assigned type
        }
        else{
          assert(false,"Wildcard in Return Type that was not assigned in Args!")
          ???
        }
      }
      else {
        acc :+ ele
      }
    })

    val newdesc = op.desc.copy(args = newargs, returns = rassigns)
    val newop = op.copy(desc = newdesc)
    newop
  }

  def genFNest(desc: CodeDescriptor, fnest: FNest): Gen[FNest] = {
    for {
      wop <- genOp(desc,fnest)
      (fop,nf) <- createFunction(desc,wop,fnest)
      op <- removeWildCards(fop, fnest)
      assign <- Gen.sequence[Vector[Int], Int](op.desc.args.map(arg => genAssignment(fnest, arg)))
    } yield {
      val f: (Vector[cTP] => Vector[cTP]) = (in: Vector[cTP]) => {
        val argsyms: Vector[Any] = assign.foldLeft(Vector.empty[Any]) {
          (acc, ele) => {
            acc :+ in(ele).sym
          }
        }
        val ret: Vector[Any] =
          if (op.desc.localfidx.isDefined) {
            val fplusargsyms = in(op.desc.localfidx.get).sym +: argsyms
            //op.desc.sf(fplusargsyms)
            op.desc.rf(fplusargsyms)
          }
          else op.desc.rf(argsyms)
        //val ret: Vector[Any] = op.desc.rf(argsyms)
        val retctp: Vector[cTP] = ret.zipWithIndex.map(e => cTP(e._1, op.desc.returns(e._2)))
        in ++ retctp
      }
      val stagedf: (Vector[cTP] => Vector[cTP]) = (in: Vector[cTP]) => {
        val argsyms: Vector[Any] = assign.foldLeft(Vector.empty[Any]) {
          (acc, ele) => {
            acc :+ in(ele).sym
          }
        }

        val ret: Vector[Any] =
          if (op.desc.localfidx.isDefined) {
            val fplusargsyms = in(op.desc.localfidx.get).sym +: argsyms
            op.desc.sf(fplusargsyms)
          }
          else op.desc.sf(argsyms)
        val retctp: Vector[cTP] = ret.zipWithIndex.map(e => cTP(e._1, op.desc.returns(e._2)))

        in ++ retctp
      }
      if (nf.isDefined)
      {
        val localf = op.desc.returns.map(e => cTP(null, e)).head
        //we know here that the returned symbol is a local function
        val (args,returns) = nf.get


        val functionvaridx = fnest.syms.size //== index of the function

          //val functiontp = x(functionvaridx)
          //val functionuntyped = fun2tp.find(p => p._2 == functiontp).get._1


          val applyop: Op = {
            val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
              val functionsym = x.head
              val inasctp: Vector[cTP] = x.tail.zipWithIndex.map(ele => cTP(ele._1,args(ele._2).tag))
              val lamops = functionsym.asInstanceOf[Vector[cTP] => Vector[cTP]]
              val res = lamops(inasctp)
              val withouttag: Vector[Any] = res.map(ele => ele.sym)
              withouttag
            }
            val sf: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
              val functionexp = x.head //we construct function applys such that the function is always the first arg
              val functiontp: TP[_] = if (!functionexp.isInstanceOf[Exp[_]])
              {
                println("TIETLJKAJEDGKJ")
                ???
              }
              else
               exp2tp.get(functionexp.asInstanceOf[Exp[_]]).get
              val stagedFunction = funexp2StagedFunction(functiontp.sym)
              val inasctp: Vector[cTP] = x.tail.zipWithIndex.map(ele => cTP(ele._1,args(ele._2).tag))
              val lamops = toLambdaOps(stagedFunction).asInstanceOf[LambdaOps[Vector[cTP],Vector[cTP]]]
              //val functiontyped = functionuntyped.asInstanceOf[Vector[cTP] => Vector[cTP]]
              //val res: Vector[cTP] = ??? //functiontyped(inasctp)
              val res: Vector[cTP] = lamops(inasctp) //functiontyped(inasctp)
              val withouttag: Vector[Any] = res.map(ele => ele.sym)
              withouttag
            }
            val returntypes = returns.map(e => e.tag)
            /*println ("returns .... ->")
            println(returntypes)
            println("-----------------")*/
            val op = OpDescription(args.map(e => e.tag),returntypes,f,sf,Some(functionvaridx))
            Op("apply"+functionvaridx, op)
          }
        FNest(fnest.syms ++ op.desc.returns.map(e => cTP(null, e)), f,stagedf, registerOp(applyop,fnest.localfs))
      }
      else {
        /*println("------------------")
        println(op)
        println(op.desc.returns)
        println("------------------")*/
        FNest(fnest.syms ++ op.desc.returns.map(e => cTP(null, e)), f,stagedf, fnest.localfs)
      }

    }
  }

  //used to limit the number of returns
  def genReturns(desc: CodeDescriptor, curfnest: Vector[FNest]): Gen[Vector[FNest]] = {
    for {
      nrrets <- Gen.chooseNum(1,desc.max_returns)
      indicies2 <- Gen.pick(nrrets,curfnest.last.syms.zipWithIndex)
    } yield {
      val indicies = indicies2.map(ele => ele._2).toVector
      val lastfnest = curfnest.last
      val syms = indicies.foldLeft(Vector.empty[cTP]) {
        (acc,ele) => acc :+ lastfnest.syms(ele)
      }
      val f: Vector[cTP] => Vector[cTP] = (in: Vector[cTP]) => {
        indicies.foldLeft(Vector.empty[cTP]) {
          (acc,ele) => acc :+ in(ele)
        }
      }
      curfnest :+ FNest(syms,f,f,lastfnest.localfs)
    }
  }

  def genCode(desc: CodeDescriptor): Gen[Vector[FNest]] = {
    for {
      ini <- genArgs(desc.max_toplevel_args)
      tailuf <- genNodes(desc, Vector(FNest(ini, null, null,Map.empty)))
      tail <- genReturns(desc,tailuf)
    } yield tail
  }


  implicit val shrinkCodeX: Shrink[Vector[FNest]] = Shrink({
      case v: Vector[FNest] => {
        println("shrinking code nest")
        if (v.isEmpty) Stream.empty else Stream(v.dropRight(1))
      }
    })




  def genNodes(desc: CodeDescriptor, ini: Vector[FNest]): Gen[Vector[FNest]] = {
    if (desc.cur_nodes_per_block < desc.max_nodes_per_block) {
      for {
        next <- genFNest(desc, ini.last)
        tail <- genNodes(desc.copy(cur_nodes_per_block = desc.cur_nodes_per_block + 1), ini :+ next)
      }
        yield tail
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

  private def chainsf(v: Vector[FNest], res: Vector[cTP]): Vector[cTP] = {

    if (v.isEmpty) res else {
      val headres = v.head.sf(res)
      chainsf(v.tail, headres)
    }
  }

  /*@tailrec
  private def chainsf(v: Vector[FNest], res: Vector[cTP]): Vector[cTP] =
    if (v.isEmpty) res else chainsf(v.tail, v.head.sf(res))
*/

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


  def genTypeInstance(targetcTP: cTP): Gen[cTP] = {
    val boolm: String = manifest[Boolean].toString()
    val intm: String = manifest[Int].toString()
    val target:String = targetcTP.tag.mf.toString()
    target match {
      case `boolm` => {
        //println("this is supposed to be bool: " + boolm + " " + target)
        for {
          choice <- Gen.oneOf(true, false)
        } yield targetcTP.copy(sym = choice)
      }
      case `intm` => for {
        choice <- Arbitrary.arbitrary[Int]
      } yield targetcTP.copy(sym = choice)
      case _ => {
        assert(false, "seems we are missing a type instance generator for type " + targetcTP.tag)
        ???
      }
    }
    }
  def genArgInstances(v: Vector[cTP]): Gen[Vector[cTP]] = {
    val t = v.map(e => {
      for {
        instance <- genTypeInstance(e)
      } yield instance
    })
    val t1 = t
    val r = for {
      ge <- Gen.sequence[Vector[cTP], cTP](t1)
    } yield ge //.zipWithIndex.map(e => cTP(e._1,v(e._2).tag))
    r
  }


  //Introduced this just to avoid that scalacheck shrinks that vector (which doesnt make sense)
  case class StealthIt(x: Vector[cTP])
  def hideit(v: Vector[cTP]): Gen[StealthIt] = for {
    x <- genArgInstances(v)
   } yield {
      val t = StealthIt(x)
      t
    }


  def genExposeRep(v: Vector[cTP]): ExposeRep[Vector[cTP]] = {
    val tags = v.map(e => e.tag)
    new ExposeRep[Vector[cTP]]{
      val freshExps = (u: Unit) => {
        def helpert[T](args: Vector[TypeRep[_]], returns: Vector[TypeRep[_]])(implicit tag: TypeRep[T]): TypeRep[T] = {
          tag match {
            case x@TypeExp(mf,dynTags) => {
              val f: Unit => (Vector[TypeRep[_]],Vector[TypeRep[_]]) = (u: Unit) => {
                //(Vector.empty[TypeExp[_]],Vector.empty[TypeExp[_]])
                (args,returns)
              }
              x.copy(dynTags = Some(f))
            }
            case _ => {
              assert(false, "this should never match")
              tag
            }
          }
        }
        tags.foldLeft(Vector.empty[Exp[_]])((acc,ele) => {
          ele.dynTags match {
            case Some(ftags) => {
              val (args,rets) = ftags()
              val tagnew: TypeRep[_=>_] = helpert(args,rets)
              val exp = Arg[_=>_](tagnew)
              acc :+ exp
            }
            case None => acc :+ Arg(ele.mf)
          }
        })
      }
      val vec2t: Vector[Exp[_]] => Vector[cTP] =
      //(v: Vector[Exp[_]]) => Vector.empty
        (v: Vector[Exp[_]]) => v.foldLeft(Vector.empty[cTP])( (acc,ele) => acc :+ cTP(ele,exp2tp(ele).tag.mf))
      val t2vec: Vector[cTP] => Vector[Exp[_]] =
          //(x: Vector[cTP]) => Vector.empty
        (x: Vector[cTP]) => x.foldLeft(Vector.empty[Exp[_]])( (acc,ele) => {
          if (!ele.sym.isInstanceOf[Exp[_]])
            assert(false, "cast error")
          acc :+ ele.sym.asInstanceOf[Exp[_]]
        })
    }
  }

  def resetRandom () = {
    funexp2StagedFunction = Map.empty
    dynOpNrTimesUsed = Map.empty
  }

}



