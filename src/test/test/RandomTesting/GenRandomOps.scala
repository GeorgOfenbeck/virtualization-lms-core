package RandomTesting

import scala.annotation.tailrec
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.lms.internal._


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

/*

 trait TypedValueorSymbol[T] {
   def getManifest(x: T): Manifest[_]
 }

 case class TV[T](x: T)(implicit val mf: Manifest[T])


 implicit object TypedSymbol extends TypedValueorSymbol[TP[_]]{
  def getManifest(x: TP[_]): Manifest[_] = x.tag.mf
 }
 implicit object TypedValue extends TypedValueorSymbol[TV[_]]{
  def getManifest(x: TV[_]): Manifest[_] = x.mf
 }



 /**
   * This takes the list of all available ops and filters out those that can be called with the current set of graph nodes
   * @param availTypes the types of the currently existing variables
   * @return returns a Map of all Ops that could be called
   */
 def filterOps(availTypes: AvailUniqueTypes, graph: IRGraph): AvailOps = {

  //all ops that are statically known
  val fixedops = allops.filter(x => x._1.subsetOf(availTypes))

  //all ops that come into existence due to dynamic definitions of functions
  //val dynops = graph.localfs.filter(x => x._1.subsetOf(availTypes))
  //2DO

  /*val currops = fixedops ++ dynops.map{ case (k,v) => {
   val other: Set[Op] = fixedops.getOrElse(k,Set.empty[Op])
   val value: Set[Op] = (v ++ other)
   k -> value
  }
  }
  currops
  */
  fixedops
 }

 //override in traits that would nest to check if its a valid op in the context
 def filterNestDepth(desc: CodeDescriptor, graph: IRGraph, op: Op): Boolean = {
  true
 }

 /***
   *
   * @param desc Configuration for the random code generation - used in this context is the nest depth
   * @param graph The current graph which is used to sample types available for ops
   * @return A random generator for operations given the current graph and available ops
    */
 def genOp(desc: CodeDescriptor, graph: IRGraph): Gen[Op] = {

  //first get all types that are currently existing in our graph
  val alltypeReps = graph.graph.id2tp.map(tuple => tuple._2.tag)
  val availTypes: AvailUniqueTypes =  alltypeReps.map(e => e.mf).map(m => cTPType(m,None)).toSet

  //then filter out those that we actually have ops for
  val availOps = filterOps(availTypes, graph)
  val flattened = availOps.flatMap(opset => opset._2)

  val nestcheck = flattened.filter(p => { filterNestDepth(desc,graph,p) })
  //2DO - DynOp filter
  for {
   randomop <- Gen.oneOf(nestcheck.toSeq)
  } yield randomop
 }

 //this checks if the op we randomly selected the creation of a function literal and then replaces the placeholder with an actual symbol
 //we do this cause we want the function to only take parameters that are also currently available to increase the liklyhood of it being used
 //the actual implemention is within the GenRandomFunctions trait
 /*def createFunction(desc: CodeDescriptor, op: Op, graph: IRGraph): Gen[(Op,Option[(Vector[cTP],Vector[cTP])])] = {
  (op,None)
 }*/

 private def checkTypeCompatibility(ctype: GenTypes, targetType: GenTypes): Boolean = {
  ctype == targetType //|| isWildCard(targetType) //2DO
 }

 /**
   * @param graph The current graph we pick compatible nodes from
   * @param targetType The type we will use to search for matching nodes within the graph
   * @return The node id within the graph
    */
 def genAssignment(graph: IRGraph, targetType: GenTypes): Gen[Int] = {
  val possible_targets = graph.graph.id2tp.filter(e => {
   val mf = e._2.tag.mf
   val gentype = cTPType(mf,None)
   checkTypeCompatibility(gentype,targetType)
  }).map(r => r._1).toVector
  for {
   choice <- Gen.oneOf(possible_targets)
  } yield choice
 }

  /***
    *
    * @param desc Configuration for the random code generation
    * @param graph The current graph which is used as a base to choose ops and operands for the next graph
    * @return A new GraphIR which is increased by one Op (an Op can also be a function which in turn contains multiple Ops)
    */

 def genIRGraphs(desc: CodeDescriptor, graph: IRGraph): Gen[IRGraph] = {
  for {
   randomOp <- genOp(desc,graph)
   assign <- Gen.sequence[Vector[Int], Int](randomOp.desc.args.map(arg => genAssignment(graph, arg)))
  } yield {
   val unstagedfunction: ( Reification => Reification) = (in: Reification) => {
    val argsyms: Vector[Any] = assign.foldLeft(Vector.empty[Any]) { (acc, ele) => acc :+ in.id2tp(ele).sym }
    //val ret = randomOp.desc.rf(argsyms)


    //val retgraph: Reification = ret
    //val retctp: Vector[cTP] = ret.zipWithIndex.map(e => cTP(e._1, op.desc.returns(e._2)))
    //in ++ retctp

    //ret
    ???


   }

   assign
  }

  ???
 }

 */


trait GenRandomOps extends ExposeRepBase with FunctionsExp {

  /** *
    * @param mf used all over the place to check if a symbol/value is a suitable input to an op
    * @param dynTags this is used in the case of a dynamic function where the mf would only yield Function[Any,Any] and we would
    *                like to get more concrete type info (note - this most likley could be solved more elegantly via reflection)
    * @tparam T The type we try to describe
    */
  case class Tag[T](mf: Manifest[T],
                    dynTags: Option[Unit => (Vector[GenTypes[_]], Vector[GenTypes[_]])] = None)

  /** *
    * alias used to make swapping out of the type representation easier
    */
  type GenTypes[T] = Tag[T]

  /**
    * shortcut type since the unique set of types is used a lot in the code
    */
  type AvailUniqueTypes = Set[GenTypes[_]]

  /**
    * a map collecting all operations possible given a set of input types
    */
  type AvailOps = Map[Set[GenTypes[_]], Set[Op]]

  /**
    * Idendity type used to be able to switch between evaluation and symbolic evaluation (staging) easily
    * @tparam T
    */
  type NoRep[T] = T

  /**
    * SoV - Symbol or Value is used to abstract over either a value (e.g. Int value) or a symbolic value such as Rep[Int]
    * it additionally holds the type of the Symbol explicitly in the field 'tag'
    * @param sym symbol or value
    * @param tag type representation
    * @tparam R higher order type that is either Rep or NoRep
    * @tparam T type of the symbol/value
    */
  case class SoV[R[_], T](val sym: R[T], val tag: GenTypes[T])


  /** *
    * @param name Name used for e.g. debugging
    * @param args the types taken as arguments by the op
    * @param returns the types returned by the op
    * @param evaluation a function evaluating the op
    * @param symbolic_evaluation a function symbolically (staging) the op
    * @param localfidx in case that op is a dynamic created op this stores which symbol nr it is
    */
  case class Op(name: String,
                args: Vector[GenTypes[_]],
                returns: Vector[GenTypes[_]],
                evaluation: Vector[SoV[NoRep, _]] => Vector[SoV[NoRep, _]],
                symbolic_evaluation: Vector[SoV[Rep, _]] => Vector[SoV[Rep, _]],
                localfidx: Option[Int]
               )

  lazy val allops = ops(Map.empty)

  /**
    * This is used to store a current set of input symbol/values and its extending function
    * @param syms all currently available symbols
    * @param evaluation given the current symbols executes the function and returns an extended set of symbols
    * @param dynamically_defined_functions during the evaluation steps of previous iterations functions might have been created
    *                                      which should also be exposed as available ops - those functions are store in this field
    * @tparam S this decideds if the iterations describe a straightforward evaluation or its symbolic counterpart
    */
  case class EvalGenIterStep[S[_]](syms: Vector[SoV[S, _]],
                                   evaluation: Vector[SoV[S, _]] => Vector[SoV[S, _]],
                                   dynamically_defined_functions: AvailOps)


  def ops(availOps: AvailOps): AvailOps = availOps


  /**
    * Using the Wildcards to encode for generic types - for sure this can be done more elegant - but good enough for now
    */
  class Wildcard1

  //using this for generic Types e.g. foo[T](x : T)
  class Wildcard2

  //using this for generic Types e.g. foo[T](x : T)
  class Wildcard3

  //using this for generic Types e.g. foo[T](x : T)
  class Wildcard4

  //using this for generic Types e.g. foo[T](x : T)
  class Wildcard5

  //using this for generic Types e.g. foo[T](x : T)
  class Wildcard6

  //using this for generic Types e.g. foo[T](x : T)
  class Wildcard7

  //using this for generic Types e.g. foo[T](x : T)

  /**
    * Hard coding the cases for Numeric and Ordering - so far those are the only typeclasses used in core LMS
    * If more cases appear a more generic elegant solution would be preferable
    */
  class WildcardNumeric

  class WildcardOrdering


  /**
    * Takes the list of all available ops and filters out those that can be called with the current set of graph nodes
    * @param availTypes the types of the currently existing variables
    * @param seval current iteration step of the random symbol creation
    * @return returns a Map of all Ops that could be called
    */
  def filterOps(availTypes: AvailUniqueTypes, seval: EvalGenIterStep[Rep]): AvailOps = {
    //all ops that are statically known
    val fixedops = allops.filter(x => x._1.subsetOf(availTypes))

    //all ops that come into existence due to dynamic definitions of functions
    val dynops = seval.dynamically_defined_functions.filter(x => x._1.subsetOf(availTypes))

    //this is just a naive way of merging the two hashmaps without loosing entries -> replace me with something more elegant!
    val dynops2 = dynops.map { case (k, v) => {
      val other: Set[Op] = fixedops.getOrElse(k, Set.empty[Op])
      val value: Set[Op] = (v ++ other)
      k -> value
    }
    }
    fixedops ++ dynops2
  }

  /**
    * checks if the op we randomly selected the creation of a function literal and then replaces the placeholder with
    * an actual symbol we do this cause we want the function to only take parameters that are also currently available
    * to increase the liklyhood of it being used  the actual implemention is within the GenRandomFunctions trait
    * @param desc Limits on the random process (e.g. nesting deep and # of functions in this case)
    * @param op the op selected (is checked if its a function placeholder)
    * @param seval
    * @return
    */
  def createFunction(desc: CodeDescriptor, op: Op, seval: EvalGenIterStep[Rep])
  : Gen[(Op, Option[(Vector[GenTypes[_]], Vector[GenTypes[_]])])] = {
    (op, None)
  }


  private def isWildCard(targetType: GenTypes[_]): Boolean = {
    targetType.mf == manifest[Wildcard1] ||
      targetType.mf == manifest[Wildcard2] ||
      targetType.mf == manifest[Wildcard3] ||
      targetType.mf == manifest[Wildcard4] ||
      targetType.mf == manifest[Wildcard5] ||
      targetType.mf == manifest[Wildcard6] ||
      targetType.mf == manifest[Wildcard7]
  }

  private def WildCardNr(targetType: GenTypes[_]): Int = {
    if (targetType.mf == manifest[Wildcard1]) 1
    else if (targetType.mf == manifest[Wildcard2]) 2
    else if (targetType.mf == manifest[Wildcard3]) 3
    else if (targetType.mf == manifest[Wildcard4]) 4
    else if (targetType.mf == manifest[Wildcard5]) 5
    else if (targetType.mf == manifest[Wildcard6]) 6
    else if (targetType.mf == manifest[Wildcard7]) 7
    else {
      assert(false, "invalid wildcard")
      0
    }
  }

  def genAssignment(seval: EvalGenIterStep[Rep], targetType: GenTypes[_]): Gen[Int] = {
    val possible_targets = seval.syms.map(e => e.tag).zipWithIndex.filter(e => checkTypeCompatibility(e._1, targetType))
    for {choice <- Gen.oneOf(possible_targets)} yield choice._2
  }

  private def checkTypeCompatibility(ctype: GenTypes[_], targetType: GenTypes[_]): Boolean = ctype == targetType || isWildCard(targetType)

  def removeWildCards(op: Op, seval: EvalGenIterStep[Rep]): Op = {
    val (wassign, newargs) = op.args.foldLeft((Map.empty[GenTypes[_], GenTypes[_]], Vector.empty[GenTypes[_]]))((acc, ele) => {
      val (wcards, assigns) = acc
      if (isWildCard(ele)) {
        //to we deal with a wild car
        val wassin = wcards.get(ele)
        if (wassin.isDefined) {
          //if so - did we assign a type to it already
          val assignedwildcard: GenTypes[_] = wcards(ele)
          (wcards, assigns :+ assignedwildcard) //use the assigned type
        }
        else {
          //not so nice to sample here....
          val assignedwildcard: GenTypes[_] = genAssignment(seval, ele).map(symnr => seval.syms(symnr).tag).sample.get
          (wcards + (ele -> assignedwildcard), assigns :+ assignedwildcard) //use the assigned type
        }
      }
      else (wcards, assigns :+ ele)
    })
    //at this point we removed all Wildcards in the Args - now we also need to assign them in the returns
    val rassigns = op.returns.foldLeft(Vector.empty[GenTypes[_]])((acc, ele) => {
      if (isWildCard(ele)) {
        //to we deal with a wild car
        val wassin = wassign.get(ele)
        if (wassin.isDefined) {
          val assignedwildcard: GenTypes[_] = wassign(ele)
          acc :+ assignedwildcard //use the assigned type
        }
        else {
          assert(false, "Wildcard in Return Type that was not assigned in Args!")
          ???
        }
      }
      else {
        acc :+ ele
      }
    })
    op.copy(args = newargs, returns = rassigns)
  }

  /**
    * Will create a random generator that produces an Op under the constraints of the types existing so far and the code describtor
    * @param desc
    * @param seval
    * @return
    */
  def genOp(desc: CodeDescriptor, seval: EvalGenIterStep[Rep]): Gen[Op] = {
    val availTypes: AvailUniqueTypes = seval.syms.map(e => e.tag).toSet
    val availOps = filterOps(availTypes, seval)
    val flattened = availOps.flatMap(opset => opset._2)
    val nestcheck = flattened.filter(p => {
      filterNestDepth(desc, seval, p) &&
        (dynOpNrTimesUsed.get(p).isEmpty || dynOpNrTimesUsed.get(p).get < desc.max_calls) //checking that a dynamic function is only called max_calls times
    })
    for {
      randomop <- Gen.oneOf(nestcheck.toSeq)
    } yield {
      val t = randomop
      if (t.desc.localfidx.isDefined) //if we selected a dynamic function - increase the call counter
        dynOpNrTimesUsed = {
          if (dynOpNrTimesUsed.get(t).isDefined)
            dynOpNrTimesUsed + (t -> (dynOpNrTimesUsed(t) + 1))
          else
            dynOpNrTimesUsed + (t -> 1)
        }
      t
    }
  }

  //override in traits that would nest to check if its a valid op in the context
  def filterNestDepth(desc: CodeDescriptor, seval: EvalGenIterStep[Rep], op: Op): Boolean = {
    true
  }


  /** *
    * Produces one iteration step of a function which will generate a random piece of code / symbolic execution
    * @param desc codestyle we want to achive (limits on the random code generation)
    * @param veval the value based execution of the code so far
    * @param seval the symbolic execution of the code (staged) so far
    * @return a random Generator that increases veval/seval by one iteration step (could be multiple ops in e.g. the case of functions)
    */
  def genEvalGenIterStep(desc: CodeDescriptor, veval: EvalGenIterStep[NoRep], seval: EvalGenIterStep[Rep])
  : Gen[(EvalGenIterStep[NoRep], EvalGenIterStep[Rep])] = {
    for {
      wop <- genOp(desc, seval)
      (fop, nf) <- createFunction(desc, wop, seval)
      op <- removeWildCards(fop, seval)
      assign <- Gen.sequence[Vector[Int], Int](op.args.map(arg => genAssignment(seval, arg)))
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
      if (nf.isDefined) {
        val localf = op.desc.returns.map(e => cTP(null, e)).head
        //we know here that the returned symbol is a local function
        val (args, returns) = nf.get


        val functionvaridx = fnest.syms.size //== index of the function

        //val functiontp = x(functionvaridx)
        //val functionuntyped = fun2tp.find(p => p._2 == functiontp).get._1


        val applyop: Op = {
          val f: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => {
            val functionsym = x.head
            val inasctp: Vector[cTP] = x.tail.zipWithIndex.map(ele => cTP(ele._1, args(ele._2).tag))
            val lamops = functionsym.asInstanceOf[Vector[cTP] => Vector[cTP]]
            val res = lamops(inasctp)
            val withouttag: Vector[Any] = res.map(ele => ele.sym)
            withouttag
          }
          val sf: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => {
            val functionexp = x.head //we construct function applys such that the function is always the first arg
            val functiontp: TP[_] = if (!functionexp.isInstanceOf[Exp[_]]) {
                println("TIETLJKAJEDGKJ")
                ???
              }
              else
                exp2tp.get(functionexp.asInstanceOf[Exp[_]]).get
            val stagedFunction = funexp2StagedFunction(functiontp.sym)
            val inasctp: Vector[cTP] = x.tail.zipWithIndex.map(ele => cTP(ele._1, args(ele._2).tag))
            val lamops = toLambdaOps(stagedFunction).asInstanceOf[LambdaOps[Vector[cTP], Vector[cTP]]]
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
          val op = OpDescription(args.map(e => e.tag), returntypes, f, sf, Some(functionvaridx))
          Op("apply" + functionvaridx, op)
        }
        FNest(fnest.syms ++ op.desc.returns.map(e => cTP(null, e)), f, stagedf, registerOp(applyop, fnest.localfs))
      }
      else {
        /*println("------------------")
        println(op)
        println(op.desc.returns)
        println("------------------")*/
        FNest(fnest.syms ++ op.desc.returns.map(e => cTP(null, e)), f, stagedf, fnest.localfs)
      }

    }
  }





}
