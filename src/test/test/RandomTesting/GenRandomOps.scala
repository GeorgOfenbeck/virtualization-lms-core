package RandomTesting

import org.scalatest.fixture

import scala.annotation.tailrec
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.lms.internal._


trait GenRandomOps extends ExposeRepBase with FunctionsExp {

  var funexp2StagedFunction: Map[Exp[_], StagedFunction[_,_]] = Map.empty

  case class CodeDescriptor(
                             max_nodes_per_block: Int,
                             max_toplevel_args: Int,
                             max_args: Int,
                             max_nest_depth: Int,
                             max_functions: Int,
                             max_returns: Int,
                             max_calls: Int,
                             //cur_nodes_per_block: Int = 0,
                             //cur_nest_depth: Int = 0
                             max_dynamic_calls: Int,
                             max_opset_calls: Map[Set[Op], Int]
                           ) {

    val op_sets: Map[Op, Set[Set[Op]]] = max_opset_calls.foldLeft(Map.empty[Op, Set[Set[Op]]])((acc, ele) => {
      val currset = ele._1
      currset.foldLeft(acc)((acc2, op) => {
        if (acc2.contains(op)) acc2 + (op -> (acc2(op) + currset))
        else acc2 + (op -> Set(currset))
      })
    })
  }


  case class CCStatus(
                       curr_nodes_in_block: Int,
                       curr_nest_depth: Int,
                       curr_nr_functions: Int,
                       avail_ops: Map[Set[GenTypes[_]], Set[Op]],
                       dynOpNrTimesUsed: Map[Op, Int]
                     )

  /** *
    *
    * @param mf      used all over the place to check if a symbol/value is a suitable input to an op
    * @param dynTags this is used in the case of a dynamic function where the mf would only yield Function[Any,Any] and we would
    *                like to get more concrete type info (note - this most likley could be solved more elegantly via reflection)
    * @tparam T The type we try to describe
    */
  case class Tag[T](mf: Manifest[T],
                    dynTags: Option[Unit => (Vector[TypeRep[_]], Vector[TypeRep[_]])] = None)

  /** *
    * alias used to make swapping out of the type representation easier
    */
  type GenTypes[T] = Tag[T]

  /**
    * shortcut type since the unique set of types is used a lot in the code
    */
  type AvailUniqueTypes = Set[GenTypes[_]]


  type AvailTypeTuples = Set[AvailUniqueTypes]

  /**
    * a map collecting all operations possible given a set of input types
    */
  type AvailOps = Map[Set[GenTypes[_]], Set[Op]]

  /**
    * Idendity type used to be able to switch between evaluation and symbolic evaluation (staging) easily
    *
    * @tparam T
    */
  type NoRep[T] = T


  /**
    * SoV - Symbol or Value is used to abstract over either a value (e.g. Int value) or a symbolic value such as Rep[Int]
    * it additionally holds the type of the Symbol explicitly in the field 'tag'
    *
    * @param sym symbol or value
    * @param tag type representation
    * @tparam R higher order type that is either Rep or NoRep
    * @tparam T type of the symbol/value
    */
  case class SoV[R[_], T](val sym: R[T], val tag: GenTypes[T])


  /** *
    *
    * @param name                Name used for e.g. debugging
    * @param args                the types taken as arguments by the op
    * @param returns             the types returned by the op
    * @param evaluation          a function evaluating the op
    * @param symbolic_evaluation a function symbolically (staging) the op
    * @param localfidx           in case that op is a dynamic created op this stores which symbol nr it is
    */
  case class Op(name: String,
                args: Vector[GenTypes[_]],
                returns: Vector[GenTypes[_]],
                evaluation: Vector[NoRep[_]] => Vector[NoRep[_]], //Vector[SoV[NoRep, _]] => Vector[SoV[NoRep, _]] ,
                symbolic_evaluation: Vector[Rep[_]] => Vector[Rep[_]], //Vector[SoV[Rep, _]] => Vector[SoV[Rep, _]] ,
                localfidx: Option[Int]
               )


  def registerOp(newop: Op, sofar: AvailOps): AvailOps = {
    val uniqueArgs = newop.args.toSet
    val withoutWildcards = uniqueArgs.filter(t => !isWildCard(t))
    val entrysofar = sofar.get(withoutWildcards)
    val newentry = if (entrysofar.isDefined) entrysofar.get + newop else Set(newop)
    sofar + (withoutWildcards -> newentry)
  }


  lazy val allops = ops(Map.empty)


  def ops(availOps: AvailOps): AvailOps = availOps

  def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = availTypes

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


  case class DagNode(val typ: GenTypes[_], val id: Int)

  case class OpID(val op: Op, args: Vector[Int], returns: Vector[Int])

  case class OpLookUp(
                       val opargsrets: Set[OpID],
                       val args2ops: Map[Vector[Int], Set[OpID]],
                       val rets2op: Map[Vector[Int], OpID],
                       val ret2rets: Map[Int, Vector[Int]],
                       val arg2ops: Map[Int, Set[OpID]],
                       val OpswithoutDep: Set[OpID]
                     ) {
    /*def apply(): OpLookUp = OpLookUp(Map.empty,Map.empty,Map.empty,Map.empty, Map.empty, Map.empty,Set.empty)*/


    def insertOp(op: Op, argids: Vector[Int], returnids: Vector[Int]) = {
      val opid = OpID(op, argids, returnids)
      val nopargsrets = opargsrets + opid
      //val nop2args = op2args + (op -> argids)
      //val nop2rets = op2rets + (op -> returnids)
      val nargs2op = if (args2ops.contains(argids))
        args2ops + (argids -> (args2ops(argids) + opid))
      else args2ops + (argids -> Set(opid))
      val nrets2op = rets2op + (returnids -> opid)
      val nret2rets = returnids.foldLeft(ret2rets) { (acc, ele) => acc + (ele -> returnids) }

      //all ops that so far had their returns not being used (relies on building it incrementaly)
      val opsaffected = argids.flatMap(a => ret2rets.get(a)).flatMap(s => rets2op.get(s)).filter(o => OpswithoutDep.contains(o))
      val nopswithoutDep2 = (OpswithoutDep -- opsaffected) + opid



      val allargids = nopargsrets.flatMap(p => p.args.toSet) //all parameters
      val nopswithoutDep = nopargsrets.filter(p => {
          val rets = p.returns
          val check = rets.filter(x => allargids.contains(x))
          //returns an empty set if non of our returns is used as an arg

          //given the current op - do we have a return that is a parameter for another op
          check.isEmpty
        })
      //val nopswithoutDep =  nopswithoutDep3 + opid
      //val diff = nopswithoutDep2 -- nopswithoutDep


      val narg2ops = argids.foldLeft(arg2ops) { (acc, ele) => {
        if (acc.contains(ele))
          acc + (ele -> (acc(ele) + opid))
        else
          acc + (ele -> Set(opid))
      }
      }

      //OpLookUp(nop2args,nop2rets,nargs2op,nrets2op, nret2rets, narg2ops, nopswithoutDep)
      OpLookUp(nopargsrets, nargs2op, nrets2op, nret2rets, narg2ops, nopswithoutDep)
    }

    def delete(opid: OpID) = {
      /*val rets = op2rets(op)
      val args = op2args(op)*/
      val rets = opid.returns
      val args = opid.args
      val op = opid.op
      val nargs2op = if (args2ops.contains(args) && args2ops(args).size > 1)
        args2ops + (args -> (args2ops(args) - opid))
      else
        args2ops - args
      val nrets2op = rets2op - rets
      /*val nop2args = op2args - op
      val nop2rets = op2rets - op*/
      val nopid = opargsrets - opid
      val nret2rets = rets.foldLeft(ret2rets) { (acc, ele) => acc - ele }

      val narg2ops = args.foldLeft(arg2ops) { (acc, ele) => {
        if (acc.contains(ele)) {
          val old = acc(ele)
          if (acc(ele).size > 1) {
            val mold = old - opid
            acc + (ele -> mold)
          }
          else {
            if (old.contains(opid))
              acc - ele
            else
              acc //this can happen if we have e.g. twice the same arg and removed ourself already
          }
        }
        else
          acc - ele
      }
      }


      val retsets = args.flatMap(r => ret2rets.get(r))
      val opsets = retsets.flatMap(s => rets2op.get(s))

      val opswithoutRetUsed = opsets.filter(o => {
        val rset = o.returns
        val using = rets.flatMap(r => narg2ops.get(r))
        using.isEmpty
      })

      val nopswithoutDep2 = OpswithoutDep ++ opswithoutRetUsed - opid

      //sanity checks

      val allargids = nopid.flatMap(p => p.args.toSet)
      val nopswithoutDep = nopid.filter(p => {
        val rets = p.returns
        val check = rets.filter(x => allargids.contains(x))
        check.isEmpty
      })

      val diff = nopswithoutDep2 -- nopswithoutDep

      if (!diff.isEmpty) {
        //assert(false)
      }



      OpLookUp(nopid, nargs2op, nrets2op, nret2rets, narg2ops, nopswithoutDep)
    }

    def deletewRets(rets: Vector[Int]) = delete(rets2op(rets))
  }

  object Dag {
    def apply(ininodes: Vector[GenTypes[_]]): Dag = {
      val z = ininodes.zipWithIndex.map(e => DagNode(e._1, e._2)).toSet
      val m = z.map(e => e.id).foldLeft(Map.empty[Int, Int]) { (acc, ele) => acc + (ele -> 0) }
      //Dag(Vector(z),m, z.size,Map.empty, Map.empty, Map.empty)
      Dag(Vector(z), m, z.size, OpLookUp(Set.empty, Map.empty, Map.empty, Map.empty, Map.empty, Set.empty), Map.empty)
    }
  }

  case class Dag(val dag: Vector[Set[DagNode]], index: Map[Int, Int], highestid: Int,
                 //id2ids: Map[Int,Set[Int]],
                 opLookUp: OpLookUp,
                 //ret2op: Map[Set[Int], Op],
                 //arg2res: Map[Int, Set[Int]]
                 dynamically_defined_functions: AvailOps
                ) {

    def OpswithoutDep(): Set[OpID] = opLookUp.OpswithoutDep


    def removetillOp(opid: OpID): Dag = {
      val possibilites = OpswithoutDep()
      val withoutop = possibilites.filterNot(p => p == opid)
      if (withoutop.isEmpty)
        this
      else {
        val ndag = removeOp(withoutop.head)
        ndag.removetillOp(opid)
      }
    }

    def removeOp(opid: OpID): Dag = {
      val rets = opid.returns
      val ndag_mayempty = rets.foldLeft(dag) {
        (acc, ele) => {
          val level = index(ele)
          val oset = dag(level);
          val rm = oset.filter(p => p.id == ele)
          val nset = oset -- rm
          acc.updated(level, nset)
        }
      }
      val ndag = ndag_mayempty.takeWhile(p => !p.isEmpty)
      val nindex = rets.foldLeft(index) { (acc, ele) => acc - ele }
      val nopLookUp = opLookUp.delete(opid)

      //sanity checks

      val allnodes = ndag.flatten
      val allnodeids = allnodes.map(t => t.id).toSet
      val check = nopLookUp.opargsrets.filter(p => {
        val op = p.op
        val args = p.args
        val missing = args.filter(p => !allnodeids.contains(p))
        !missing.isEmpty
      }
      )
      if (!check.isEmpty) {
        assert(false)
      }

      Dag(ndag, nindex, highestid, nopLookUp, dynamically_defined_functions) //FIX - make it nindex again!
    }

    def availTypes(): AvailUniqueTypes = dag.flatMap(x => x.map(n => n.typ)).toSet

    def addOp(op: Op, assignedids: Vector[Int]): Dag = {
      val depth = (assignedids.map(e => getDeepth(e)) :+ 0 ).max //this puts ops without args at level 1 (just below f args)
      val t = op.returns.zipWithIndex.map(e => DagNode(e._1, e._2 + highestid + 1))
      val returnNodes = t.toSet
      val ids = t.map(e => e.id)
      val (ndag, pos) = if (depth == dag.size - 1) {
        //its the last level of the dag
        val ndag: Vector[Set[DagNode]] = dag :+ returnNodes
        (ndag, dag.size)
      } else {
        val oSet: Set[DagNode] = dag(depth + 1)
        val nSet = oSet ++ returnNodes
        val ndag: Vector[Set[DagNode]] = dag.updated(depth + 1, nSet)
        (ndag, depth + 1)
      }
      val nmap = ids.foldLeft(index) { (acc, ele) => acc + (ele -> pos) }
      val retidset = ids
      val nopLookUp = opLookUp.insertOp(op, assignedids, retidset)
      Dag(ndag, nmap, ids.max, nopLookUp, dynamically_defined_functions)
    }

    //returns all possible ID's of assignments
    def possibleAssigns(targetType: GenTypes[_]): Vector[Int] = {
      dag.flatMap(s => s.filter(e => checkTypeCompatibility(e.typ, targetType))).map(e => e.id)
    }

    def getDeepth(id: Int) = index(id)
  }

  /**
    * This is used to store a current set of input symbol/values and its extending function
    *
    * @param types                         all currently available symbol types
    * @param evaluation                    given the current symbols executes the function and returns an extended set of symbols
    * @param dynamically_defined_functions during the evaluation steps of previous iterations functions might have been created
    *                                      which should also be exposed as available ops - those functions are store in this field
    * @tparam S this decideds if the iterations describe a straightforward evaluation or its symbolic counterpart
    */
  case class EvalGenIterStep[S[_]](types: Vector[GenTypes[_]],
                                   evaluation: Vector[SoV[S, _]] => Vector[SoV[S, _]],
                                   dynamically_defined_functions: AvailOps)


  /**
    * Takes the list of all available ops and filters out those that can be called with the current set of graph nodes
    *
    * @param cCStatus   status variables for the code construction - used here are the remaining available ops
    * @param availTypes the types of the currently existing variables
    * @param seval      current iteration step of the random symbol creation
    * @return returns a Map of all Ops that could be called
    */
  def filterOps(cCStatus: CCStatus, availTypes: AvailUniqueTypes, dag: Dag): AvailOps = {
    // seval: EvalGenIterStep[Rep]): AvailOps = {
    //all ops that are statically known
    val fixedops = allops.filter(x => x._1.subsetOf(availTypes))

    //all ops that come into existence due to dynamic definitions of functions
    val dynops = dag.dynamically_defined_functions.filter(x => x._1.subsetOf(availTypes))

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
    *
    * @param desc Limits on the random process (e.g. nesting deep and # of functions in this case)
    * @param op   the op selected (is checked if its a function placeholder)
    * @param seval
    * @return
    */
  def createFunction(desc: CodeDescriptor, op: Op, dag: Dag, cCStatus: CCStatus)
  : Gen[(Op, Option[(Vector[GenTypes[_]], Vector[GenTypes[_]])], CCStatus)] = {
    (op, None, cCStatus)
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
    val possible_targets = seval.types.zipWithIndex.filter(e => checkTypeCompatibility(e._1, targetType))
    for {choice <- Gen.oneOf(possible_targets)} yield choice._2
  }

  def genAssignment(dag: Dag, targetType: GenTypes[_]): Gen[Int] =
    for {choice <- Gen.oneOf(dag.possibleAssigns(targetType))} yield choice

  private def checkTypeCompatibility(ctype: GenTypes[_], targetType: GenTypes[_]): Boolean = ctype == targetType || isWildCard(targetType)

  def removeWildCards(op: Op, dag: Dag): Op = {
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
          val assignedwildcard: GenTypes[_] = ??? //genAssignment(seval, ele).map(symnr => seval.types(symnr)).sample.get
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


  //override in traits that would nest to check if its a valid op in the context
  def filterNestDepth(desc: CodeDescriptor, dag: Dag, op: Op, cCStatus: CCStatus): Boolean = {
    true
  }


  /**
    * Will create a random generator that produces an Op under the constraints of the types existing so far and the code describtor
    *
    * @param desc
    * @param seval
    * @return
    */

  def genOp(desc: CodeDescriptor, cCStatus: CCStatus, dag: Dag): Gen[(Op, CCStatus)] = {
    //val availTypes: AvailUniqueTypes = seval.types.toSet //all types we currently see in the code
    val availTypes: AvailUniqueTypes = dag.availTypes() //all types we currently see in the code
    val availOps = filterOps(cCStatus, availTypes, dag) //filter which ops can operate on those types
    val flattened = availOps.flatMap(opset => opset._2)
    val nestcheck = flattened.filter(p => {
      filterNestDepth(desc, dag, p, cCStatus) &&
        (cCStatus.dynOpNrTimesUsed.get(p).isEmpty ||
        cCStatus.dynOpNrTimesUsed.get(p).get < desc.max_calls)
    })

    for {randomop <- Gen.oneOf(nestcheck.toSeq)} yield {
      val t = randomop
      val nStatus: CCStatus = if (randomop.localfidx.isDefined) {
        if (cCStatus.dynOpNrTimesUsed.get(t).isDefined)
          cCStatus.copy(dynOpNrTimesUsed = (cCStatus.dynOpNrTimesUsed + (t -> (cCStatus.dynOpNrTimesUsed(t) + 1))))
        else
          cCStatus.copy(dynOpNrTimesUsed = (cCStatus.dynOpNrTimesUsed + (t -> 1)))
      } else cCStatus
      (t,nStatus)
    }
  }

/*
  def createf[S[_]](assign: Vector[Int], op: Op, eval: Vector[S[_]] => Vector[S[_]])
  : (Vector[SoV[S, _]] => Vector[SoV[S, _]]) = {
    val f: (Vector[SoV[S, _]] => Vector[SoV[S, _]]) = (in: Vector[SoV[S, _]]) => {
      //take the assigned symbols / values from the vector of existing values / symbols and put them in a dedicated vector
      val argsyms: Vector[SoV[S, _]] = assign.foldLeft(Vector.empty[SoV[S, _]]) {
        (acc, ele) => acc :+ in(ele)
      }
      // get rid of the tags
      val symsonly = argsyms.map(e => e.sym)

      //then use this vector with the execution (symbolic or actual) get the result
      val ret: Vector[S[_]] =
        if (op.localfidx.isDefined) {
          ???
          /*val localf: SoV[S, _] = in(op.localfidx.get)
          val fplusargsyms = localf +: argsyms
          eval(fplusargsyms)*/
        }
        else eval(symsonly)

      assert(op.returns.size == ret.size) //make sure we got as many symbols back as expected
      val retwithtags: Vector[SoV[S, _]] = op.returns.zip(ret).map(e => {
          val (tag, sym) = e //TODO - can typecheck that the expected type and returned type are the same
          val sym2: S[Any] = sym.asInstanceOf[S[Any]]
          val tag2: Tag[Any] = tag.asInstanceOf[Tag[Any]]
          SoV(sym2, tag2)
        }).toVector
      in ++ retwithtags //concatenate symbol/values so far with the new results
    }
    f
  }*/


  /** *
    * Produces one iteration step of a function which will generate a random piece of code / symbolic execution
    *
    * @param desc     codestyle we want to achive (limits on the random code generation)
    * @param cCStatus status variables for the code construction (all state ideally is carried here)
    * @param veval    the value based execution of the code so far
    * @param seval    the symbolic execution of the code (staged) so far
    * @return a random Generator that increases veval/seval by one iteration step (could be multiple ops in e.g. the case of functions)
    */

  //def genEvalGenIterStep(desc: CodeDescriptor, cCStatus: CCStatus, veval: EvalGenIterStep[NoRep], seval: EvalGenIterStep[Rep])
  def genDagStep(desc: CodeDescriptor, cCStatus: CCStatus, indag: Dag): Gen[(CCStatus,Dag)] = {
    for {
      (wop, oCCStatus) <- genOp(desc, cCStatus, indag)
      (fop, nf, uCStatus) <- createFunction(desc, wop, indag, oCCStatus)
      op <- removeWildCards(fop, indag)
      assign <- Gen.sequence[Vector[Int], Int](op.args.map(arg => genAssignment(indag, arg)))
    } yield {
      if (nf.isDefined) {
        //the op is defining a new local function
        val dagwithfcreate = indag.addOp(op, assign) //we add the function
        val t = (dagwithfcreate.opLookUp.opargsrets -- indag.opLookUp.opargsrets)
        val opid = t.head
        val localf = op.returns.head
        val (args, returns) = nf.get

        val functionvaridx = opid.returns.head //here we put the function or the tuple

        val applyop: Op = {
          val f: Function1[Vector[_],Vector[_]] = (x: Vector[_]) => {
            val functionsym = x.head
            //val inasctp: Vector[cTP] = x.tail.zipWithIndex.map(ele => cTP(ele._1,args(ele._2).tag))
            val inargs = x.tail
            val lamops = functionsym.asInstanceOf[Vector[_] => Vector[_]]
            val res = lamops(inargs)
            //val withouttag: Vector[Any] = res.map(ele => ele.sym)
            val withouttag: Vector[Any] = res
            withouttag
          }
          val sf: Function1[Vector[Rep[_]],Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
            val functionexp = x.head //.asInstanceOf[(Exp[_],StagedFunction[_,_])] //we construct function applys such that the function is always the first arg
            val stagedFunction: StagedFunction[_,_] = funexp2StagedFunction(functionexp)
            //val  = x.tail.head.asInstanceOf[]
            val functiontp: TP[_] = if (!functionexp.isInstanceOf[Exp[_]])
              {
                println("how did we end up here?")
                ???
              }
              else
                exp2tp.get(functionexp.asInstanceOf[Exp[_]]).get
            val inargs = x.tail.tail //.asInstanceOf[Vector[Rep[_]]]
            //val inasctp: Vector[cTP] = x.tail.zipWithIndex.map(ele => cTP(ele._1,args(ele._2).tag))
            val lamops = toLambdaOps(stagedFunction).asInstanceOf[LambdaOps[Vector[Rep[_]],Vector[Rep[_]]]]
            //val functiontyped = functionuntyped.asInstanceOf[Vector[cTP] => Vector[cTP]]
            //val res: Vector[cTP] = ??? //functiontyped(inasctp)
            //val res: Vector[cTP] = lamops(inasctp) //functiontyped(inasctp)
            val res = lamops(inargs)
            //val withouttag: Vector[Any] = res.map(ele => ele.sym)
            //withouttag
            res
          }
          //val returntypes = returns.map(e => e.tag)
          /*println ("returns .... ->")
          println(returntypes)
          println("-----------------")*/
          Op("apply"+functionvaridx, args,returns,f,sf,Some(functionvaridx))
        }

        val dagwithfcreateandapply = dagwithfcreate
          /*dagwithfcreate.copy(
            dynamically_defined_functions =  registerOp(applyop,dagwithfcreate.dynamically_defined_functions)
          )*/

        (uCStatus,dagwithfcreateandapply)
      }
      else (uCStatus,indag.addOp(op, assign))
    }
  }

  //def genNodes(desc: CodeDescriptor, cCStatus: CCStatus, iniv: Vector[EvalGenIterStep[NoRep]], inis: Vector[EvalGenIterStep[Rep]]):
  def genNodes(desc: CodeDescriptor, cCStatus: CCStatus, indag: Dag): Gen[(CCStatus,Dag)] = Gen.lzy{
    if (cCStatus.curr_nodes_in_block < desc.max_nodes_per_block) {
      for {
        (nstatus,nextdag) <- genDagStep(desc, cCStatus, indag)
        (ustatus,tail) <- genNodes(desc, nstatus.copy(curr_nodes_in_block = nstatus.curr_nodes_in_block + 1), nextdag)
      } yield {
        (ustatus,tail)
      }
    } else (cCStatus,indag)
  }

  /**
    * create a Vector of random cTPs
    * its a vector cause we might require a certain type combination so that our ops work
    * e.g. if we only have the plus(l: T, r: Q) operation we require that we have a T and a Q in the args
    *
    * @return
    */
  def genArg(): Gen[Vector[GenTypes[_]]] = for {
    typechoice <- Gen.oneOf(supported_types(Set.empty).toSeq)
  } yield typechoice.toVector


  //creates n Args - since a single "arg" might be a tuple of args we simple sample n tuples and then only consider the first
  //m args such that the total size is smaller then n
  def genArgs(size: Int): Gen[Dag] =
    for {
      args <- Gen.containerOfN[Vector, Vector[GenTypes[_]]](size, genArg)
    } yield {
      val t = args.foldLeft(Vector.empty[GenTypes[_]]) {
        (acc, ele) => {
          val p1 = acc ++ ele
          if (p1.size > size)
            acc
          else
            p1
        }
      }
      Dag(t)
    }

  def getfinalIterStep[S[_]](indicies: Vector[Int], itersteps: Vector[EvalGenIterStep[S]]): Vector[EvalGenIterStep[S]] = {

    val f: (Vector[SoV[S, _]] => Vector[SoV[S, _]]) = (in: Vector[SoV[S, _]]) => {
      indicies.foldLeft(Vector.empty[SoV[S, _]]) { (acc, ele) => acc :+ in(ele) }
    }
    val types = indicies.foldLeft(Vector.empty[GenTypes[_]]) {
      (acc, ele) => acc :+ itersteps.last.types(ele)
    }
    itersteps :+ EvalGenIterStep(types, f, itersteps.last.dynamically_defined_functions)
  }

  //used to limit the number of returns
  def genReturns(desc: CodeDescriptor, vecv: Vector[EvalGenIterStep[NoRep]], vecs: Vector[EvalGenIterStep[Rep]])
  : Gen[(Vector[EvalGenIterStep[NoRep]], Vector[EvalGenIterStep[Rep]])] = {
    for {
      nrrets <- Gen.chooseNum(1, desc.max_returns)
      indicies <- Gen.pick(nrrets, vecs.last.types.zipWithIndex.map(e => e._2))
    } yield {
      val finalv = getfinalIterStep(indicies.toVector, vecv)
      val finals = getfinalIterStep(indicies.toVector, vecs)
      (finalv, finals)
    }
  }

  //def genCode(desc: CodeDescriptor, cCStatus: CCStatus): Gen[(Vector[EvalGenIterStep[NoRep]], Vector[EvalGenIterStep[Rep]])] = {
  def genCode(desc: CodeDescriptor, cCStatus: CCStatus): Gen[(CCStatus,Dag)] = {
    for {
      ini <- genArgs(desc.max_toplevel_args) //(vecv, vecs) <- genNodes(desc, cCStatus, Vector(EvalGenIterStep[NoRep](ini, null, Map.empty)), Vector(EvalGenIterStep[Rep](ini, null, Map.empty)))
      (uStatus,ndag) <- genNodes(desc, cCStatus, ini)
    } yield (uStatus,ndag)
  }

  @tailrec
  private def chainf[S[_]](v: Vector[EvalGenIterStep[S]], res: Vector[SoV[S, _]]): Vector[SoV[S, _]] =
    if (v.isEmpty) res else chainf(v.tail, v.head.evaluation(res))


  def chainHeadf[S[_]](v: Vector[EvalGenIterStep[S]]): (Vector[SoV[S, _]] => Vector[SoV[S, _]]) = {
    if (v.tail.isEmpty)
      assert(false, "seems there are no instructions")
    val f: (Vector[SoV[S, _]] => Vector[SoV[S, _]]) = (in: Vector[SoV[S, _]]) => {
      chainf(v.tail, in)
    }
    f
  }


  def chainDag(dag: Dag):
  (Vector[SoV[NoRep, _]] => Vector[SoV[NoRep, _]], Vector[SoV[Rep, _]] => Vector[SoV[Rep, _]]) = {
    val lookup = dag.opLookUp
    val retsets = lookup.rets2op.keySet
    val sorted = retsets.toVector.sortWith((a, b) => (a.max < b.max))


    val real = sorted.map(s => {
      val op = lookup.rets2op(s)
      createf2[NoRep](lookup, op, op.op.evaluation)
    })
    val symbolic = sorted.map(s => {
      val op = lookup.rets2op(s)
      createf2[Rep](lookup, op, op.op.symbolic_evaluation)
    })

    val freal: Vector[SoV[NoRep, _]] => Vector[SoV[NoRep, _]] = (in: Vector[SoV[NoRep, _]]) => {
      val m = in.zipWithIndex.foldLeft(Map.empty[Int, SoV[NoRep, _]]) { (acc, ele) => acc + (ele._2 -> ele._1) }
      val mr = real.foldLeft(m) { (acc, ele) => ele(acc) }
      val vr = mr.toVector.sortWith((a, b) => (a._1 < b._1)).map(e => e._2)
      vr
    }

    val fsym: Vector[SoV[Rep, _]] => Vector[SoV[Rep, _]] = (in: Vector[SoV[Rep, _]]) => {
      val m = in.zipWithIndex.foldLeft(Map.empty[Int, SoV[Rep, _]]) { (acc, ele) => acc + (ele._2 -> ele._1) }
      val mr = symbolic.foldLeft(m) { (acc, ele) => ele(acc) }
      val vr = mr.toVector.sortWith((a, b) => (a._1 < b._1)).map(e => e._2)
      vr
    }

    (freal, fsym)
    //(real,symbolic)

  }

  def createf2[S[_]](lookup: OpLookUp, opid: OpID, eval: Vector[S[_]] => Vector[S[_]])
  : (Map[Int, SoV[S, _]] => Map[Int, SoV[S, _]]) = {
    val f: (Map[Int, SoV[S, _]] => Map[Int, SoV[S, _]]) = (in: Map[Int, SoV[S, _]]) => {
      //take the assigned symbols / values from the vector of existing values / symbols and put them in a dedicated vector

      val assign = opid.args
      val argsyms: Vector[SoV[S, _]] = assign.foldLeft(Vector.empty[SoV[S, _]]) {
        (acc, ele) => acc :+ in(ele)
      }
      // get rid of the tags
      val symsonly = argsyms.map(e => e.sym)

      //then use this vector with the execution (symbolic or actual) get the result
      val ret: Vector[S[_]] =
        if (opid.op.localfidx.isDefined) {
          val fplusargsyms = in(opid.op.localfidx.get).sym +: symsonly
          eval(fplusargsyms)
        }
        else
          eval(symsonly)

      assert(opid.returns.size == ret.size) //make sure we got as many symbols back as expected
      val retwithtags: Vector[SoV[S, _]] = opid.op.returns.zip(ret).map(e => {
          val (tag, sym) = e //TODO - can typecheck that the expected type and returned type are the same
          val sym2: S[Any] = sym.asInstanceOf[S[Any]]
          val tag2: Tag[Any] = tag.asInstanceOf[Tag[Any]]
          SoV(sym2, tag2)
        }).toVector
      val retids = opid.returns
      assert(retids.size == retwithtags.size)

      retids.zipWithIndex.foldLeft(in) { (acc, ele) => {
        acc + (ele._1 -> retwithtags(ele._2))
      }
      }
      //in ++ retwithtags //concatenate symbol/values so far with the new results
    }
    f
  }


  def genExposeRep(v: Vector[GenTypes[_]]): ExposeRep[Vector[SoV[Rep, _]]] = {
    //val tags = v.map(e => e.tag)
    new ExposeRep[Vector[SoV[Rep, _]]] {
      val freshExps = (u: Unit) => {
        def helpert[T](args: Vector[TypeRep[_]], returns: Vector[TypeRep[_]])(implicit tag: TypeRep[T]): TypeRep[T] = {
          tag match {
            case x@TypeExp(mf, dynTags) => {
              val f: Unit => (Vector[TypeRep[_]], Vector[TypeRep[_]]) = (u: Unit) => {
                //(Vector.empty[TypeExp[_]],Vector.empty[TypeExp[_]])
                (args, returns)
              }
              x.copy(dynTags = Some(f))
            }
            case _ => {
              assert(false, "this should never match")
              tag
            }
          }
        }
        v.foldLeft(Vector.empty[Exp[_]])((acc, ele) => {
          ele.dynTags match {
            case Some(ftags) => {
              val (args, rets) = ftags()
              val tagnew: TypeRep[_ => _] = helpert(args, rets)
              val exp = Arg[_ => _](tagnew)
              acc :+ exp
            }
            case None => acc :+ Arg(ele.mf)
          }
        })
      }
      val vec2t: Vector[Exp[_]] => Vector[SoV[Rep, _]] =
      //(v: Vector[Exp[_]]) => Vector.empty
        (v: Vector[Exp[_]]) => v.foldLeft(Vector.empty[SoV[Rep, _]])((acc, ele) => {
          val mf: Manifest[Any] = exp2tp(ele).tag.mf.asInstanceOf[Manifest[Any]]
          val tag: Tag[Any] = Tag(mf) //the cast above is nasty - but since we don't use the type
          val sov: SoV[Rep, Any] = SoV(ele, tag) //and discard it it wont matter in the end
          acc :+ sov
        })
      val t2vec: Vector[SoV[Rep, _]] => Vector[Exp[_]] =
      //(x: Vector[cTP]) => Vector.empty
        (x: Vector[SoV[Rep, _]]) => x.foldLeft(Vector.empty[Exp[_]])((acc, ele) => {
          if (!ele.sym.isInstanceOf[Exp[_]])
            assert(false, "cast error")
          acc :+ ele.sym.asInstanceOf[Exp[_]]
        })
    }
  }

  //Introduced this just to avoid that scalacheck shrinks that vector (which doesnt make sense)
  case class StealthIt(x: Vector[SoV[NoRep, _]])

  def hideit(v: Vector[GenTypes[_]]): Gen[StealthIt] = for {
    x <- genArgInstances(v)
  } yield {
    val t = StealthIt(x)
    t
  }

  def genArgInstances(v: Vector[GenTypes[_]]): Gen[Vector[SoV[NoRep, _]]] = {
    val t = v.map(e => {
      for {
        instance <- genTypeInstance(e)
      } yield instance
    })
    val t1 = t
    val r = for {
      ge <- Gen.sequence[Vector[SoV[NoRep, _]], SoV[NoRep, _]](t1)
    } yield ge //.zipWithIndex.map(e => cTP(e._1,v(e._2).tag))
    r
  }

  def genTypeInstance(targetcTP: GenTypes[_]): Gen[SoV[NoRep, _]] = {
    val boolm: String = manifest[Boolean].toString()
    val intm: String = manifest[Int].toString()
    val longm: String = manifest[Long].toString()
    val floatm: String = manifest[Float].toString()
    val doublem: String = manifest[Double].toString()
    val target: String = targetcTP.mf.toString()
    target match {
      case `boolm` => {
        for {
          choice <- Gen.oneOf(true, false)
        } yield SoV[NoRep, Boolean](choice, Tag(manifest[Boolean]))
      }
      case `intm` => for {
        choice <- Arbitrary.arbitrary[Int]
      } yield SoV[NoRep, Int](choice, Tag(manifest[Int]))

      case `floatm` => for {
        choice <- Arbitrary.arbitrary[Float]
      } yield SoV[NoRep, Float](choice, Tag(manifest[Float]))

      case `longm` => for {
        choice <- Arbitrary.arbitrary[Long]
      } yield SoV[NoRep, Long](choice, Tag(manifest[Long]))

      case `doublem` => for {
        choice <- Arbitrary.arbitrary[Double]
      } yield SoV[NoRep, Double](choice, Tag(manifest[Double]))

      case _ => {
        assert(false, "seems we are missing a type instance generator for type " + targetcTP.mf)
        ???
      }
    }
  }


}
