

package RandomTesting


import org.scalacheck._
import scala.lms.BaseExp
import scala.lms.ops._


trait GenRandomFunctions extends GenRandomOps {
  this: BaseExp =>

  var cur_nr_functions = 0

  //this is just a placeholder which will always be a fitting option during Op selection
  val createinternalfunction_placeholder: Op = {
    val f: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => ???
    val sf: Function1[Vector[Rep[_]], Vector[Rep[_]]] = (x: Vector[Rep[_]]) => ???
    Op("createinternalfunction_placeholder", Vector(Tag(manifest[Wildcard1])), Vector(Tag(manifest[Wildcard2])), f, sf, None)
  }

  override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
    super.supported_types(availTypes)
  }

  override def ops(map: AvailOps) = {
    super.ops(registerOp(createinternalfunction_placeholder, map))
  }

  override def createFunction(desc: CodeDescriptor, op: Op, dag: Dag, cCStatus: CCStatus)
  : Gen[(Op, Option[(Vector[GenTypes[_]], Vector[GenTypes[_]])], CCStatus)] =
  {
    if (op == createinternalfunction_placeholder) {
      val ncCStatus = cCStatus.copy(
        curr_nr_functions = cCStatus.curr_nr_functions + 1,
        curr_nodes_in_block = 0,
        curr_nest_depth = cCStatus.curr_nest_depth + 1)
      cur_nr_functions = cur_nr_functions + 1

      for {
        ini <- genExistingArgs(desc, dag, ncCStatus)
        (uStatus,ndag) <- genNodes(desc, ncCStatus, ini) //does not take any outside symbol for now (capture)
      } yield {
        val inisyms = ndag.dag(0).toVector.sortWith((a, b) => a.id < b.id).map(e => e.typ)
        val resultsyms = ndag.dag.flatMap(l => l.toVector).sortWith((a, b) => a.id < b.id).map(e => e.typ)
        val (callstack, callstack_staged) = chainDag(ndag)
        val exposeargs = genExposeRep(inisyms)
        val exposeres = genExposeRep(resultsyms)



        val createinternalfunction: Op = {
          val f: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => {
            val lambda = callstack
            Vector(lambda)
          }
          val sf: Function1[Vector[Rep[_]], Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
            val lambda = fun(callstack_staged, false)(exposeargs, exposeres)
            val lambdatp: TP[_] = exp2tp(lambda.exp)
            funexp2StagedFunction = funexp2StagedFunction + (lambda.exp -> lambda)
            Vector(lambdatp.sym)
          }

          val dyntypes: Unit => (Vector[TypeRep[_]], Vector[TypeRep[_]]) = (u: Unit) => {
            val args = exposeargs.freshExps()
            val rets = exposeres.freshExps()
            val a = args.map(_.tp)
            val r = rets.map(_.tp)
            (a, r)
          }
          val rettype = Tag(manifest[Function[_, _]], Some(dyntypes))
          val declaration = Op("createinternalfunction" + dag.highestid, Vector.empty, Vector(rettype), f, sf, None)
          //giving the op a number depending on the fnest size (not unique in nested case?)
          declaration
        }

        val afterCStatus = cCStatus.copy(curr_nr_functions = cCStatus.curr_nr_functions + 1)
        //uStatus.copy(curr_nr_functions = uStatus.curr_nr_functions + 1)
        (createinternalfunction, Some((inisyms, resultsyms )), afterCStatus)
      }
    }
    else super.createFunction(desc, op, dag, cCStatus)
  }

  //this is a version of GenRandomOps GenArg - the difference is that this one will only consider Symbols that already
  //exist (to increase the liklyhood that the function is actually called)
  def genExistingArg(dag: Dag): Gen[GenTypes[_]] = {
    val alltypes = dag.dag.flatMap(p => p.map(x => x.typ))
    for {
      typechoice <- Gen.oneOf(alltypes.filter(p => !p.mf.toString().contains("Function"))) //don't allow passing functions for now
    } yield {
      typechoice
    }
  }

  //as previous function.....
  def genExistingArgs(desc: CodeDescriptor, dag: Dag, cCStatus: CCStatus): Gen[Dag] =
    for {
      nrargs <- Gen.chooseNum(0, desc.max_args)
      args <- Gen.containerOfN[Vector, GenTypes[_]](nrargs, genExistingArg(dag))
    } yield {
      Dag(args)
    }

   //override in traits that would nest to check if its a valid op in the context
   override def filterNestDepth(desc: CodeDescriptor, dag: Dag, op: Op, cCStatus: CCStatus): Boolean = {
     if(op == createinternalfunction_placeholder) {
       val b = cCStatus.curr_nest_depth < desc.max_nest_depth && cCStatus.curr_nr_functions < desc.max_functions
       b
     }
     else super.filterNestDepth(desc, dag, op, cCStatus)
   }
}


