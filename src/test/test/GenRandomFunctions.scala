package test

import org.scalacheck._


import scala.lms.BaseExp
import scala.lms.ops._


trait GenRandomFunctions extends GenRandomOps{
 this: BaseExp  =>

 var cur_nr_functions = 0

 //this is just a placeholder which will always be a fitting option during Op selection
  val createinternalfunction_placeholder: Op = {
   val f: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => ???
   val sf: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => ???
   val op = OpDescription(Vector(manifest[Wildcard1]), Vector(manifest[Wildcard2]), f, sf, None)
   Op("createinternalfunction_placeholder", op)
  }



 override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
  super.supported_types(availTypes)
 }

 override def ops(map: AvailOps) = {
  super.ops(registerOp(createinternalfunction_placeholder,map))
 }

 //this is a version of GenRandomOps GenArg - the difference is that this one will only consider Symbols that already
 //exist (to increase the liklyhood that the function is actually called)
 def genExistingArg(symssofar: Vector[cTP]): Gen[Vector[cTP]] = for {
  typechoice <- Gen.oneOf(symssofar.map(x => x.tag).filter(p => p.mf != manifest[FunctionMarker]))
  //don't allow passing functions for now
 } yield Vector(cTP(null, typechoice))


 //as previous function.....
 def genExistingArgs(size: Int, symssofar: Vector[cTP]): Gen[Vector[cTP]] =
  for {
   args <- Gen.containerOfN[Vector, Vector[cTP]](size, genExistingArg(symssofar))
  } yield {
   args.foldLeft(Vector.empty[cTP]) { //this is not needed since in this variant it will only ever return single elements
    (acc, ele) => {                  //refactor at some point
     val p1 = acc ++ ele
     if (p1.size > size)
      acc
     else
      p1
    }
   }
  }


 //override in traits that would nest to check if its a valid op in the context
 override def filterNestDepth(desc: CodeDescriptor, fnest: FNest, op: Op): Boolean = {
  if(op == createinternalfunction_placeholder) {
   desc.cur_nest_depth < desc.max_nest_depth && cur_nr_functions < desc.max_functions
  }
  else super.filterNestDepth(desc,fnest,op)
 }


 override def createFunction(desc: CodeDescriptor, op: Op, fNest: FNest): Gen[(Op,Option[(Vector[cTP],Vector[cTP])])] = {
  if (op == createinternalfunction_placeholder){
   println("doing a internal! " + desc.cur_nest_depth)
   cur_nr_functions = cur_nr_functions + 1

   for {
    args <- genExistingArgs(desc.max_args,fNest.syms)
    tail <- genNodes(desc.copy(cur_nodes_per_block = 0, cur_nest_depth = desc.cur_nest_depth + 1),Vector(FNest(args,null,null, Map.empty))) //this doesn't take any outside symbol
   } yield {

    println("seems we survive the recursion")
    val callstack = chainHeadf(tail)
    val callstack_staged = chainHeadsf(tail)

    val exposeargs = genExposeRep(args)
    val exposeres = genExposeRep(tail.last.syms)


    val createinternalfunction: Op = {
     val f: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => {
      /*val ctpv = args.zipWithIndex.map(symwIndex => cTP(x(symwIndex._2),symwIndex._1.tag))
      val resctp = callstack(ctpv)
      resctp.map(t => t.sym)*/
      ???
     }
     val sf: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => {
      /*val ctpv = args.zipWithIndex.map(symwIndex => cTP(x(symwIndex._2),symwIndex._1.tag))
      val resctp = callstackstaged(ctpv)
      resctp.map(t => t.sym)*/
      val lambda = fun(callstack_staged)(exposeargs,exposeres)
      val lambdatp: TP[_] = fun2tp(lambda)
      Vector(lambdatp.sym)
      //Vector(x.head)
     }
     val argsyms: Vector[GenTypes] = args.map(t => t.tag)
     val returnsyms: Vector[GenTypes] = tail.last.syms.map(t => t.tag)
     //val op = OpDescription(argsyms,returnsyms, f, sf)
     val op = OpDescription(Vector.empty,Vector(manifest[FunctionMarker]), f, sf, None)
     //val op = OpDescription(Vector.empty,Vector.empty, f, sf)
     val declaration = Op("createinternalfunction"+fNest.syms.size, op) //giving the op a number depending on the fnest size (not unique in nested case?)


     //val appop = OpDescription(intypes,outtypes,f,)
     val application = Some(Op("applyinternalfunction"+fNest.syms.size, op)) //giving the op a number depending on the fnest size (not unique in nested case?)
     declaration
    }
    val intypes: Vector[cTP] = args
    val outtypes: Vector[cTP] = tail.last.syms
    (createinternalfunction, Some((intypes,outtypes)))
  }
  }
  else super.createFunction(desc,op,fNest)
 }

}
