package test

import org.scalacheck._


import scala.lms.BaseExp
import scala.lms.ops._


trait GenRandomFunctions extends GenRandomOps{
 this: BaseExp  =>



 //this is just a placeholder which will always be a fitting option during Op selection
  val createinternalfunction_placeholder: Op = {
   val f: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => ???
   val sf: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => ???
   val op = OpDescription(Vector(manifest[Wildcard1]), Vector(manifest[Wildcard2]), f, sf)
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
  typechoice <- Gen.oneOf(symssofar.map(x => x.tag))
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
  if(op == createinternalfunction_placeholder) desc.max_nest_depth > 0
  else super.filterNestDepth(desc,fnest,op)
 }


 override def createFunction(desc: CodeDescriptor, op: Op, fNest: FNest): Gen[Op] = {
  if (op == createinternalfunction_placeholder){
   for {
    args <- genExistingArgs(desc.max_args,fNest.syms)
    tail <- genNodes(desc.copy(max_nest_depth = desc.max_nest_depth -1),Vector(FNest(args,null,null))) //this doesn't take any outside symbol
   } yield {

    val callstack = chainHeadf(tail)
    val callstackstaged = chainHeadsf(tail)

    val createinternalfunction: Op = {
     val f: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => {
      val ctpv = args.zipWithIndex.map(symwIndex => cTP(x(symwIndex._2),symwIndex._1.tag))
      val resctp = callstack(ctpv)
      resctp.map(t => t.sym)
     }
     val sf: Function1[Vector[_], Vector[_]] = (x: Vector[_]) => {
      val ctpv = args.zipWithIndex.map(symwIndex => cTP(x(symwIndex._2),symwIndex._1.tag))
      val resctp = callstackstaged(ctpv)
      resctp.map(t => t.sym)
     }
     val argsyms: Vector[GenTypes] = args.map(t => t.tag)
     val returnsyms: Vector[GenTypes] = tail.last.syms.map(t => t.tag)
     val op = OpDescription(argsyms,returnsyms, f, sf)
     Op("createinternalfunction"+fNest.syms.size, op) //giving the op a number depending on the fnest size (not unique in nested case?)
    }
    createinternalfunction
   }
  }
  else op
 }

}
