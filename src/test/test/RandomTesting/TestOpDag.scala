package RandomTesting


import org.scalacheck._

import scala.lms.internal.FunctionsExp
import scala.lms.ops.{ImplicitOpsExp, PurePrimitiveOpsExpOpt, BooleanOpsExp}
import scala.lms.targets.scalalike.{ScalaGenPrimitivOps, ScalaGenBooleanOps, EmitHeadInternalFunctionAsClass}


object TestOpDag extends org.scalacheck.Properties("Dag Checking"){

  class MRandomClass extends RandomClass
  with BooleanOpsExp
  with PurePrimitiveOpsExpOpt
  with ImplicitOpsExp
  with FunctionsExp
  with GenRandomBooleanOps
  with GenRandomPrimitiveOps
  { self => override val codegen = new EmitHeadInternalFunctionAsClass with ScalaGenBooleanOps with ScalaGenPrimitivOps{ val IR: self.type = self}  }

  //the actual test object has to define what Random Class is returned
  def iniRandomC(): MRandomClass =  new MRandomClass()

  def getCodeDescription(randomClass: RandomClass): randomClass.CodeDescriptor  =
    randomClass.CodeDescriptor(5, 2, 20, 5, 20, 20 ,1 ,1 ,Map.empty)

  def iniCCStatus(randomClass: RandomClass): randomClass.CCStatus = {
    randomClass.CCStatus(0,0,0,Map.empty)
  }

  abstract class DSLwCode {
    val dsl: MRandomClass
    //val code: Vector[dsl.EvalGenIterStep[dsl.NoRep]]
    //val symbolic_code: Vector[dsl.EvalGenIterStep[dsl.Rep]]
    val dag: dsl.Dag
  }

  //had to introduce this indirection cause otherwise ScalaCheck will assume the class constant
  def genDSL(): Gen[MRandomClass] = {
    def workaround(i: Int) = iniRandomC()
    for {
      nr <- Gen.posNum[Int]
    } yield workaround(nr)
  }

  def genDSLwCode(): Gen[DSLwCode] = //org.scalacheck.Gen.lzy {
  {
    for {
      dslr <- genDSL()
      rdag <- dslr.genCode(getCodeDescription(dslr),iniCCStatus(dslr))
    } yield new DSLwCode {
      override val dsl: dslr.type = dslr
      override val dag: dslr.Dag = rdag
    }
  }

  val dslwCode = genDSLwCode().sample.get
  val dsl = dslwCode.dsl
  val ini = dsl.genArgs(10).sample.get


  property("DSL Dag Graph Removal 2") =
    Prop.forAll(dsl.genCode(getCodeDescription(dsl),iniCCStatus(dsl))) {
      dag => {
        Prop.forAll(dsl.genDagStep(getCodeDescription(dsl), iniCCStatus(dsl), dag))  {
          newdag => {
            val lookup = dag.opLookUp
            val after_insert = newdag.opLookUp
            val opiddiff = (after_insert.opargsrets -- lookup.opargsrets).head
            val op = opiddiff.op
            val returns = opiddiff.returns
            val args = opiddiff.args
            val after_delete = newdag.removeOp(opiddiff)
            import org.scalacheck.Prop.{AnyOperators, forAll, all}
            all(
              "dag size" |:
              dag.dag.size =? after_delete.dag.size,
            "index size" |:
              dag.index.size =? after_delete.index.size
            )
          }
        }
      }
    }




  property("DSL Dag Tests 1") =
    Prop.forAll(dsl.genArgs(5), dsl.genOp(getCodeDescription(dsl),iniCCStatus(dsl),ini)) {
      (dag,op) => {
        val lookup = dag.opLookUp
        val args:Vector[Int] = (for (i <- 0 until op.args.length) yield i).toVector
        val returns = (for (i <- 0 until op.returns.length) yield i+999).toVector
        val after_insert = lookup.insertOp(op,args,returns)
        if (lookup.OpswithoutDep.size != after_insert.OpswithoutDep.size - 1) {
          val after_insert2 = lookup.insertOp(op,args,returns)
        }
        import org.scalacheck.Prop.{AnyOperators, forAll, all}
        all(
         "length ret2rets" |:
           lookup.ret2rets.size =? after_insert.ret2rets.size - returns.length,
        "length ret2op" |:
          lookup.rets2op.size =? after_insert.rets2op.size - 1,
        "length args2ops" |:
          lookup.args2ops.size =? after_insert.args2ops.size - 1,
        "length arg2ops" |:
          lookup.arg2ops.size =? after_insert.arg2ops.size - args.length,
        "opswithoutDep" |:
          lookup.OpswithoutDep.size =? after_insert.OpswithoutDep.size - 1,
        "opargrets set" |:
          lookup.opargsrets.size =? after_insert.opargsrets.size - 1
        )
      }
    }


  property("DSL Dag Tests 2") =
  Prop.forAll(dsl.genCode(getCodeDescription(dsl),iniCCStatus(dsl))) {
    dag => {
      Prop.forAll(dsl.genDagStep(getCodeDescription(dsl), iniCCStatus(dsl), dag))  {
        newdag => {
          val lookup = dag.opLookUp
          val after_insert = newdag.opLookUp
          val opiddiff = (after_insert.opargsrets -- lookup.opargsrets).head
          val op = opiddiff.op
          val returns = opiddiff.returns
          val args = opiddiff.args
          import org.scalacheck.Prop.{AnyOperators, forAll, all}
          all(
            "only one op" |:
              (after_insert.opargsrets -- lookup.opargsrets).size =? 1,
            "length ret2rets" |:
              lookup.ret2rets.size =? after_insert.ret2rets.size - returns.length,
            "length ret2op" |:
              lookup.rets2op.size =? after_insert.rets2op.size - 1,
            "length args2ops" |:
              {
                if (lookup.args2ops.contains(opiddiff.args))
                  lookup.args2ops.size =? after_insert.args2ops.size
                else
                  lookup.args2ops.size =? after_insert.args2ops.size - 1
              },
            "length arg2ops" |:
              {
                val argids1 = lookup.opargsrets.flatMap(o => o.args.toSet)
                val argids2 = after_insert.opargsrets.flatMap(o => o.args.toSet)
                val diff = argids2 -- argids1
                val mapdiff = after_insert.arg2ops.keySet -- lookup.arg2ops.keySet
                diff =? mapdiff
              },
            /*"opswithoutDep" |:
              //lookup.OpswithoutDep.size =? after_insert.OpswithoutDep.size - 1,
              lookup.OpswithoutDep.size =? after_insert.OpswithoutDep.size - 1,*/
            "opargrets set" |:
              lookup.opargsrets.size =? after_insert.opargsrets.size - 1
          )
        }
      }
    }
  }
  property("DSL Tag Simple removal") =
    Prop.forAll(dsl.genArgs(5), dsl.genOp(getCodeDescription(dsl),iniCCStatus(dsl),ini)) {
      (dag,op) => {
        val lookup = dag.opLookUp
        val args:Vector[Int] = (for (i <- 0 until op.args.length) yield i).toVector
        val returns = (for (i <- 0 until op.returns.length) yield i+999).toVector
        val after_insert = lookup.insertOp(op,args,returns)
        val opid = after_insert.rets2op(returns)
        val after_delete = after_insert.delete(opid)

        import org.scalacheck.Prop.{AnyOperators, forAll, all}
        all(
          "length ret2rets" |:
            lookup.ret2rets.size =? after_delete.ret2rets.size,
          "length ret2op" |:
            lookup.rets2op.size =? after_delete.rets2op.size,
          "length args2ops" |:
            lookup.args2ops.size =? after_delete.args2ops.size,
          "length arg2ops" |:
            lookup.arg2ops.size =? after_delete.arg2ops.size,
          "opswithoutDep" |:
            lookup.OpswithoutDep.size =? after_delete.OpswithoutDep.size,
          "opargrets set" |:
            lookup.opargsrets.size =? after_delete.opargsrets.size
        )
      }
    }



  property("DSL Dag Removal 2") =
    Prop.forAll(dsl.genCode(getCodeDescription(dsl),iniCCStatus(dsl))) {
      dag => {
        Prop.forAll(dsl.genDagStep(getCodeDescription(dsl), iniCCStatus(dsl), dag))  {
          newdag => {
            val lookup = dag.opLookUp
            val after_insert = newdag.opLookUp
            val opiddiff = (after_insert.opargsrets -- lookup.opargsrets).head
            val op = opiddiff.op
            val returns = opiddiff.returns
            val args = opiddiff.args
            val after_delete = after_insert.delete(opiddiff)
            import org.scalacheck.Prop.{AnyOperators, forAll, all}
            all(
              "length ret2rets" |:
                lookup.ret2rets.size =? after_delete.ret2rets.size,
              "length ret2op" |:
                lookup.rets2op.size =? after_delete.rets2op.size,
              "length args2ops" |:
                lookup.args2ops.size =? after_delete.args2ops.size,
              "length arg2ops" |:
                lookup.arg2ops.size =? after_delete.arg2ops.size,
              "opswithoutDep" |:
                lookup.OpswithoutDep.size =? after_delete.OpswithoutDep.size,
              "opargrets set" |:
                lookup.opargsrets.size =? after_delete.opargsrets.size
            )
          }
        }
      }
    }





}
