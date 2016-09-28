package RandomTesting

import org.scalacheck._
import Gen._

import scala.lms.ops._
/**
  * Created by rayda on 28-Jul-16.
  */
trait GenRandomStringOps  extends GenRandomOps{
  this: StringOpsExp =>

  
  val string_plus: Op = {
    val f: Function1[Vector[NoRep[_]], Vector[NoRep[_]]] = (x: Vector[NoRep[_]]) => {
      val lhs = x.head.asInstanceOf[String]
      val rhs = x.tail.head.asInstanceOf[String]
      Vector(lhs + rhs)
    }

    val sf: Function1[Vector[Rep[_]], Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
      val l: Rep[String] = x.head.asInstanceOf[Rep[String]]
      val r: Rep[String] = x.tail.head.asInstanceOf[Rep[String]]
      Vector(string_plus(l, r))
    }
    Op("string_plus", Vector(Tag(manifest[String]), Tag(manifest[String])), Vector(Tag(manifest[String])), f, sf, None)
  }

  val string_length: Op = {
    val f: Function1[Vector[NoRep[_]], Vector[NoRep[_]]] = (x: Vector[NoRep[_]]) => {
      val lhs = x.head.asInstanceOf[String]
      Vector(lhs.length())
    }

    val sf: Function1[Vector[Rep[_]], Vector[Rep[_]]] = (x: Vector[Rep[_]]) => {
      val l: Rep[String] = x.head.asInstanceOf[Rep[String]]
      Vector(string_length(l))
    }
    Op("string_length", Vector(Tag(manifest[String])), Vector(Tag(manifest[Int])), f, sf, None)
  }



  override def supported_types(availTypes: AvailTypeTuples): AvailTypeTuples = {
    super.supported_types(availTypes + (Set(Tag(manifest[String]))))
  }

  override def ops(map: AvailOps) = {
    val t = registerOp(string_plus,registerOp(string_length,map))
    super.ops(t)
  }
 /* def string_plus(s: Exp[Any], o: Exp[Any])(implicit pos: SourceContext): Rep[String] = StringPlus(s,o)
  def string_startswith(s1: Exp[String], s2: Exp[String])(implicit pos: SourceContext) = StringStartsWith(s1,s2)
  def string_trim(s: Exp[String])(implicit pos: SourceContext) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String], limit: Exp[Int])(implicit pos: SourceContext) : Rep[Array[String]] = StringSplit(s, separators, limit)
  def string_valueof(a: Exp[Any])(implicit pos: SourceContext) = StringValueOf(a)
  def string_charAt(s: Exp[String], i: Exp[Int])(implicit pos: SourceContext) = StringCharAt(s,i)
  def string_endsWith(s: Exp[String], e: Exp[String])(implicit pos: SourceContext) = StringEndsWith(s,e)
  def string_contains(s1: Exp[String], s2: Exp[String])(implicit pos: SourceContext) = StringContains(s1,s2)
  def string_todouble(s: Rep[String])(implicit pos: SourceContext) = StringToDouble(s)
  def string_tofloat(s: Rep[String])(implicit pos: SourceContext) = StringToFloat(s)
  def string_toint(s: Rep[String])(implicit pos: SourceContext) = StringToInt(s)
  def string_tolong(s: Rep[String])(implicit pos: SourceContext) = StringToLong(s)
  def string_substring(s: Rep[String], start:Rep[Int], end:Rep[Int])(implicit pos: SourceContext) = StringSubstring(s,start,end)
  def string_length(s: Rep[String])(implicit pos: SourceContext) = StringLength(s)*/
}
