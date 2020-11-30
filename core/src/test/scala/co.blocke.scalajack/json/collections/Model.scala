package co.blocke.scalajack
package json.collections

import scala.math._
import scala.collection.Map
import scala.collection.Seq

//------ Arrays (Scala)
case class BigDecimalArr( a1: Array[BigDecimal], a2: Array[BigDecimal] )
case class BigIntArr( a1: Array[BigInt], a2: Array[BigInt] )
case class BooleanArr( a1: Array[Boolean], a2: Array[Boolean] )
case class CharArr( a1: Array[Char], a2: Array[Char] )
case class DoubleArr( a1: Array[Double], a2: Array[Double] )
case class FloatArr( a1: Array[Float], a2: Array[Float] )
case class IntArr( a1: Array[Int], a2: Array[Int] )
case class LongArr( a1: Array[Long], a2: Array[Long] )
case class ShortArr( a1: Array[Short], a2: Array[Short] )
case class StringArr( a1: Array[String], a2: Array[String] )

case class MultiArr( a0: Array[Array[Boolean]], a1: Array[Array[Array[Long]]], a2: Array[Array[BigInt]] )
case class ClassArr( a1: Array[IntArr] )

case class ListArr( a1: Array[List[Int]])
case class SetArr( a1: Array[Set[Int]])
case class MapArr( a1: Array[Map[String,Int]])

//------ Seqs (Scala)
case class BigDecimalSeq( a1: Seq[BigDecimal], a2: Seq[BigDecimal] )
case class BigIntSeq( a1: Seq[BigInt], a2: Seq[BigInt] )
case class BooleanSeq( a1: Seq[Boolean], a2: Seq[Boolean] )
case class ByteSeq( a1: Seq[Byte], a2: Seq[Byte] )
case class CharSeq( a1: Seq[Char], a2: Seq[Char] )
case class DoubleSeq( a1: Seq[Double], a2: Seq[Double] )
case class FloatSeq( a1: Seq[Float], a2: Seq[Float] )
case class IntSeq( a1: Seq[Int], a2: Seq[Int] )
case class LongSeq( a1: Seq[Long], a2: Seq[Long] )
case class ShortSeq( a1: Seq[Short], a2: Seq[Short] )
case class StringSeq( a1: Seq[String], a2: Seq[String] )

case class MultiSeq( a0: Seq[Seq[Boolean]], a1: Seq[Seq[Seq[Long]]], a2: Seq[Seq[BigInt]] )
case class ClassSeq( a1: Seq[IntArr] )

case class SeqSeq( a1: Seq[List[Int]])
case class MapSeq( a1: Seq[Map[String,Int]])

//------ Seqs (Java)
case class JBigDecimalSeq( a1: java.util.LinkedList[BigDecimal], a2: java.util.LinkedList[BigDecimal] )
case class JBigIntSeq( a1: java.util.Vector[BigInt], a2: java.util.Vector[BigInt] )
case class JBooleanSeq( a1: java.util.ArrayList[Boolean], a2: java.util.ArrayList[Boolean] )
case class JCharSeq( a1: java.util.PriorityQueue[Char], a2: java.util.PriorityQueue[Char] )
case class JIntSeq( a1: java.util.Stack[Int], a2: java.util.Stack[Int] )
// Other primitive-type tests would simply be redundant in this series... we've already confirmed Java primtivie serializations.
case class JClassSeq( a1: java.util.LinkedList[IntArr] )
case class JMapSeq( a1: java.util.ArrayList[Map[String,Int]])


//------ Maps (Some "2" variants to avoid same-name collision with Scala collection classes)
case class BigDecimalMap( a0: Map[BigDecimal,String], a1: Map[BigDecimal,String], a2: Map[String,BigDecimal] )
case class BigIntMap( a0: Map[BigInt,String], a1: Map[BigInt,String], a2: Map[String,BigInt] )
case class BooleanMap( a0: Map[Boolean,String], a1: Map[Boolean,String], a2: Map[String,Boolean] )
case class ByteMap( a0: Map[Byte,String], a1: Map[Byte,String], a2: Map[String,Byte] )
case class CharMap( a0: Map[Char,String], a1: Map[Char,String], a2: Map[String,Char] )
case class DoubleMap( a0: Map[Double,String], a1: Map[Double,String], a2: Map[String,Double] )
case class FloatMap( a0: Map[Float,String], a1: Map[Float,String], a2: Map[String,Float] )
case class IntMap2( a0: Map[Int,String], a1: Map[Int,String], a2: Map[String,Int] )
case class LongMap2( a0: Map[Long,String], a1: Map[Long,String], a2: Map[String,Long] )
case class ShortMap2( a0: Map[Short,String], a1: Map[Short,String], a2: Map[String,Short] )

case class MultiMap( a1: Map[Map[String,Boolean],Int], a2: Map[Int,Map[String,Boolean]] )
case class ClassMap( a1: Map[String,IntArr], a2: Map[IntArr,String] )
case class ArrayMap( a1: Map[String,Array[Int]], a2:Map[Array[Int],String])
case class SeqMap2( a1: Map[String,Seq[Int]], a2: Map[Seq[Int],String])

//------ Maps (Java)
case class JBigDecimalMap( a0: java.util.HashMap[BigDecimal,String], a1: java.util.HashMap[BigDecimal,String], a2: java.util.HashMap[String,BigDecimal] )
case class JBigIntMap( a0: java.util.HashMap[BigInt,String], a1: java.util.HashMap[BigInt,String], a2: java.util.HashMap[String,BigInt] )
case class JBooleanMap( a0: java.util.HashMap[Boolean,String], a1: java.util.HashMap[Boolean,String], a2: java.util.HashMap[String,Boolean] )
case class JByteMap( a0: java.util.HashMap[Byte,String], a1: java.util.HashMap[Byte,String], a2: java.util.HashMap[String,Byte] )
case class JCharMap( a0: java.util.HashMap[Char,String], a1: java.util.HashMap[Char,String], a2: java.util.HashMap[String,Char] )
case class JDoubleMap( a0: java.util.HashMap[Double,String], a1: java.util.HashMap[Double,String], a2: java.util.HashMap[String,Double] )
case class JFloatMap( a0: java.util.HashMap[Float,String], a1: java.util.HashMap[Float,String], a2: java.util.HashMap[String,Float] )
case class JIntMap2( a0: java.util.HashMap[Int,String], a1: java.util.HashMap[Int,String], a2: java.util.HashMap[String,Int] )
case class JLongMap2( a0: java.util.HashMap[Long,String], a1: java.util.HashMap[Long,String], a2: java.util.HashMap[String,Long] )
case class JShortMap2( a0: java.util.HashMap[Short,String], a1: java.util.HashMap[Short,String], a2: java.util.HashMap[String,Short] )

case class JMultiMap( a1: java.util.HashMap[java.util.HashMap[String,Boolean],Int], a2: java.util.HashMap[Int,java.util.HashMap[String,Boolean]] )
case class JClassMap( a1: java.util.HashMap[String,IntSeq], a2: java.util.HashMap[IntSeq,String] )
case class JArrayMap( a1: java.util.HashMap[String,Array[Int]], a2:java.util.HashMap[Array[Int],String])
case class JSeqMap2( a1: java.util.HashMap[String,scala.collection.mutable.Seq[Int]], a2: java.util.HashMap[scala.collection.mutable.Seq[Int],String])

//------- Options
trait Person { val name: String }
case class SomeClass(name: String, age: Int) extends Person
trait Thing[A, B] { val a: A; val b: B }
case class AThing[Y, X](a: X, b: Y) extends Thing[X, Y]
case class OptionBigInt(o: Option[BigInt])
case class OptionClass(name: String, age: Option[Int])
case class OptionTuple(foo: Int, t: (Boolean, Option[String], Int))

case class OptionalBigInt(o: java.util.Optional[BigInt])
case class OptionalClass(name: String, age: java.util.Optional[Int])
case class OptionalTuple(foo: Int, t: (Boolean, java.util.Optional[String], Int))

//------- Any
case class Player(name: String, age: Int)