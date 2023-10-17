package co.blocke.scalajack
package json.collections

import co.blocke.scala_reflection._
import scala.math._
import java.util.UUID
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import scala.collection.immutable._
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions._

class Seqs() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("BigDecimal must work") {
    describe("---------------------\n:  Scala Seq Tests  :\n---------------------", Console.BLUE)
    describe("+++ Primitive Types +++")

    val inst = BigDecimalSeq(null, Seq(BigDecimal(123.456),BigDecimal(78.91)))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[123.456,78.91]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[BigDecimalSeq](js))
  }

  test("BigInt must work") {
    val inst = BigIntSeq(null, List(BigInt(123),BigInt(78)))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[123,78]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[BigIntSeq](js))
  }

  test("Boolean must work") {
    val inst = BooleanSeq(null, scala.collection.mutable.ListBuffer(true,false))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[true,false]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[BooleanSeq](js))
  }

  test("Byte must work") {
    val inst = ByteSeq(null, List(123.toByte,200.toByte))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[123,-56]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[ByteSeq](js))
  }

  test("Char must work") {
    val inst = CharSeq(null, Queue('a','b','c'))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":["a","b","c"]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[CharSeq](js))
  }

  test("Double must work") {
    val inst = DoubleSeq(null, scala.collection.mutable.ArrayBuffer(12.34,56.78))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[12.34,56.78]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[DoubleSeq](js))
  }

  test("Float must work") {
    val inst = FloatSeq(null, LinearSeq(12.34F,56.78F))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[12.34,56.78]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[FloatSeq](js))
  }

  test("Int must work") {
    val inst = IntSeq(null, scala.collection.mutable.IndexedSeq(1,2,3))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[1,2,3]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[IntSeq](js))
  }

  test("Long must work") {
    val inst = LongSeq(null, List(1L,2L,3L))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[1,2,3]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[LongSeq](js))
  }

  test("Short must work") {
    val inst = ShortSeq(null, List(1.toShort,2.toShort,3.toShort))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[1,2,3]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[ShortSeq](js))
  }

  test("String must work") {
    val inst = StringSeq(null, List("a","b","c"))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":["a","b","c"]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[StringSeq](js))
  }

  test("Lists must work") {
    describe("+++ Collection Types +++")
    val inst = SeqSeq(List( List(1,2,3), List(4,5,6) ))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":[[1,2,3],[4,5,6]]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[SeqSeq](js))
  }

  test("Maps must work") {
    val inst = MapSeq(List( Map("a"->1,"b"->2), Map("c"->3,"d"->4) ))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":[{"a":1,"b":2},{"c":3,"d":4}]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[MapSeq](js))
  }

  test("Classes must work") {
    describe("+++ Class Types +++")
    val inst = ClassSeq(List(IntArr(null,Array(1,2)), IntArr(null,Array(1,2))))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":[{"a1":null,"a2":[1,2]},{"a1":null,"a2":[1,2]}]}""".asInstanceOf[JSON],
      js
    )
    val i2 = sj.read[ClassSeq](js)
    assertEquals(i2.a1.toList(0).a1,null)
    assert(inst.a1.toList(0).a2.sameElements(i2.a1.toList(0).a2))
    assertEquals(i2.a1.toList(1).a1,null)
    assert(inst.a1.toList(1).a2.sameElements(i2.a1.toList(1).a2))
  }

  test("Multidimensional arrays must work") {
    describe("+++ Complex Types +++")
    val inst = MultiSeq(null, Seq(Seq( Seq(1L,2L), Seq(3L,4L) ), Seq(Seq(5L,6L), Seq(7L,8L)) ), 
      Seq(Seq(BigInt(12),BigInt(13)), Seq(BigInt(14),BigInt(15))))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":[[[1,2],[3,4]],[[5,6],[7,8]]],"a2":[[12,13],[14,15]]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[MultiSeq](js))
  }


  test("BigDecimal (LinkedList) must work") {
    describe("--------------------\n:  Java Seq Tests  :\n--------------------", Console.BLUE)
    describe("+++ Primitive Types (Mix of List/Set/Queue +++")

    val inst = JBigDecimalSeq(null, new java.util.LinkedList(List(BigDecimal(123.456),BigDecimal(78.91)).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[123.456,78.91]}""".asInstanceOf[JSON],
      js
    )
    val readIn = sj.read[JBigDecimalSeq](js)
    assertEquals(inst.a1, readIn.a1)
    assertEquals(inst.a2, readIn.a2)
  }

  test("BigInt (Vector) must work") {
    val inst = JBigIntSeq(null, new java.util.Vector(List(BigInt(123),BigInt(78)).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[123,78]}""".asInstanceOf[JSON],
      js
    )
    val readIn = sj.read[JBigIntSeq](js)
    assertEquals(inst.a1, readIn.a1)
    assertEquals(inst.a2, readIn.a2)
  }

  test("Boolean (ArrayList) must work") {
    val inst = JBooleanSeq(null, new java.util.ArrayList(List(true,false).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[true,false]}""".asInstanceOf[JSON],
      js
    )
    val readIn = sj.read[JBooleanSeq](js)
    assertEquals(inst.a1, readIn.a1)
    assertEquals(inst.a2, readIn.a2)
  }

  test("Char (PriorityQueue) must work") {
    val inst = JCharSeq(null, new java.util.PriorityQueue(List('a','b','c').asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":["a","b","c"]}""".asInstanceOf[JSON],
      js
    )
    val readIn = sj.read[JCharSeq](js)
    assertEquals(inst.a1, readIn.a1)
    assertEquals(inst.a2.toString, readIn.a2.toString)
  }
  //case class JIntSeq( a1: java.util.Stack[Int], a2: java.util.Stack[Int] )

  test("Int (Stack) must work") {
    val stack = new java.util.Stack[Int]()
    stack.push(1)
    stack.push(2)
    stack.push(3)
    val inst = JIntSeq(null, stack)
    val js = sj.render(inst)
    assertEquals(
      """{"a1":null,"a2":[1,2,3]}""".asInstanceOf[JSON],
      js
    )
    val readIn = sj.read[JIntSeq](js)
    assertEquals(inst.a1, readIn.a1)
    assertEquals(inst.a2, readIn.a2)
  }

  test("Maps must work") {
    val inst = JMapSeq(java.util.ArrayList( List(Map("a"->1,"b"->2), Map("c"->3,"d"->4)).asJava ))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":[{"a":1,"b":2},{"c":3,"d":4}]}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst.a1.asScala, sj.read[JMapSeq](js).a1.asScala)
  }

  test("Classes must work") {
    describe("+++ Class Types +++")
    val inst = JClassSeq( new java.util.LinkedList(List(IntArr(null,Array(1,2)), IntArr(null,Array(1,2))).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":[{"a1":null,"a2":[1,2]},{"a1":null,"a2":[1,2]}]}""".asInstanceOf[JSON],
      js
    )
    val i2 = sj.read[JClassSeq](js)
    assertEquals(i2.a1.get(0).a1, null)
    assert(inst.a1.get(0).a2.sameElements(i2.a1.get(0).a2))
    assertEquals(i2.a1.get(1).a1, null)
    assert(inst.a1.get(1).a2.sameElements(i2.a1.get(1).a2))
  }
