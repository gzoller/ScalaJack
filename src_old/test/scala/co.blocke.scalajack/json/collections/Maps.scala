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
import scala.language.implicitConversions

class Maps() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("BigDecimal must work") {
    describe("---------------------\n:  Scala Map Tests  :\n---------------------", Console.BLUE)
    describe("+++ Primitive Types +++")

    val inst = BigDecimalMap(null, Map(BigDecimal(123.456)->"a",BigDecimal(78.91)->"b"), Map("a"->BigDecimal(123.456),"b"->BigDecimal(78.91)))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"123.456":"a","78.91":"b"},"a2":{"a":123.456,"b":78.91}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[BigDecimalMap](js))
  }

  test("BigInt must work") {
    val inst = BigIntMap(null, Map(BigInt(123)->"a",BigInt(456)->"b"), Map("a"->BigInt(789),"b"->BigInt(321)))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"123":"a","456":"b"},"a2":{"a":789,"b":321}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[BigIntMap](js))
  }

  test("Boolean must work") {
    val inst = BooleanMap(null, Map(true->"a",false->"b"), Map("a"->true,"b"->false))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"true":"a","false":"b"},"a2":{"a":true,"b":false}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[BooleanMap](js))
  }

  test("Byte must work") {
    val inst = ByteMap(null, Map(250.toByte->"a",200.toByte->"b"), Map("a"->150.toByte,"b"->100.toByte))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"-6":"a","-56":"b"},"a2":{"a":-106,"b":100}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[ByteMap](js))
  }

  test("Char must work") {
    val inst = CharMap(null, Map('t'->"a",'u'->"b"), Map("a"->'v',"b"->'w'))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"t":"a","u":"b"},"a2":{"a":"v","b":"w"}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[CharMap](js))
  }

  test("Double must work") {
    val inst = DoubleMap(null, Map(12.34->"a",45.67->"b"), Map("a"->67.89,"b"->1923.432))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12.34":"a","45.67":"b"},"a2":{"a":67.89,"b":1923.432}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[DoubleMap](js))
  }

  test("Float must work") {
    val inst = FloatMap(null, Map(12.34F->"a",45.67F->"b"), Map("a"->67.89F,"b"->1923.432F))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12.34":"a","45.67":"b"},"a2":{"a":67.89,"b":1923.432}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[FloatMap](js))
  }

  test("Int must work") {
    val inst = IntMap2(null, Map(12->"a",-45->"b"), Map("a"->67,"b"->1923))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12":"a","-45":"b"},"a2":{"a":67,"b":1923}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[IntMap2](js))
  }

  test("Long must work") {
    val inst = LongMap2(null, Map(12L->"a",-45L->"b"), Map("a"->67L,"b"->1923L))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12":"a","-45":"b"},"a2":{"a":67,"b":1923}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[LongMap2](js))
  }

  test("Short must work") {
    val inst = ShortMap2(null, Map(12.toShort->"a",-45.toShort->"b"), Map("a"->67.toShort,"b"->19.toShort))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12":"a","-45":"b"},"a2":{"a":67,"b":19}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[ShortMap2](js))
  }

  test("Arrays must work") {
    describe("+++ Collection Types +++")
    val inst = ArrayMap( Map("a"->Array(1,2,3),"b"->Array(4,5,6)), Map(Array(1,2,3)->"a",Array(4,5,6)->"b") )
    val js = sj.render(inst)
    assertEquals(
      """{"a1":{"a":[1,2,3],"b":[4,5,6]},"a2":{"[1,2,3]":"a","[4,5,6]":"b"}}""".asInstanceOf[JSON],
      js
    )
    val i2 = sj.read[ArrayMap](js)
    assert( inst.a1("a").sameElements(i2.a1("a")) )
    assert( inst.a1("b").sameElements(i2.a1("b")) )
    assert( inst.a2.keySet.toList(0).sameElements(i2.a2.keySet.toList(0)))
    assert( inst.a2.keySet.toList(1).sameElements(i2.a2.keySet.toList(1)))
  }

  test("Maps must work") {
    val inst = SeqMap2( Map("a"->List(1,2,3),"b"->List(4,5,6)), Map(List(1,2,3)->"a",List(4,5,6)->"b") )
    val js = sj.render(inst)
    assertEquals(
      """{"a1":{"a":[1,2,3],"b":[4,5,6]},"a2":{"[1,2,3]":"a","[4,5,6]":"b"}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[SeqMap2](js))
  }

  test("Classes must work") {
    describe("+++ Class Types +++")
    val inst = ClassMap(Map("a"->IntArr(Array(1,2),null),"b"->IntArr(Array(3,4),null)), Map(IntArr(Array(1,2),null)->"a",IntArr(Array(3,4),null)->"b"))
    val js = sj.render(inst)
    assertEquals(
      """{"a1":{"a":{"a1":[1,2],"a2":null},"b":{"a1":[3,4],"a2":null}},"a2":{"{\"a1\":[1,2],\"a2\":null}":"a","{\"a1\":[3,4],\"a2\":null}":"b"}}""".asInstanceOf[JSON],
      js
    )
    val i2 = sj.read[ClassMap](js)
    assert( inst.a1("a").a1.sameElements(i2.a1("a").a1) )
    assertEquals( i2.a1("a").a2, null )
    assert( inst.a1("b").a1.sameElements(i2.a1("b").a1) )
    assertEquals( i2.a1("b").a2, null )
    assert( inst.a2.keySet.toList(0).a1.sameElements(i2.a2.keySet.toList(0).a1) )
    assertEquals( i2.a2.keySet.toList(0).a2, null )
    assert( inst.a2.keySet.toList(1).a1.sameElements(i2.a2.keySet.toList(1).a1) )
    assertEquals( i2.a2.keySet.toList(1).a2, null )
  }

  test("Multidimensional arrays must work") {
    describe("+++ Complex Types +++")
    val inst = MultiMap( Map( Map("a"->true,"b"->false)->1, Map("c"->true,"d"->false)->2), Map( 3->Map("a"->true,"b"->false), 4->Map("c"->true,"d"->false)) )
    val js = sj.render(inst)
    assertEquals(
      """{"a1":{"{\"a\":true,\"b\":false}":1,"{\"c\":true,\"d\":false}":2},"a2":{"3":{"a":true,"b":false},"4":{"c":true,"d":false}}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[MultiMap](js))
  }

  test("BigDecimal must work") {
    describe("--------------------\n:  Java Map Tests  :\n--------------------", Console.BLUE)
    describe("+++ Primitive Types +++")

    val inst = JBigDecimalMap(null, new java.util.HashMap( Map(BigDecimal(123.456)->"a",BigDecimal(78.91)->"b").asJava ), java.util.HashMap( Map("a"->BigDecimal(123.456),"b"->BigDecimal(78.91)).asJava ))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"123.456":"a","78.91":"b"},"a2":{"a":123.456,"b":78.91}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JBigDecimalMap](js))
  }

  test("BigInt must work") {
    val inst = JBigIntMap(null, new java.util.HashMap(Map(BigInt(123)->"a",BigInt(456)->"b").asJava), new java.util.HashMap(Map("a"->BigInt(789),"b"->BigInt(321)).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"456":"b","123":"a"},"a2":{"a":789,"b":321}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JBigIntMap](js))
  }

  test("Boolean must work") {
    val inst = JBooleanMap(null, java.util.HashMap(Map(true->"a",false->"b").asJava), java.util.HashMap(Map("a"->true,"b"->false).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"false":"b","true":"a"},"a2":{"a":true,"b":false}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JBooleanMap](js))
  }

  test("Byte must work") {
    val inst = JByteMap(null, java.util.HashMap(Map(250.toByte->"a",200.toByte->"b").asJava), java.util.HashMap(Map("a"->150.toByte,"b"->100.toByte).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"-6":"a","-56":"b"},"a2":{"a":-106,"b":100}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JByteMap](js))
  }

  test("Char must work") {
    val inst = JCharMap(null, java.util.HashMap(Map('t'->"a",'u'->"b").asJava), java.util.HashMap(Map("a"->'v',"b"->'w').asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"t":"a","u":"b"},"a2":{"a":"v","b":"w"}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JCharMap](js))
  }

  test("Double must work") {
    val inst = JDoubleMap(null, java.util.HashMap(Map(12.34->"a",45.67->"b").asJava), java.util.HashMap(Map("a"->67.89,"b"->1923.432).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"45.67":"b","12.34":"a"},"a2":{"a":67.89,"b":1923.432}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JDoubleMap](js))
  }

  test("Float must work") {
    val inst = JFloatMap(null, java.util.HashMap(Map(12.34F->"a",45.67F->"b").asJava), java.util.HashMap(Map("a"->67.89F,"b"->1923.432F).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12.34":"a","45.67":"b"},"a2":{"a":67.89,"b":1923.432}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JFloatMap](js))
  }

  test("Int must work") {
    val inst = JIntMap2(null, java.util.HashMap(Map(12->"a",-45->"b").asJava), java.util.HashMap(Map("a"->67,"b"->1923).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12":"a","-45":"b"},"a2":{"a":67,"b":1923}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JIntMap2](js))
  }

  test("Long must work") {
    val inst = JLongMap2(null, java.util.HashMap(Map(12L->"a",-45L->"b").asJava), java.util.HashMap(Map("a"->67L,"b"->1923L).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12":"a","-45":"b"},"a2":{"a":67,"b":1923}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JLongMap2](js))
  }

  test("Short must work") {
    val inst = JShortMap2(null, java.util.HashMap(Map(12.toShort->"a",-45.toShort->"b").asJava), java.util.HashMap(Map("a"->67.toShort,"b"->19.toShort).asJava))
    val js = sj.render(inst)
    assertEquals(
      """{"a0":null,"a1":{"12":"a","-45":"b"},"a2":{"a":67,"b":19}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JShortMap2](js))
  }

  test("Lists must work") {
    val hm1 = new java.util.HashMap[String,scala.collection.mutable.Seq[Int]]()
    hm1.put("a",scala.collection.mutable.Seq(1,2,3) )
    hm1.put("b",scala.collection.mutable.Seq(4,5,6) )
    val hm2 = new java.util.HashMap[scala.collection.mutable.Seq[Int],String]()
    hm2.put(scala.collection.mutable.Seq(1,2,3),"a" )
    hm2.put(scala.collection.mutable.Seq(4,5,6),"b" )
    
    val inst = JSeqMap2(hm1, hm2)
    val js = sj.render(inst)
    assertEquals(
      """{"a1":{"a":[1,2,3],"b":[4,5,6]},"a2":{"[1,2,3]":"a","[4,5,6]":"b"}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JSeqMap2](js))
  }

  test("Classes must work") {
    describe("+++ Class Types +++")

    val hm1 = new java.util.HashMap[String,IntSeq]()
    hm1.put("a",IntSeq(List(1,2),null))
    hm1.put("b",IntSeq(List(3,4),null))
    val hm2 = new java.util.HashMap[IntSeq,String]()
    hm2.put(IntSeq(List(1,2),null),"a")
    hm2.put(IntSeq(List(3,4),null),"b")

    val inst = JClassMap(hm1, hm2)
    val js = sj.render(inst)
    assertEquals(
      """{"a1":{"a":{"a1":[1,2],"a2":null},"b":{"a1":[3,4],"a2":null}},"a2":{"{\"a1\":[1,2],\"a2\":null}":"a","{\"a1\":[3,4],\"a2\":null}":"b"}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JClassMap](js))
  }

  test("Multidimensional arrays must work") {
    describe("+++ Complex Types +++")

    //case class JMultiMap( a1: java.util.HashMap[java.util.HashMap[String,Boolean],Int], a2: java.util.HashMap[Int,java.util.HashMap[String,Boolean]] )
    val m1: java.util.HashMap[String,Boolean] = java.util.HashMap( Map( ("a",true), ("b",false) ).asJava )
    val m2: java.util.HashMap[String,Boolean] = java.util.HashMap( Map( ("c",true), ("d",false) ).asJava )
    val inst = JMultiMap(
      java.util.HashMap(
        Map(
          (m1, 1), 
          (m2, 2)
        ).asJava
      ), 
      java.util.HashMap(
        Map(
          (1, m1), 
          (2, m2)
        ).asJava
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"a1":{"{\"a\":true,\"b\":false}":1,"{\"d\":false,\"c\":true}":2},"a2":{"1":{"a":true,"b":false},"2":{"d":false,"c":true}}}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[JMultiMap](js))
  }
