package co.blocke.scalajack
package json
package collections

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

import java.util.UUID

class MapSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:          Map Tests          :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Map is null must work") {
        val inst = MapHolder[Int, Int](null)
        val sj = sjCodecOf[MapHolder[Int, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of string must work") {
        val inst = MapHolder[String, Int](Map("x" -> 1, "y" -> 2))
        val sj = sjCodecOf[MapHolder[String, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of long must work") {
        val inst = MapHolder[Long, Int](Map(15L -> 1, 25L -> 2))
        val sj = sjCodecOf[MapHolder[Long, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"15":1,"25":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of boolean must work") {
        val inst = MapHolder[Boolean, Int](Map(true -> 1, false -> 2))
        val sj = sjCodecOf[MapHolder[Boolean, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"true":1,"false":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of uuid must work") {
        val inst = MapHolder[UUID, String](Map(UUID.fromString("1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03") -> "x", UUID.fromString("09abdeb1-8b07-4683-8f97-1f5621696008") -> "y"))
        val sj = sjCodecOf[MapHolder[UUID, String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03":"x","09abdeb1-8b07-4683-8f97-1f5621696008":"y"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of string must work") {
        val inst = MapHolder[String, String](Map("w" -> "x", "y" -> "z"))
        val sj = sjCodecOf[MapHolder[String, String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":"x","y":"z"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of long must work") {
        val inst = MapHolder[String, Long](Map("w" -> 3L, "y" -> 4L))
        val sj = sjCodecOf[MapHolder[String, Long]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":3,"y":4}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of boolean must work") {
        val inst = MapHolder[String, Boolean](Map("w" -> true, "y" -> false))
        val sj = sjCodecOf[MapHolder[String, Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":true,"y":false}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key and value of opaque alias type must work") {
        val inst = MapHolder[OnOff, Counter](Map(true.asInstanceOf[OnOff] -> 1.asInstanceOf[Counter], false.asInstanceOf[OnOff] -> 0.asInstanceOf[Counter]))
        val sj = sjCodecOf[MapHolder[OnOff, Counter]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"true":1,"false":0}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of uuid must work") {
        val inst = MapHolder[String, UUID](Map("x" -> UUID.fromString("1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03"), "y" -> UUID.fromString("09abdeb1-8b07-4683-8f97-1f5621696008")))
        val sj = sjCodecOf[MapHolder[String, UUID]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":"1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03","y":"09abdeb1-8b07-4683-8f97-1f5621696008"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of Seq must work") {
        val inst = MapHolder[String, List[Int]](Map("w" -> List(1, 2), "y" -> List(3, 4)))
        val sj = sjCodecOf[MapHolder[String, List[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":[1,2],"y":[3,4]}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of Map (nested) must work") {
        val inst = MapHolder[String, Map[String, Int]](Map("w" -> Map("r" -> 3, "t" -> 4), "y" -> Map("s" -> 7, "q" -> 9)))
        val sj = sjCodecOf[MapHolder[String, Map[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":{"r":3,"t":4},"y":{"s":7,"q":9}}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of union type must work") {
        val inst = MapHolder[String, Int | List[String]](Map("w" -> 3, "y" -> List("wow", "blah")))
        val sj = sjCodecOf[MapHolder[String, Int | List[String]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":3,"y":["wow","blah"]}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of class must work") {
        val inst = MapHolder[String, Person](Map("w" -> Person("Bob", 34), "y" -> Person("Sally", 25)))
        val sj = sjCodecOf[MapHolder[String, Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":{"name":"Bob","age":34},"y":{"name":"Sally","age":25}}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of value class must work") {
        val inst = MapHolder[Distance, String](Map(new Distance(1.23) -> "x", Distance(4.56) -> "y"))
        val sj = sjCodecOf[MapHolder[Distance, String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"1.23":"x","4.56":"y"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of value class must work") {
        val inst = MapHolder[String, Distance](Map("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MapHolder[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Mutable Map value must work - Map") {
        val inst = MMapHolder[String, Distance](scala.collection.mutable.Map("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MMapHolder[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Mutable Map value must work - HashMap") {
        val inst = MMapHolder2[String, Distance](scala.collection.mutable.HashMap("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MMapHolder2[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Mutable Map value must work - SeqMap (examplar for all other mutable Maps)") {
        val inst = MMapHolder3[String, Distance](scala.collection.mutable.SeqMap("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MMapHolder3[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value must work - HashMap") {
        val inst = MapHolder2[String, Distance](scala.collection.immutable.HashMap("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MapHolder2[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value must work - SeqMap") {
        val inst = MapHolder3[String, Distance](scala.collection.immutable.SeqMap("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MapHolder3[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value must work - VectorMap (examplar for all other immutable Maps)") {
        val inst = MapHolder4[String, Distance](scala.collection.immutable.TreeMap("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MapHolder4[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map keys of remaining types must work (test coverage addition)") {
        val inst = MapHolder[java.lang.Number, Int](Map(java.lang.Integer.valueOf(5).asInstanceOf[java.lang.Number] -> 12))
        val sj = sjCodecOf[MapHolder[java.lang.Number, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"5":12}}""")
        sj.fromJson(js) shouldEqual (inst)

        val inst2 = MapHolder[java.lang.Short, Int](Map(java.lang.Short.valueOf("5") -> 12))
        val sj2 = sjCodecOf[MapHolder[java.lang.Short, Int]]
        val js2 = sj2.toJson(inst2)
        js2 should matchJson("""{"a":{"5":12}}""")
        sj2.fromJson(js2) shouldEqual (inst2)

        val inst3 = MapHolder[java.lang.Long, Int](Map(java.lang.Long.valueOf(5) -> 12))
        val sj3 = sjCodecOf[MapHolder[java.lang.Long, Int]]
        val js3 = sj3.toJson(inst3)
        js3 should matchJson("""{"a":{"5":12}}""")
        sj3.fromJson(js3) shouldEqual (inst3)

        val inst4 = MapHolder[java.lang.Integer, Int](Map(java.lang.Integer.valueOf(5) -> 12))
        val sj4 = sjCodecOf[MapHolder[java.lang.Integer, Int]]
        val js4 = sj4.toJson(inst4)
        js4 should matchJson("""{"a":{"5",12}}""")
        sj4.fromJson(js4) shouldEqual (inst4)

        val inst5 = MapHolder[java.lang.Float, Int](Map(java.lang.Float.valueOf(5) -> 12))
        val sj5 = sjCodecOf[MapHolder[java.lang.Float, Int]]
        val js5 = sj5.toJson(inst5)
        js5 should matchJson("""{"a":{"5.0":12}}""")
        sj5.fromJson(js5) shouldEqual (inst5)

        val inst6 = MapHolder[java.lang.Double, Int](Map(java.lang.Double.valueOf(5) -> 12))
        val sj6 = sjCodecOf[MapHolder[java.lang.Double, Int]]
        val js6 = sj6.toJson(inst6)
        js6 should matchJson("""{"a":{"5.0":12}}""")
        sj6.fromJson(js6) shouldEqual (inst6)

        val inst7 = MapHolder[java.lang.Byte, Int](Map(java.lang.Byte.valueOf("5") -> 12))
        val sj7 = sjCodecOf[MapHolder[java.lang.Byte, Int]]
        val js7 = sj7.toJson(inst7)
        js7 should matchJson("""{"a":{"5":12}}""")
        sj7.fromJson(js7) shouldEqual (inst7)

        val inst8 = MapHolder[java.math.BigDecimal, Int](Map(java.math.BigDecimal.valueOf(5) -> 12))
        val sj8 = sjCodecOf[MapHolder[java.math.BigDecimal, Int]]
        val js8 = sj8.toJson(inst8)
        js8 should matchJson("""{"a":{"5":12}}""")
        sj8.fromJson(js8) shouldEqual (inst8)

        val inst9 = MapHolder[java.math.BigInteger, Int](Map(java.math.BigInteger.valueOf(5) -> 12))
        val sj9 = sjCodecOf[MapHolder[java.math.BigInteger, Int]]
        val js9 = sj9.toJson(inst9)
        js9 should matchJson("""{"a":{"5":12}}""")
        sj9.fromJson(js9) shouldEqual (inst9)

        val inst10 = MapHolder[java.lang.Boolean, Int](Map(java.lang.Boolean.valueOf(true) -> 12))
        val sj10 = sjCodecOf[MapHolder[java.lang.Boolean, Int]]
        val js10 = sj10.toJson(inst10)
        js10 should matchJson("""{"a":{"true":12}}""")
        sj10.fromJson(js10) shouldEqual (inst10)

        val inst11 = MapHolder[Short, Int](Map(5.toShort -> 12))
        val sj11 = sjCodecOf[MapHolder[Short, Int]]
        val js11 = sj11.toJson(inst11)
        js11 should matchJson("""{"a":{"5":12}}""")
        sj11.fromJson(js11) shouldEqual (inst11)

        val inst12 = MapHolder[Byte, Int](Map(5.toByte -> 12))
        val sj12 = sjCodecOf[MapHolder[Byte, Int]]
        val js12 = sj12.toJson(inst12)
        js12 should matchJson("""{"a":{"5":12}}""")
        sj12.fromJson(js12) shouldEqual (inst12)

        val inst13 = MapHolder[Float, Int](Map(5.0.toFloat -> 12))
        val sj13 = sjCodecOf[MapHolder[Float, Int]]
        val js13 = sj13.toJson(inst13)
        js13 should matchJson("""{"a":{"5.0":12}}""")
        sj13.fromJson(js13) shouldEqual (inst13)

        val inst14 = MapHolder[scala.math.BigDecimal, Int](Map(scala.math.BigDecimal(5) -> 12))
        val sj14 = sjCodecOf[MapHolder[scala.math.BigDecimal, Int]]
        val js14 = sj14.toJson(inst14)
        js14 should matchJson("""{"a":{"5":12}}""")
        sj14.fromJson(js14) shouldEqual (inst14)

        val inst15 = MapHolder[scala.math.BigInt, Int](Map(scala.math.BigInt(5) -> 12))
        val sj15 = sjCodecOf[MapHolder[scala.math.BigInt, Int]]
        val js15 = sj15.toJson(inst15)
        js15 should matchJson("""{"a":{"5":12}}""")
        sj15.fromJson(js15) shouldEqual (inst15)

        val inst16 = MapHolder[OnOff, OnOff](Map(true.asInstanceOf[OnOff] -> false.asInstanceOf[OnOff]))
        val sj16 = sjCodecOf[MapHolder[OnOff, OnOff]]
        val js16 = sj16.toJson(inst16)
        js16 should matchJson("""{"a":{"true":false}}""")
        sj16.fromJson(js16) shouldEqual (inst16)

        val inst17 = MapHolder[Boolean, OnOff](Map(true -> false.asInstanceOf[OnOff]))
        val sj17 = sjCodecOf[MapHolder[Boolean, OnOff]]
        val js17 = sj17.toJson(inst17)
        js17 should matchJson("""{"a":{"true":false}}""")
        sj17.fromJson(js17) shouldEqual (inst17)

        val inst18 = MapHolder[OnOff, Boolean](Map(true.asInstanceOf[OnOff] -> false))
        val sj18 = sjCodecOf[MapHolder[OnOff, Boolean]]
        val js18 = sj18.toJson(inst18)
        js18 should matchJson("""{"a":{"true":false}}""")
        sj18.fromJson(js18) shouldEqual (inst18)

        val now = java.time.Instant.now()
        val inst19 = MapHolder[java.time.Instant, Boolean](Map(now -> false))
        val sj19 = sjCodecOf[MapHolder[java.time.Instant, Boolean]]
        val js19 = sj19.toJson(inst19)
        sj19.fromJson(js19) shouldEqual (inst19)
      }
    }
  }
