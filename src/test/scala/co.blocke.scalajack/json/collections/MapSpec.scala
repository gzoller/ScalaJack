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
      it("Map value must work - SeqMap (examplar for all other immutable Maps)") {
        val inst = MapHolder3[String, Distance](scala.collection.immutable.SeqMap("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val sj = sjCodecOf[MapHolder3[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
    }
  }
