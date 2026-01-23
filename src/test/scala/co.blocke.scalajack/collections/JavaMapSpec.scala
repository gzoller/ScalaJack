package co.blocke.scalajack
package json
package collections

import ScalaJack.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

import java.util.UUID

class JavaMapSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:       Java Map Tests        :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ General Map Interface Tests +++")) {
      it("Map is null must work") {
        val inst = JMapHolder[Int, Int](null)
        val sj = sjCodecOf[JMapHolder[Int, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of string must work") {
        val m: java.util.Map[String, Int] = new java.util.HashMap[String, Int]()
        m.put("x", 1)
        m.put("y", 2)
        val inst = JMapHolder[String, Int](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of long must work") {
        val m: java.util.Map[Long, Int] = new java.util.HashMap[Long, Int]()
        m.put(15L, 1)
        m.put(25L, 2)
        val inst = JMapHolder[Long, Int](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[Long, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"15":1,"25":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of boolean must work") {
        val m: java.util.Map[Boolean, Int] = new java.util.HashMap[Boolean, Int]()
        m.put(true, 1)
        m.put(false, 2)
        val inst = JMapHolder[Boolean, Int](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[Boolean, Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"true":1,"false":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of uuid must work") {
        val m: java.util.Map[UUID, String] = new java.util.HashMap[UUID, String]()
        m.put(UUID.fromString("1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03"), "x")
        m.put(UUID.fromString("09abdeb1-8b07-4683-8f97-1f5621696008"), "y")
        val inst = JMapHolder[UUID, String](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[UUID, String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03":"x","09abdeb1-8b07-4683-8f97-1f5621696008":"y"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map key of value class must work") {
        val m: java.util.Map[Distance, String] = new java.util.HashMap[Distance, String]()
        m.put(Distance(1.23), "w")
        m.put(Distance(4.56), "y")
        val inst = JMapHolder[Distance, String](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[Distance, String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"1.23":"w","4.56":"y"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of string must work") {
        val m: java.util.Map[String, String] = new java.util.HashMap[String, String]()
        m.put("w", "x")
        m.put("y", "z")
        val inst = JMapHolder[String, String](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, String]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":"x","y":"z"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of long must work") {
        val m: java.util.Map[String, Long] = new java.util.HashMap[String, Long]()
        m.put("w", 3L)
        m.put("y", 4L)
        val inst = JMapHolder[String, Long](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, Long]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":3,"y":4}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of boolean must work") {
        val m: java.util.Map[String, Boolean] = new java.util.HashMap[String, Boolean]()
        m.put("w", true)
        m.put("y", false)
        val inst = JMapHolder[String, Boolean](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":true,"y":false}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of uuid must work") {
        val m: java.util.Map[String, UUID] = new java.util.HashMap[String, UUID]()
        m.put("x", UUID.fromString("1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03"))
        m.put("y", UUID.fromString("09abdeb1-8b07-4683-8f97-1f5621696008"))
        val inst = JMapHolder[String, UUID](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, UUID]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":"1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03","y":"09abdeb1-8b07-4683-8f97-1f5621696008"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of Seq must work") {
        val m: java.util.Map[String, List[Int]] = new java.util.HashMap[String, List[Int]]()
        m.put("w", List(1, 2))
        m.put("y", List(3, 4))
        val inst = JMapHolder[String, List[Int]](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, List[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":[1,2],"y":[3,4]}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of Map (nested) must work") {
        val m: java.util.Map[String, Map[String, Int]] = new java.util.HashMap[String, Map[String, Int]]()
        m.put("w", Map("r" -> 3, "t" -> 4))
        m.put("y", Map("s" -> 7, "q" -> 9))
        val inst = JMapHolder[String, Map[String, Int]](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, Map[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":{"r":3,"t":4},"y":{"s":7,"q":9}}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of class must work") {
        val m: java.util.Map[String, Person] = new java.util.HashMap[String, Person]()
        m.put("w", Person("Bob", 34))
        m.put("y", Person("Sally", 25))
        val inst = JMapHolder[String, Person](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, Person]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":{"name":"Bob","age":34},"y":{"name":"Sally","age":25}}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of union type must work") {
        val m: java.util.Map[String, Int | List[String]] = new java.util.HashMap[String, Int | List[String]]()
        m.put("w", 3)
        m.put("y", List("wow", "blah"))
        val inst = JMapHolder[String, Int | List[String]](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, Int | List[String]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":3,"y":["wow","blah"]}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Map value of value class must work") {
        val m: java.util.Map[String, Distance] = new java.util.HashMap[String, Distance]()
        m.put("w", new Distance(1.23))
        m.put("y", Distance(4.56))
        val inst = JMapHolder[String, Distance](new java.util.HashMap(m))
        val sj = sjCodecOf[JMapHolder[String, Distance]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
    }
    describe(colorString("+++ Special/Specific Map Tests +++")) {
      it("NavigableMap must work") {
        val m: java.util.NavigableMap[String, Int] = new java.util.TreeMap[String, Int]()
        m.put("x", 1)
        m.put("y", 2)
        val inst = Holder[java.util.NavigableMap[String, Int]](m)
        val sj = sjCodecOf[Holder[java.util.NavigableMap[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("SortedMap must work") {
        val m: java.util.SortedMap[String, Int] = new java.util.TreeMap[String, Int]()
        m.put("x", 1)
        m.put("y", 2)
        val inst = Holder[java.util.SortedMap[String, Int]](m)
        val sj = sjCodecOf[Holder[java.util.SortedMap[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("TreeMap must work") {
        val m = new java.util.TreeMap[String, Int]()
        m.put("x", 1)
        m.put("y", 2)
        val inst = Holder[java.util.TreeMap[String, Int]](m)
        val sj = sjCodecOf[Holder[java.util.TreeMap[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ConcurrentMap must work") {
        val m = new java.util.concurrent.ConcurrentHashMap[String, Int]()
        m.put("x", 1)
        m.put("y", 2)
        val inst = Holder[java.util.concurrent.ConcurrentMap[String, Int]](m)
        val sj = sjCodecOf[Holder[java.util.concurrent.ConcurrentMap[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("ConcurrentNavigableMap must work") {
        val m = new java.util.concurrent.ConcurrentSkipListMap[String, Int]()
        m.put("x", 1)
        m.put("y", 2)
        val inst = Holder[java.util.concurrent.ConcurrentNavigableMap[String, Int]](m)
        val sj = sjCodecOf[Holder[java.util.concurrent.ConcurrentNavigableMap[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Specific must work (eg HashMap)") {
        val m = new java.util.HashMap[String, Int]()
        m.put("x", 1)
        m.put("y", 2)
        val inst = Holder[java.util.HashMap[String, Int]](m)
        val sj = sjCodecOf[Holder[java.util.HashMap[String, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
    }
  }
