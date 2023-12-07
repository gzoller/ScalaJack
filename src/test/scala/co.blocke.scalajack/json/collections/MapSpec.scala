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
        val js = sj[MapHolder[Int, Int]].toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Map key of string must work") {
        val inst = MapHolder[String, Int](Map("x" -> 1, "y" -> 2))
        val js = sj[MapHolder[String, Int]].toJson(inst)
        js should matchJson("""{"a":{"x":1,"y":2}}""")
      }
      it("Map key of long must work") {
        val inst = MapHolder[Long, Int](Map(15L -> 1, 25L -> 2))
        val js = sj[MapHolder[Long, Int]].toJson(inst)
        js should matchJson("""{"a":{"15":1,"25":2}}""")
      }
      it("Map key of boolean must work") {
        val inst = MapHolder[Boolean, Int](Map(true -> 1, false -> 2))
        val js = sj[MapHolder[Boolean, Int]].toJson(inst)
        js should matchJson("""{"a":{"true":1,"false":2}}""")
      }
      it("Map key of uuid must work") {
        val inst = MapHolder[UUID, String](Map(UUID.fromString("1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03") -> "x", UUID.fromString("09abdeb1-8b07-4683-8f97-1f5621696008") -> "y"))
        val js = sj[MapHolder[UUID, String]].toJson(inst)
        js should matchJson("""{"a":{"1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03":"x","09abdeb1-8b07-4683-8f97-1f5621696008":"y"}}""")
      }

      it("Map value of string must work") {
        val inst = MapHolder[String, String](Map("w" -> "x", "y" -> "z"))
        val js = sj[MapHolder[String, String]].toJson(inst)
        js should matchJson("""{"a":{"w":"x","y":"z"}}""")
      }
      it("Map value of long must work") {
        val inst = MapHolder[String, Long](Map("w" -> 3L, "y" -> 4L))
        val js = sj[MapHolder[String, Long]].toJson(inst)
        js should matchJson("""{"a":{"w":3,"y":4}}""")
      }
      it("Map value of boolean must work") {
        val inst = MapHolder[String, Boolean](Map("w" -> true, "y" -> false))
        val js = sj[MapHolder[String, Boolean]].toJson(inst)
        js should matchJson("""{"a":{"w":true,"y":false}}""")
      }
      it("Map value of uuid must work") {
        val inst = MapHolder[String, UUID](Map("x" -> UUID.fromString("1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03"), "y" -> UUID.fromString("09abdeb1-8b07-4683-8f97-1f5621696008")))
        val js = sj[MapHolder[String, UUID]].toJson(inst)
        js should matchJson("""{"a":{"x":"1b9ab03f-26a3-4ec5-a8dd-d5122ff86b03","y":"09abdeb1-8b07-4683-8f97-1f5621696008"}}""")
      }
      it("Map value of Seq must work") {
        val inst = MapHolder[String, List[Int]](Map("w" -> List(1, 2), "y" -> List(3, 4)))
        val js = sj[MapHolder[String, List[Int]]].toJson(inst)
        js should matchJson("""{"a":{"w":[1,2],"y":[3,4]}}""")
      }
      it("Map value of Map (nested) must work") {
        val inst = MapHolder[String, Map[String, Int]](Map("w" -> Map("r" -> 3, "t" -> 4), "y" -> Map("s" -> 7, "q" -> 9)))
        val js = sj[MapHolder[String, Map[String, Int]]].toJson(inst)
        js should matchJson("""{"a":{"w":{"r":3,"t":4},"y":{"s":7,"q":9}}}""")
      }
      it("Map value of class must work") {
        val inst = MapHolder[String, Person](Map("w" -> Person("Bob", 34), "y" -> Person("Sally", 25)))
        val js = sj[MapHolder[String, Person]].toJson(inst)
        js should matchJson("""{"a":{"w":{"name":"Bob","age":34},"y":{"name":"Sally","age":25}}}""")
      }
      it("Map value of union type must work") {
        val inst = MapHolder[String, Int | List[String]](Map("w" -> 3, "y" -> List("wow", "blah")))
        val js = sj[MapHolder[String, Int | List[String]]].toJson(inst)
        js should matchJson("""{"a":{"w":3,"y":["wow","blah"]}}""")
      }
      it("Map value of value class must work") {
        val inst = MapHolder[String, Distance](Map("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val js = sj[MapHolder[String, Distance]].toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
      }
      it("Mutable Map value must work") {
        val inst = MMapHolder[String, Distance](scala.collection.mutable.HashMap("w" -> new Distance(1.23), "y" -> Distance(4.56)))
        val js = sj[MMapHolder[String, Distance]].toJson(inst)
        js should matchJson("""{"a":{"w":1.23,"y":4.56}}""")
      }

      it("Enum as Map key and value must work") {
        val inst = MapHolder[Color, Color](Map(Color.Red -> Color.Blue, Color.Green -> Color.Red))
        val js = sj[MapHolder[Color, Color]].toJson(inst)
        js should matchJson("""{"a":{"Red":"Blue","Green":"Red"}}""")
      }
      it("Enum as Map key and value must work (using id)") {
        val inst = MapHolder[Color, Color](Map(Color.Red -> Color.Blue, Color.Green -> Color.Red))
        val js = sj[MapHolder[Color, Color]](JsonConfig.withEnumsAsIds(Some(Nil))).toJson(inst)
        js should matchJson("""{"a":{"0":2,"1":0}}""")
      }
      it("Enumeration as Map key and value must work") {
        import Permissions.*
        val inst = MapHolder[Permissions, Permissions](Map(Permissions.READ -> Permissions.WRITE, Permissions.EXEC -> Permissions.NONE))
        val js = sj[MapHolder[Permissions, Permissions]].toJson(inst)
        js should matchJson("""{"a":{"READ":"WRITE","EXEC":"NONE"}}""")
      }
      it("Enumeration as Map key and value must work (using id)") {
        import Permissions.*
        val inst = MapHolder[Permissions, Permissions](Map(Permissions.READ -> Permissions.WRITE, Permissions.EXEC -> Permissions.NONE))
        val js = sj[MapHolder[Permissions, Permissions]](JsonConfig.withEnumsAsIds(Some(Nil))).toJson(inst)
        js should matchJson("""{"a":{"0":1,"2":3}}""")
      }
      it("Java Enumeration as Map key and value must work") {
        val inst = MapHolder[CarEnum, CarEnum](Map(CarEnum.VW -> CarEnum.PORSCHE, CarEnum.PORSCHE -> CarEnum.TOYOTA))
        val js = sj[MapHolder[CarEnum, CarEnum]].toJson(inst)
        js should matchJson("""{"a":{"VW":"PORSCHE","PORSCHE":"TOYOTA"}}""")
      }
      it("Java Enumeration as Map key and value must work (using id)") {
        val inst = MapHolder[CarEnum, CarEnum](Map(CarEnum.VW -> CarEnum.PORSCHE, CarEnum.PORSCHE -> CarEnum.TOYOTA))
        val js = sj[MapHolder[CarEnum, CarEnum]](JsonConfig.withEnumsAsIds(Some(Nil))).toJson(inst)
        js should matchJson("""{"a":{"1":2,"2":0}}""")
      }
      it("Enum/Enumeration mix of enum as value must work") {
        import Permissions.*
        val inst = MapHolder[Color, Permissions](Map(Color.Red -> Permissions.WRITE, Color.Blue -> Permissions.NONE))
        val js = sj[MapHolder[Color, Permissions]](JsonConfig.withEnumsAsIds(Some(List("co.blocke.scalajack.json.collections.Color")))).toJson(inst)
        js should matchJson("""{"a":{"0":"WRITE","2":"NONE"}}""")
      }
    }
  }
