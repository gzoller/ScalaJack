package co.blocke.scalajack
package json4s

import org.json4s._
import org.json4s.{ Diff, JDecimal, JNothing, JObject }
import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec

import scala.math.BigDecimal

class AnyColl() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(Json4sFlavor())

  describe("-----------------------------------\n:  Any Collection Tests (Json4s)  :\n-----------------------------------") {
    it("List works (Int)") {
      val inst: Any = List(1, 2L, 3.2, BigDecimal(123.45))
      val js4s = sj.render(inst)
      val expected = JArray(List(JInt(1), JLong(2), JDouble(3.2), JDecimal(123.45)))
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sj.read[Any](js4s)
      }
    }
    it("First-Level List works (Class)") {
      val inst: Any = List(Player("Mike", 34), Player("Sarah", 29))
      val js4s = sj.render(inst)
      val expected = JArray(List(JObject(List("_hint" -> JString("co.blocke.scalajack.json4s.Player"), "name" -> JString("Mike"), "age" -> JInt(34))), JObject(List("_hint" -> JString("co.blocke.scalajack.json4s.Player"), "name" -> JString("Sarah"), "age" -> JInt(29)))))
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(List(Player("Mike", 34), Player("Sarah", 29))) {
        sj.read[List[Any]](js4s)
      }
    }
    it("Map works (Int,Int)") {
      val inst: Any = Map(1 -> 2, 3 -> 4)
      val js4s = sj.render(inst)
      val expected = JObject(List("1" -> JInt(2), "3" -> JInt(4)))
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sj.read[Any](js4s)
      }
    }
    /*
    it("Map works (String,Int)") {
      val inst: Any = Map("yes" -> 1, "no" -> 2)
      val js = sj.render(inst)
      assertResult("""{"yes":1,"no":2}""") { js }
      assertResult(Map("yes" -> 1, "no" -> 2)) {
        sj.read[Any](js)
      }
    }
    it("First-Level Map works (Class)") {
      val js = """{"_hint":"co.blocke.scalajack.json.collections.Player","name":"Mike","age":34}"""
      assertResult(Player("Mike", 34)) {
        sj.read[Any](js)
      }
    }
    it("Second-Level Map works (Class keys) (Class)") {
      val js = """{"{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34}":15, "{\"name\":\"Mike\",\"age\":34}":16}"""
      assertResult(Map(Player("Mike", 34) -> 15, Map("name" -> "Mike", "age" -> 34) -> 16)) {
        sj.read[Any](js)
      }
    }
    it("Second-Level Map (List keys) works (Class)") {
      val js = """{"[{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34},{\"name\":\"Mike\",\"age\":34}]":15}"""
      assertResult(Map(List(Player("Mike", 34), Map("name" -> "Mike", "age" -> 34)) -> 15)) {
        sj.read[Any](js)
      }
    }

     */
  }
}
