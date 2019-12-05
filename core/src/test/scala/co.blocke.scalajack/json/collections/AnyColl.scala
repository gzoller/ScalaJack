package co.blocke.scalajack
package json.collections

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec

class AnyColl() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "--------------------------\n:  Any Collection Tests  :\n--------------------------"
  ) {
      it("List works (Int)") {
        val inst: Any = List(1, 2, 3)
        val js = sj.render(inst)
        assertResult("""[1,2,3]""") { js }
        assertResult(List(1, 2, 3)) {
          sj.read[Any](js)
        }
      }
      it("List works (String)") {
        val inst: Any = List("one", "two", "three")
        val js = sj.render(inst)
        assertResult("""["one","two","three"]""") { js }
        assertResult(List("one", "two", "three")) {
          sj.read[Any](js)
        }
      }
      it("First-Level List works (Class)") {
        val inst: Any = List(Player("Mike", 34), Player("Sarah", 29))
        val js = sj.render(inst)
        assertResult(
          """[{"_hint":"co.blocke.scalajack.json.collections.Player","name":"Mike","age":34},{"_hint":"co.blocke.scalajack.json.collections.Player","name":"Sarah","age":29}]"""
        ) {
            js
          }
        assertResult(List(Player("Mike", 34), Player("Sarah", 29))) {
          sj.read[List[Any]](js)
        }
      }
      it("Map works (Int,Int)") {
        val inst: Any = Map(1 -> 2, 3 -> 4)
        val js = sj.render(inst)
        assertResult("""{"1":2,"3":4}""") { js }
        assertResult(inst) {
          sj.read[Any](js)
        }
      }
      it("Map works (String,Int)") {
        val inst: Any = Map("yes" -> 1, "no" -> 2)
        val js = sj.render(inst)
        assertResult("""{"yes":1,"no":2}""") { js }
        assertResult(Map("yes" -> 1, "no" -> 2)) {
          sj.read[Any](js)
        }
      }
      it("First-Level Map works (Class)") {
        val js =
          """{"_hint":"co.blocke.scalajack.json.collections.Player","name":"Mike","age":34}"""
        assertResult(Player("Mike", 34)) {
          sj.read[Any](js)
        }
      }
      it("Second-Level Map works (Class keys) (Class)") {
        val js =
          """{"{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34}":15, "{\"name\":\"Mike\",\"age\":34}":16}"""
        assertResult(
          Map(Player("Mike", 34) -> 15, Map("name" -> "Mike", "age" -> 34) -> 16)
        ) {
            sj.read[Any](js)
          }
      }
      it("Second-Level Map (List keys) works (Class)") {
        val js =
          """{"[{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34},{\"name\":\"Mike\",\"age\":34}]":15}"""
        assertResult(
          Map(List(Player("Mike", 34), Map("name" -> "Mike", "age" -> 34)) -> 15)
        ) {
            sj.read[Any](js)
          }
      }
    }
}
