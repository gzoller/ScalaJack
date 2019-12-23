package co.blocke.scalajack
package yaml
package collections

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

case class Player(name: String, age: Int)

class AnyColl() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "---------------------------------\n:  Any Collection Tests (YAML)  :\n---------------------------------"
  ) {
    it("List works (Int)") {
      val inst: Any  = List(1, 2, 3)
      val yaml       = sj.render(inst)
      val comparison = """- 1
                         |- 2
                         |- 3
                         |""".stripMargin
      assertResult(comparison) { yaml }
      assertResult(List(1, 2, 3)) {
        sj.read[Any](yaml)
      }
    }
    it("List works (String)") {
      val inst: Any  = List("one", "two", "three")
      val yaml       = sj.render(inst)
      val comparison = """- one
                         |- two
                         |- three
                         |""".stripMargin
      assertResult(comparison) { yaml }
      assertResult(List("one", "two", "three")) {
        sj.read[Any](yaml)
      }
    }
    it("First-Level List works (Class)") {
      val inst: Any  = List(Player("Mike", 34), Player("Sarah", 29))
      val yaml       = sj.render(inst)
      val comparison = """- _hint: co.blocke.scalajack.yaml.collections.Player
                         |  name: Mike
                         |  age: 34
                         |- _hint: co.blocke.scalajack.yaml.collections.Player
                         |  name: Sarah
                         |  age: 29
                         |""".stripMargin
      assertResult(comparison) { yaml }
      assertResult(List(Player("Mike", 34), Player("Sarah", 29))) {
        sj.read[List[Any]](yaml)
      }
    }
    it("Map works (Int,Int)") {
      val inst: Any  = Map(1 -> 2, 3 -> 4)
      val yaml       = sj.render(inst)
      val comparison = """1: 2
                         |3: 4
                         |""".stripMargin
      assertResult(comparison) { yaml }
      assertResult(inst) {
        sj.read[Any](yaml)
      }
    }
    it("Map works (String,Int)") {
      val inst: Any  = Map("yes" -> 1, "no" -> 2)
      val yaml       = sj.render(inst)
      val comparison = """yes: 1
                         |no: 2
                         |""".stripMargin
      assertResult(comparison) { yaml }
      assertResult(Map("yes" -> 1, "no" -> 2)) {
        sj.read[Any](yaml)
      }
    }
    it("First-Level Map works (Class)") {
      val yaml =
        """_hint: co.blocke.scalajack.yaml.collections.Player
          |name: Mike
          |age: 34
          |""".stripMargin
      assertResult(Player("Mike", 34)) {
        sj.read[Any](yaml)
      }
    }
    it("Second-Level Map works (Class keys) (Class)") {
      val yaml =
        """?
          |  _hint: co.blocke.scalajack.yaml.collections.Player
          |  name: Mike
          |  age: 34
          |: 15
          |?
          |  name: Mike
          |  age: 34
          |: 16
          |""".stripMargin
      assertResult(
        Map(Player("Mike", 34) -> 15, Map("name" -> "Mike", "age" -> 34) -> 16)
      ) {
        sj.read[Any](yaml)
      }
    }
    it("Second-Level Map (List keys) works (Class)") {
      val yaml =
        """?
          |  -
          |    _hint: co.blocke.scalajack.yaml.collections.Player
          |    name: Mike
          |    age: 34
          |  -
          |    name: Mike
          |    age: 34
          |: 15
          |""".stripMargin
      assertResult(
        Map(List(Player("Mike", 34), Map("name" -> "Mike", "age" -> 34)) -> 15)
      ) {
        sj.read[Any](yaml)
      }
    }
  }
}
