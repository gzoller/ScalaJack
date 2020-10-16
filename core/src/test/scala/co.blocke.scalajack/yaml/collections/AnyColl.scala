package co.blocke.scalajack
package yaml
package collections

import TestUtil._
import munit._
import munit.internal.console

case class Player(name: String, age: Int)

class AnyColl() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("List works (Int)") {
    describe(
      "---------------------------------\n:  Any Collection Tests (YAML)  :\n---------------------------------", Console.BLUE
    )
    val inst: Any  = List(1, 2, 3)
    val yaml       = sj.render(inst)
    val comparison = """- 1
                        |- 2
                        |- 3
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assert(List(1, 2, 3) == sj.read[Any](yaml))
  }

  test("List works (String)") {
    val inst: Any  = List("one", "two", "three")
    val yaml       = sj.render(inst)
    val comparison = """- one
                        |- two
                        |- three
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml)
    assert(List("one", "two", "three") == sj.read[Any](yaml))
  }

  test("First-Level List works (Class)") {
    val inst: Any  = List(Player("Mike", 34), Player("Sarah", 29))
    val yaml       = sj.render(inst)
    val comparison = """- _hint: co.blocke.scalajack.yaml.collections.Player
                        |  name: Mike
                        |  age: 34
                        |- _hint: co.blocke.scalajack.yaml.collections.Player
                        |  name: Sarah
                        |  age: 29
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assert(List(Player("Mike", 34), Player("Sarah", 29)) == sj.read[List[Any]](yaml))
  }

  test("Map works (Int,Int)") {
    val inst: Any  = Map(1 -> 2, 3 -> 4)
    val yaml       = sj.render(inst)
    val comparison = """1: 2
                        |3: 4
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Any](yaml))
  }

  test("Map works (String,Int)") {
    val inst: Any  = Map("yes" -> 1, "no" -> 2)
    val yaml       = sj.render(inst)
    val comparison = """yes: 1
                        |no: 2
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assert(Map("yes" -> 1, "no" -> 2) == sj.read[Any](yaml))
  }

  test("First-Level Map works (Class)") {
    val yaml =
      """_hint: co.blocke.scalajack.yaml.collections.Player
        |name: Mike
        |age: 34
        |""".stripMargin.asInstanceOf[YAML]
    assert(Player("Mike", 34) == sj.read[Any](yaml))
  }

  test("Second-Level Map works (Class keys) (Class)") {
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
        |""".stripMargin.asInstanceOf[YAML]
    assert(
      Map(Player("Mike", 34) -> 15, Map("name" -> "Mike", "age" -> 34) -> 16) == sj.read[Any](yaml)
    )
  }

  test("Second-Level Map (List keys) works (Class)") {
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
        |""".stripMargin.asInstanceOf[YAML]
    assert(
      Map(List(Player("Mike", 34), Map("name" -> "Mike", "age" -> 34)) -> 15) == sj.read[Any](yaml)
    )
  }
