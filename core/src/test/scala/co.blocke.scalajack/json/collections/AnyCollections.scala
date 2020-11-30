package co.blocke.scalajack
package json.collections

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import co.blocke.scala_reflection._

class AnyCollections() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("List works (Int)") {
    describe("--------------------------\n:  Any Collection Tests  :\n--------------------------", Console.BLUE)
    val inst: Any = List(1, 2, 3)
    val js = sj.render(inst)
    assertEquals("""[1,2,3]""".asInstanceOf[JSON], js)
    assert(List(1, 2, 3) == sj.read[Any](js))
  }

  test("List works (String)") {
    val inst: Any = List("one", "two", "three")
    val js = sj.render(inst)
    assertEquals("""["one","two","three"]""".asInstanceOf[JSON], js)
    assert(List("one", "two", "three") == sj.read[Any](js))
  }

  test("First-Level List works (Class)") {
    val inst: Any = List(Player("Mike", 34), Player("Sarah", 29))
    val js = sj.render(inst)
    assertEquals(
      """[{"_hint":"co.blocke.scalajack.json.collections.Player","name":"Mike","age":34},{"_hint":"co.blocke.scalajack.json.collections.Player","name":"Sarah","age":29}]""".asInstanceOf[JSON], js)
      assert(List(Player("Mike", 34), Player("Sarah", 29)) == sj.read[List[Any]](js))
  }

  test("Map works (Int,Int)") {
    val inst: Any = Map(1 -> 2, 3 -> 4)
    val js = sj.render(inst)
    assertEquals("""{"1":2,"3":4}""".asInstanceOf[JSON], js)
    assertEquals(inst,sj.read[Any](js))
  }

  test("Map works (String,Int)") {
    val inst: Any = Map("yes" -> 1, "no" -> 2)
    val js = sj.render(inst)
    assertEquals("""{"yes":1,"no":2}""".asInstanceOf[JSON], js)
    assert(Map("yes" -> 1, "no" -> 2) == sj.read[Any](js))
  }

  test("First-Level Map works (Class)") {
    val js = """{"_hint":"co.blocke.scalajack.json.collections.Player","name":"Mike","age":34}""".asInstanceOf[JSON]
    assert(Player("Mike", 34) == sj.read[Any](js))
  }

  test("Second-Level Map works (Class keys) (Class)") {
    val js =
      """{"{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34}":15, "{\"name\":\"Mike\",\"age\":34}":16}""".asInstanceOf[JSON]
    assert(Map(Player("Mike", 34) -> 15, Map("name" -> "Mike", "age" -> 34) -> 16) == sj.read[Any](js))
  }

  test("Second-Level Map (List keys) works (Class)") {
    val js =
      """{"[{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34},{\"name\":\"Mike\",\"age\":34}]":15}""".asInstanceOf[JSON]
    assert(Map(List(Player("Mike", 34), Map("name" -> "Mike", "age" -> 34)) -> 15) == sj.read[Any](js))
  }
