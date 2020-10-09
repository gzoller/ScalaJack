package co.blocke.scalajack
package json4s

import org.json4s._
import org.json4s.{ Diff, JDecimal, JNothing, JObject }
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json4s.Json4sFlavor

import scala.math.BigDecimal

class AnyColl() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack(Json4sFlavor())

  test("List works (Int)") {
    describe(
      "-----------------------------------\n:  Any Collection Tests (Json4s)  :\n-----------------------------------", Console.BLUE
    )
    val inst: Any = List(1, 2L, 3.2, BigDecimal(123.45))
    val js4s = sj.render(inst)
    val expected = JArray(List(JInt(1), JLong(2), JDouble(3.2), JDecimal(123.45)))
    assertEquals(Diff(JNothing, JNothing, JNothing), js4s.diff(expected))
    assertEquals(inst, sj.read[Any](js4s))
  }

  test("First-Level List works (Class)") {
    val inst: Any = List(Player("Mike", 34), Player("Sarah", 29))
    val js4s = sj.render(inst)
    val expected = JArray(
      List(
        JObject(
          List(
            "_hint" -> JString("co.blocke.scalajack.json4s.Player"),
            "name" -> JString("Mike"),
            "age" -> JInt(34)
          )
        ),
        JObject(
          List(
            "_hint" -> JString("co.blocke.scalajack.json4s.Player"),
            "name" -> JString("Sarah"),
            "age" -> JInt(29)
          )
        )
      )
    )
    assertEquals(Diff(JNothing, JNothing, JNothing), js4s.diff(expected))
    assert(List(Player("Mike", 34), Player("Sarah", 29)) == sj.read[List[Any]](js4s))
  }

  test("Map works (Int,Int)") {
    val inst: Any = Map(1 -> 2, 3 -> 4)
    val js4s = sj.render(inst)
    val expected = JObject(List("1" -> JInt(2), "3" -> JInt(4)))
    assertEquals(Diff(JNothing, JNothing, JNothing), js4s.diff(expected))
    assert(Map("1" -> 2, "3" -> 4) ==
      sj.read[Any](js4s)) // May keys converted to String when read back in (because they're Any)
  }

  test("Map works (String,Int)") {
    val inst: Any = Map("yes" -> 1, "no" -> 2)
    val js4s = sj.render(inst)
    val expected = JObject(List("yes" -> JInt(1), "no" -> JInt(2)))
    assertEquals(Diff(JNothing, JNothing, JNothing), js4s.diff(expected))
    assertEquals(inst, sj.read[Any](js4s))
  }

  test("First-Level Map works (Class)") {
    val js4s = JObject(
      "_hint" -> JString("co.blocke.scalajack.json4s.Player"),
      "name" -> JString("Mike"),
      "age" -> JInt(34)
    )
    assert(Player("Mike", 34) == sj.read[Any](js4s))
  }

  /* These last 2 tests have no meaning in Json4s, as it requires a String for Object (Map) key.  Since Json4s supplies
     no canonical way to stringify a class then having a non-scalar as a Map key has no representational meaning.

  test("Second-Level Map works (Class keys) (Class)") {
    val m =
      Map(Player("Mike", 34) -> 15, Map("name" -> "Mike", "age" -> 34) -> 16)
    //    val js = """{"{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34}":15, "{\"name\":\"Mike\",\"age\":34}":16}"""
    //    assertResult(Map(Player("Mike", 34) -> 15, Map("name" -> "Mike", "age" -> 34) -> 16)) {
    //      sj.read[Any](js)
    //    }
  }
  test("Second-Level Map (List keys) works (Class)") {
    val js = """{"[{\"_hint\":\"co.blocke.scalajack.json.collections.Player\",\"name\":\"Mike\",\"age\":34},{\"name\":\"Mike\",\"age\":34}]":15}"""
    assertResult(Map(List(Player("Mike", 34), Map("name" -> "Mike", "age" -> 34)) -> 15)) {
      sj.read[Any](js)
    }
  }
 */
