package co.blocke.scalajack
package json4s

import TestUtil._
import munit._
import munit.internal.console
import org.json4s._
import co.blocke.scalajack.json4s._
import co.blocke.scala_reflection.RType

class Custom extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack(Json4sFlavor())

  test("parse()") {
    describe(
      "---------------------------\n:  Custom Tests (Json4s) :\n---------------------------", Console.BLUE
    )
    val p = sj.parse(JInt(5))
    assertEquals(p.isInstanceOf[Json4sParser], true)
  }

  test("allowPermissivePrimitives()") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Permissive primitives not supported for Json4s"){
      sj.allowPermissivePrimitives()
    }
  }

  test("parseOrElse()") {
    val sj2 = sj.parseOrElse(RType.of[Address] -> RType.of[DefaultAddress])
    val js4s =
      JObject(
        List("_hint" -> JString("unknown"), "postalCode" -> JString("12345"))
      )
    assertEquals(sj2.read[Address](js4s), DefaultAddress("12345"))
  }

  test("withAdapters()") {
    val sj2 = sj.withAdapters(PhoneAdapter)
    val dbo = JObject(
      List("name" -> JString("Fred"), "phone" -> JString("123-456-7890"))
    )
    assertEquals(sj2.read[Employee](dbo), Employee("Fred", "1234567890".asInstanceOf[Phone]))
  }

  test("withDefaultHint()") {
    val sj2 = co.blocke.scalajack.ScalaJack(Json4sFlavor()).withDefaultHint("kind")
    val dbo = new JObject(
      List(
        "kind" -> JString("co.blocke.scalajack.json4s.USDemographic"),
        "age" -> JInt(34),
        "address" -> JObject(
          List(
            "kind" -> JString("co.blocke.scalajack.json4s.USAddress"),
            "street" -> JString("123 Main"),
            "city" -> JString("New York"),
            "state" -> JString("NY"),
            "postalCode" -> JString("39822")
          )
        )
      )
    )
    assertEquals(sj2.read[Demographic](dbo), USDemographic(34, USAddress("123 Main", "New York", "NY", "39822"))
    )
  }

  test("withHints()") {
    val sj2 = co.blocke.scalajack.ScalaJack(Json4sFlavor()).withHints(
      RType.of[Address] -> "addr_kind",
      RType.of[Demographic] -> "demo"
    )
    val dbo = new JObject(
      List(
        "demo" -> JString("co.blocke.scalajack.json4s.USDemographic"),
        "age" -> JInt(34),
        "address" -> JObject(
          List(
            "addr_kind" -> JString("co.blocke.scalajack.json4s.USAddress"),
            "street" -> JString("123 Main"),
            "city" -> JString("New York"),
            "state" -> JString("NY"),
            "postalCode" -> JString("39822")
          )
        )
      )
    )
    assertEquals(sj2.read[Demographic](dbo), USDemographic(34, USAddress("123 Main", "New York", "NY", "39822"))
    )
  }
  // Other custom configs are already covered in other tests elsewhere
