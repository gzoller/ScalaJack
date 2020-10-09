package co.blocke.scalajack
package json4s

import org.json4s._
import org.json4s.{ Diff, JNothing, JObject }
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack._
import co.blocke.scalajack.json4s._
import co.blocke.scala_reflection.RType

class Json4sSpec extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack(Json4sFlavor())

  test("Null Arrays work") {
    describe(
      "------------------\n:  Json4s Tests  :\n------------------", Console.BLUE
    )
    val inst: List[String] = null
    val js4s = sj.render(inst)
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(JNull) )
    assertEquals(inst, sj.read[List[String]](js4s))
  }

  test("Null Maps work") {
    val inst: Map[String, String] = null
    val js4s = sj.render(inst)
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(JNull) )
    assertEquals(inst, sj.read[Map[String, String]](js4s))

    val s: String = null
    val inst2 = Map("a" -> 1, s -> 2)
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Map keys cannot be null."){
      sj.render(inst2)
    }
  }

  test("Null strings work") {
    val inst: String = null
    val js4s = sj.render(inst)
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(JNull) )
    assertEquals(inst, sj.read[String](js4s))
  }

  test("Null objects work") {
    val inst: Player = null
    val js4s = sj.render(inst)
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(JNull) )
    assert(inst == sj.read[Player](js4s))
  }

  test("Tuples work") {
    val inst = List(("Fred", 34), ("Sally", 29))
    val js4s = sj.render(inst)
    val expected = JArray(
      List(
        JArray(List(JString("Fred"), JInt(34))),
        JArray(List(JString("Sally"), JInt(29)))
      )
    )
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[List[(String, Int)]](js4s))
  }

  test("Bad JValueBuilder access") {
    val b = JValueBuilder()
    interceptMessage[co.blocke.scalajack.ScalaJackError]("No value set for internal json4s builder"){
      b.result()
    }
  }
  test("SJCapture works") {
    val js4s = JObject(
      List(
        "name" -> JString("Harry"),
        "age" -> JInt(43),
        "foo" -> JBool(true),
        "bar" -> JInt(3)
      )
    )
    val inst = sj.read[PlayerCapture](js4s)
    assertEquals(PlayerCapture("Harry", 43), inst )
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(sj.render(inst)))
  }

  test("Trait support") {
    val inst: Thing[Int, String] = AThing(5, "foo")
    val js4s = sj.render(inst)
    val expected = JObject(
      List(
        "_hint" -> JString("co.blocke.scalajack.json4s.AThing"),
        "a" -> JInt(5),
        "b" -> JString("foo")
      )
    )
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[Thing[Int, String]](js4s))
  }

  test("Malformed error works") {
    val js4s = JArray(List(JInt(3), JDouble(3.1)))
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Cannot parse an Int from value"){
      sj.read[List[Int]](js4s)
    }
  }

  test("Unexpected error works") {
    val js4s = JArray(List(JInt(3), JDouble(3.1)))
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected object here, not 'JArray(List(JInt(3), JDouble(3.1)))'"){
      sj.read[Player](js4s)
    }
  }

  test("Hint mods work") {
    val prependHintMod = model.ClassNameHintModifier(
      (hint: String) => "co.blocke.scalajack.json4s." + hint,
      (cname: String) => cname.split('.').last
    )
    val sjx = sj.withHintModifiers((RType.of[Address], prependHintMod))
    val inst: Demographic =
      USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
    val js4s = sjx.render(inst)
    val expected = JObject(
      List(
        "_hint" -> JString("co.blocke.scalajack.json4s.USDemographic"),
        "age" -> JInt(50),
        "address" -> JObject(
          List(
            "_hint" -> JString("USAddress"),
            "street" -> JString("123 Main"),
            "city" -> JString("New York"),
            "state" -> JString("NY"),
            "postalCode" -> JString("39822")
          )
        )
      )
    )
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sjx.read[Demographic](js4s))
  }

  test("Broken hint mod (no class)") {
    val prependHintMod = model.ClassNameHintModifier(
      (hint: String) => "co.blocke.scalajack.bogus." + hint,
      (cname: String) => cname.split('.').last
    )
    val sjx = sj.withHintModifiers((RType.of[Address], prependHintMod))
    val js4s = JObject(
      List(
        "_hint" -> JString("co.blocke.scalajack.json4s.USDemographic"),
        "age" -> JInt(50),
        "address" -> JObject(
          List(
            "_hint" -> JString("BogusAddress"),
            "street" -> JString("123 Main"),
            "city" -> JString("New York"),
            "state" -> JString("NY"),
            "postalCode" -> JString("39822")
          )
        )
      )
    )
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Couldn't marshal class for BogusAddress"){
      sj.read[Demographic](js4s)
    }
  }

  test("Null object value") {
    val inst = USDemographic(25, null)
    val js4s = sj.render(inst)
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(JObject(List("age" -> JInt(25), "address" -> JNull))))
    assertEquals(inst, sj.read[USDemographic](js4s))
  }

  test("No type hint in trait") {
    val js4s = JObject(List("a" -> JInt(5), "b" -> JString("foo")))
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Type hint '_hint' not found"){
      sj.read[Thing[Int, String]](js4s)
    }
  }

  test("Any type that looks like trait but unknown hint") {
    val js4s = JObject(
      List("_hint" -> JInt(4), "name" -> JString("Fred"), "age" -> JInt(55))
    )
    assert(Map("_hint" -> 4, "name" -> "Fred", "age" -> 55) == sj.read[Any](js4s))
  }

  test("Non-scalars can't be map keys for Json4s") {
    val p = Player("Fred", 1)
    val m = Map(p -> 3)
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Json4s type org.json4s.JsonAST$JObject is not supported as a Map key"){
      sj.render(m)
    }
    val js4s = JObject(
      List(
        "name" -> JString("Harry"),
        "age" -> JInt(43),
        "foo" -> JBool(true),
        "bar" -> JInt(3)
      )
    )
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Only scalar values are supported as BSON Map keys"){
      sj.read[Map[Player, Int]](js4s)
    }
  }

  test("Externalized type hints work (with type modifier!)") {
    import model._
    val scalaJack = co.blocke.scalajack.ScalaJack(Json4sFlavor()).withTypeValueModifier(
      ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.json4s." + hint,
        (cname: String) => cname.split('.').last
      )
    )
    val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
    val d = scalaJack.render[Envelope[Body]](value)
    assertEquals(
      "JObject(List((Giraffe,JString(FancyBody)), (id,JString(DEF)), (body,JObject(List((message,JString(BOO)))))))",
      d.toString)
    assertEquals(scalaJack.read[Envelope[Body]](d), value)
  }

  test("Source as string") {
    val js4s = JObject(
      List(
        "name" -> JString("Harry"),
        "age" -> JInt(43),
        "foo" -> JBool(true),
        "bar" -> JInt(3)
      )
    )
    val p = sj.parse(js4s)
    assertEquals(p.sourceAsString,
      "JObject(List((name,JString(Harry)), (age,JInt(43)), (foo,JBool(true)), (bar,JInt(3))))"
    )
  }
