package co.blocke.scalajack
package json4s

import org.json4s._
import org.json4s.{ Diff, JNothing, JObject }
import org.scalatest.matchers.should._
import org.scalatest.funspec.AnyFunSpec

import scala.reflect.runtime.universe._

class Json4sSpec extends AnyFunSpec with Matchers {

  val sj = ScalaJack(Json4sFlavor())

  describe("------------------\n:  Json4s Tests  :\n------------------") {
    it("Null Arrays work") {
      val inst: List[String] = null
      val js4s = sj.render(inst)
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(JNull) }
      assertResult(inst) {
        sj.read[List[String]](js4s)
      }
    }
    it("Null Maps work") {
      val inst: Map[String, String] = null
      val js4s = sj.render(inst)
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(JNull) }
      assertResult(inst) {
        sj.read[Map[String, String]](js4s)
      }

      val s: String = null
      val inst2 = Map("a" -> 1, s -> 2)
      the[ScalaJackError] thrownBy sj.render(inst2) should have message "Map keys cannot be null."
    }
    it("Null strings work") {
      val inst: String = null
      val js4s = sj.render(inst)
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(JNull) }
      assertResult(inst) {
        sj.read[String](js4s)
      }
    }
    it("Null objects work") {
      val inst: Player = null
      val js4s = sj.render(inst)
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(JNull) }
      assertResult(inst) {
        sj.read[String](js4s)
      }
    }
    it("Tuples work") {
      val inst = List(("Fred", 34), ("Sally", 29))
      val js4s = sj.render(inst)
      val expected = JArray(
        List(
          JArray(List(JString("Fred"), JInt(34))),
          JArray(List(JString("Sally"), JInt(29)))
        )
      )
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sj.read[List[(String, Int)]](js4s)
      }
    }
    it("Bad JValueBuilder access") {
      val b = JValueBuilder()
      the[ScalaJackError] thrownBy b
        .result() should have message "No value set for internal json4s builder"
    }
    it("SJCapture works") {
      val js4s = JObject(
        List(
          "name" -> JString("Harry"),
          "age" -> JInt(43),
          "foo" -> JBool(true),
          "bar" -> JInt(3)
        )
      )
      val inst = sj.read[PlayerCapture](js4s)
      assertResult(PlayerCapture("Harry", 43)) { inst }
      assertResult(Diff(JNothing, JNothing, JNothing)) {
        js4s.diff(sj.render(inst))
      }
    }
    it("Trait support") {
      val inst: Thing[Int, String] = AThing(5, "foo")
      val js4s = sj.render(inst)
      val expected = JObject(
        List(
          "_hint" -> JString("co.blocke.scalajack.json4s.AThing"),
          "a" -> JInt(5),
          "b" -> JString("foo")
        )
      )
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sj.read[Thing[Int, String]](js4s)
      }
    }
    it("Malformed error works") {
      val js4s = JArray(List(JInt(3), JDouble(3.1)))
      the[ScalaJackError] thrownBy sj.read[List[Int]](js4s) should have message "Cannot parse an Int from value"
    }
    it("Unexpected error works") {
      val js4s = JArray(List(JInt(3), JDouble(3.1)))
      the[ScalaJackError] thrownBy sj.read[Player](js4s) should have message "Expected object here, not 'JArray(List(JInt(3), JDouble(3.1)))'"
    }
    it("Hint mods work") {
      val prependHintMod = model.ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.json4s." + hint,
        (cname: String) => cname.split('.').last
      )
      val sjx = sj.withHintModifiers((typeOf[Address], prependHintMod))
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
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sjx.read[Demographic](js4s)
      }
    }
    it("Broken hint mod (no class)") {
      val prependHintMod = model.ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.bogus." + hint,
        (cname: String) => cname.split('.').last
      )
      val sjx = sj.withHintModifiers((typeOf[Address], prependHintMod))
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
      the[ScalaJackError] thrownBy sjx.read[Demographic](js4s) should have message "Couldn't marshal class for BogusAddress"
    }
    it("Null object value") {
      val inst = USDemographic(25, null)
      val js4s = sj.render(inst)
      assertResult(Diff(JNothing, JNothing, JNothing)) {
        js4s.diff(JObject(List("age" -> JInt(25), "address" -> JNull)))
      }
      assertResult(inst) {
        sj.read[USDemographic](js4s)
      }
    }
    it("No type hint in trait") {
      val js4s = JObject(List("a" -> JInt(5), "b" -> JString("foo")))
      the[ScalaJackError] thrownBy sj.read[Thing[Int, String]](js4s) should have message "Type hint '_hint' not found"
    }
    it("Any type that looks like trait but unknown hint") {
      val js4s = JObject(
        List("_hint" -> JInt(4), "name" -> JString("Fred"), "age" -> JInt(55))
      )
      assertResult(Map("_hint" -> 4, "name" -> "Fred", "age" -> 55)) {
        sj.read[Any](js4s)
      }
    }
    it("Non-scalars can't be map keys for Json4s") {
      val p = Player("Fred", 1)
      val m = Map(p -> 3)
      the[ScalaJackError] thrownBy sj.render(m) should have message "Json4s type org.json4s.JsonAST$JObject is not supported as a Map key"
      val js4s = JObject(
        List(
          "name" -> JString("Harry"),
          "age" -> JInt(43),
          "foo" -> JBool(true),
          "bar" -> JInt(3)
        )
      )
      the[ScalaJackError] thrownBy sj.read[Map[Player, Int]](js4s) should have message "Only scalar values are supported as BSON Map keys"
    }
    it("Externalized type hints work (with type modifier!)") {
      import model._
      val scalaJack = ScalaJack(Json4sFlavor()).withTypeValueModifier(
        ClassNameHintModifier(
          (hint: String) => "co.blocke.scalajack.json4s." + hint,
          (cname: String) => cname.split('.').last
        )
      )
      val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
      val d = scalaJack.render[Envelope[Body]](value)
      assertResult(
        "JObject(List((Giraffe,JString(Body)), (id,JString(DEF)), (body,JObject(List((_hint,JString(co.blocke.scalajack.json4s.FancyBody)), (message,JString(BOO)))))))"
      ) {
          d.toString
        }
      scalaJack.read[Envelope[Body]](d) should be(value)
    }
    it("Source as string") {
      val js4s = JObject(
        List(
          "name" -> JString("Harry"),
          "age" -> JInt(43),
          "foo" -> JBool(true),
          "bar" -> JInt(3)
        )
      )
      val p = sj.parse(js4s)
      p.sourceAsString should be(
        "JObject(List((name,JString(Harry)), (age,JInt(43)), (foo,JBool(true)), (bar,JInt(3))))"
      )
    }
  }
  describe("forType functionality") {
    it("Basic forType") {
      val sj2 = sj.forType[Employee]
      val five = Employee("Fred", "555-555-5555")
      val dbo = sj2.render(five)
      dbo.toString should be(
        "JObject(List((name,JString(Fred)), (phone,JString(555-555-5555))))"
      )
      sj2.read(dbo) should equal(five)
    }
    it("forType after a forType") {
      val sj2 = sj.forType[Employee].forType[SomeClass]
      val six = SomeClass("Fred", 24)
      val dbo = sj2.render(six)
      dbo.toString should be(
        "JObject(List((name,JString(Fred)), (age,JInt(24))))"
      )
      sj2.read(dbo) should equal(six)
    }
  }
}
