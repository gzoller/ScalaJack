package co.blocke.scalajack
package json4s

import org.json4s._
import org.json4s.{ Diff, JDecimal, JNothing, JObject }
import org.scalatest.{ FunSpec, Matchers }

class Json4sSpec extends FunSpec with Matchers {

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
      the[model.SJError] thrownBy sj.render(inst2) should have message "Map keys cannot be null."
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
      val expected = JArray(List(JArray(List(JString("Fred"), JInt(34))), JArray(List(JString("Sally"), JInt(29)))))
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sj.read[List[(String, Int)]](js4s)
      }
    }
    it("Bad JValueBuilder access") {
      val b = JValueBuilder()
      the[model.SJError] thrownBy b.result() should have message "No value set for internal json4s builder"
    }
    it("SJCapture works") {
      val js4s = JObject(List("name" -> JString("Harry"), "age" -> JInt(43), "foo" -> JBool(true), "bar" -> JInt(3)))
      val inst = sj.read[PlayerCapture](js4s)
      assertResult(PlayerCapture("Harry", 43)) { inst }
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(sj.render(inst)) }
    }
    it("Trait support") {
      val inst: Thing[Int, String] = AThing(5, "foo")
      val js4s = sj.render(inst)
      val expected = JObject(List("_hint" -> JString("co.blocke.scalajack.json4s.AThing"), "a" -> JInt(5), "b" -> JString("foo")))
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sj.read[Thing[Int, String]](js4s)
      }
    }
    it("Skip object works") {
      val inst = List(Player("Greg", 45), Player("Mary", 30))
      val r = sj.parse(sj.render(inst))
      r.next
      r.hasNext should be(true)
      r.skipObject(util.Path.Root)
      r.head.tokenType should be(model.TokenType.BeginObject)
      r.back
      r.back
      r.back
      r.back
      r.back
      r.back
      r.back
      r.back
      r.head.tokenType should be(model.TokenType.BeginArray)
      r.skipObject(util.Path.Root)
      r.reset
      r.head.tokenType should be(model.TokenType.BeginArray)
    }
    it("Malformed error works") {
      val js4s = JArray(List(JInt(3), JDouble(3.1)))
      the[model.ReadMalformedError] thrownBy sj.read[List[Int]](js4s) should have message "[$[1]]: Unable to read value (e.g. bad number format)"
    }
    it("Unexpected error works") {
      val js4s = JArray(List(JInt(3), JDouble(3.1)))
      the[model.ReadUnexpectedError] thrownBy sj.read[Player](js4s) should have message "[$]: Expected BeginObject here but found BeginArray"
    }
    it("Hint mods work") {
      val prependHintMod = model.ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json4s." + hint, (cname: String) => cname.split('.').last)
      val sjx = sj.withHintModifiers((typeOf[Address], prependHintMod))
      val inst: Demographic = USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
      val js4s = sjx.render(inst)
      val expected = JObject(List(
        "_hint" -> JString("co.blocke.scalajack.json4s.USDemographic"),
        "age" -> JInt(50),
        "address" -> JObject(List(
          "_hint" -> JString("USAddress"),
          "street" -> JString("123 Main"),
          "city" -> JString("New York"),
          "state" -> JString("NY"),
          "postalCode" -> JString("39822")
        ))
      ))
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sjx.read[Demographic](js4s)
      }
    }
    it("Broken hint mod (no class)") {
      val prependHintMod = model.ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json4s." + hint, (cname: String) => cname.split('.').last)
      val sjx = sj.withHintModifiers((typeOf[Address], prependHintMod))
      val js4s = JObject(List(
        "_hint" -> JString("co.blocke.scalajack.json4s.USDemographic"),
        "age" -> JInt(50),
        "address" -> JObject(List(
          "_hint" -> JString("BogusAddress"),
          "street" -> JString("123 Main"),
          "city" -> JString("New York"),
          "state" -> JString("NY"),
          "postalCode" -> JString("39822")
        ))
      ))
      the [model.ReadMissingError] thrownBy sj.read[Demographic](js4s) should have message "[$.address]: Unable to find class named \"BogusAddress\""
    }
  }
}
