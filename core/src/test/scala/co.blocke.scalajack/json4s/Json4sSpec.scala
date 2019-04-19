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
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(js4s) }
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
  }
}
