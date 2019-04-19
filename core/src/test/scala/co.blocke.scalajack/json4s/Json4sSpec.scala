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
    it("Tuples work") {
      val inst = List(("Fred", 34), ("Sally", 29))
      val js4s = sj.render(inst)
      val expected = JArray(List(JArray(List(JString("Fred"), JInt(34))), JArray(List(JString("Sally"), JInt(29)))))
      assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
      assertResult(inst) {
        sj.read[List[(String, Int)]](js4s)
      }
    }
    // TODO: Test embeddit objects to be sure Maps/Objects arent flattened by ++!
  }
}
