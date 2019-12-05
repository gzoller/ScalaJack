package co.blocke.scalajack
package json.misc

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

case class EmptyClass()

class Skipping extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe("--------------\n:  Skipping  :\n--------------") {

    val examples = Map(
      "string" -> """"haha"""",
      "number" -> """1""",
      "null" -> """null""",
      "boolean" -> """true""",
      "list of strings" -> """["a", "b", "c"]""",
      "list of objects" -> """[{"a":1},{"b":2},{"c":3}]"""
    )

    for ((description, json) <- examples) {
      it(s"Should skip a $description field value") {
        assertResult(EmptyClass()) {
          sj.read[EmptyClass](
            s"""{"fieldThatDoesNotExistAndThusRequiresSkippingThereforeQED":$json}"""
          )
        }
      }
    }

    it("Should skip") {
      val json =
        """
          |{
          |  "top": {
          |    "middle": [
          |      {
          |        "a": "1"
          |      },
          |      {
          |        "b": "2"
          |      },
          |      {
          |        "c": "3"
          |      }
          |    ]
          |  }
          |}
          |
        """.stripMargin

      sj.read[EmptyClass](json) should be(EmptyClass())
    }
    it("Skipping string with embedded special chars works") {
      val js =
        """{"name":"Fido \"The Beast\"","_hint":"co.blocke.scalajack.json.misc.Dog","kind":15}"""
      sj.read[Pet](js) should be(Dog("""Fido "The Beast"""", 15))
    }
    it("Skip over nested lists") {
      val js =
        """{"name":"Fido \"The Beast\"","_hint":"co.blocke.scalajack.json.misc.Dog","notneeded":[[1,2],[3,4]],"kind":15}"""
      sj.read[Pet](js) should be(Dog("""Fido "The Beast"""", 15))
    }
  }

}
