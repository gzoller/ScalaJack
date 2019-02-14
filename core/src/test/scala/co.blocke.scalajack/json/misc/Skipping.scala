package co.blocke.scalajack
package json.misc

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }

case class EmptyClass()

class Skipping extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-----------------------------\n:  Skipping  :\n-----------------------------") {

    val examples = Map(
      "string" -> """"haha"""",
      "number" -> """1""",
      "null" -> """null""",
      "boolean" -> """true""",
      "list of strings" -> """["a", "b", "c"]""",
      "list of objects" -> """[{"a":1},{"b":2},{"c":3}]""")

    for ((description, json) <- examples) {
      it(s"Should skip a $description field value") {
        assertResult(EmptyClass()) {
          sj.read[EmptyClass](s"""{"fieldThatDoesNotExistAndThusRequiresSkippingThereforeQED":$json}""")
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

      sj.read[EmptyClass](json)
    }
  }

}
