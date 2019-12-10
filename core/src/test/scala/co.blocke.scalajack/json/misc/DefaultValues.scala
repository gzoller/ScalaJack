package co.blocke.scalajack
package json.misc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DefaultValues extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "-------------------------\n:  Default Value Tests  :\n-------------------------"
  ) {
      it("Default values are found - not confused by optional values") {
        SimpleHasDefaults
        val inst = SimpleHasDefaults("Me")
        val js = sj.render(inst)
        assertResult("""{"name":"Me","age":5}""") { js }
        assertResult(inst) {
          sj.read[SimpleHasDefaults](js)
        }
        val missing = """{"name":"Me"}"""
        assertResult(inst) {
          sj.read[SimpleHasDefaults](missing)
        }
      }
      it("Traits with default values handled") {
        val inst = HasDefaults("Me", None)
        val js = sj.render(inst)
        assertResult(
          """{"name":"Me","pet":{"_hint":"co.blocke.scalajack.json.misc.Dog","name":"Fido","kind":true}}"""
        ) { js }
        assertResult(inst) {
          sj.read[HasDefaults](js)
        }
        val missing = """{"name":"Me"}"""
        assertResult(inst) {
          sj.read[HasDefaults](missing)
        }
      }
      it("Marshals default optional value (before assuming None)") {
        val js = """{"name": "Harry"}"""
        assertResult(DefaultOpt("Harry")) {
          sj.read[DefaultOpt](js)
        }
      }
      it("Optional default value and null") {
        val js = """{"name": "Harry", "age":null}"""
        val js2 = """{"name": "Harry"}"""
        assertResult(DefaultOpt("Harry", null)) {
          sj.read[DefaultOpt](js)
        }
        assertResult(DefaultOpt("Harry", Some(19))) {
          sj.read[DefaultOpt](js2)
        }
      }
      it("Fails if no default is found for a given field") {
        val js = """{"age":null}"""
        val msg = """Class DefaultOpt missing required fields: name
                  |{"age":null}
                  |-----------^""".stripMargin
        the[ScalaJackError] thrownBy sj.read[DefaultOpt](js) should have message msg
      }
    }
}
