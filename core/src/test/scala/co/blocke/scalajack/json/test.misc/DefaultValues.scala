package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._

class DefaultValues extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-------------------------\n:  Default Value Tests  :\n-------------------------") {
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
      assertResult("""{"name":"Me","pet":{"_hint":"co.blocke.scalajack.json.test.misc.Dog","name":"Fido","kind":true}}""") { js }
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
    it("Force an optional default value to resolve None using null") {
      val js = """{"name": "Harry", "age":null}"""
      assertResult(DefaultOpt("Harry", None)) {
        sj.read[DefaultOpt](js)
      }
    }
    it("Fails if no default is found for a given field") {
      val js = """{"age":null}"""
      val msg = """DeserializationException(1 error):
                  |  [$.name] Required field missing (reported by: co.blocke.scalajack.typeadapter.StringDeserializer)""".stripMargin
      the[DeserializationException] thrownBy sj.read[DefaultOpt](js) should have message msg
    }
  }
}
