package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID

class DefaultValues extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-------------------------\n:  Default Value Tests  :\n-------------------------") {
    it("Default values are found - not confused by optional values") {
      val inst = HasDefaults("Me", None)
      val js = sj.render(inst)
      assertResult("""{"name": "Me", "pet": {"_hint": "co.blocke.scalajack.json.test.misc.Dog", "name": "Fido", "kind": true}}""") { js }
      assertResult(inst) {
        sj.read[HasDefaults](js)
      }
    }
    it("Marshals default optional value (before asuming None)") {
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
      val msg = """Required field name in class co.blocke.scalajack.json.test.misc.DefaultOpt is missing from input and has no specified default value
        |{"age":null}
        |-----------^""".stripMargin
      the[java.lang.IllegalStateException] thrownBy sj.read[DefaultOpt](js) should have message msg
    }
  }
}
