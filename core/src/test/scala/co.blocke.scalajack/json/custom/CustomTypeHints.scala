package co.blocke.scalajack
package json.custom

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec
import scala.reflect.runtime.universe.typeOf
import model._

class CustomTypeHints() extends AnyFunSpec with Matchers {

  describe("-----------------------------\n:  Custom Type Hints Tests  :\n-----------------------------") {
    describe("+++ Positive Tests +++") {
      it("Override default trait/polymorphic type hint") {
        val sj = ScalaJack().withDefaultHint("which")
        val inst: Address = USAddress("123 Main", "New York", "NY", "39822")
        val js = sj.render(inst)
        assertResult("""{"which":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}""") { js }
        assertResult(inst) {
          sj.read[Address](js)
        }
      }
      it("Override type-specific trait/polymorphic type hint") {
        val sj = ScalaJack().withHints((typeOf[Address] -> "addr_kind"))
        val inst: Demographic = USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
        val js = sj.render(inst)
        assertResult("""{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"addr_kind":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""") { js }
        assertResult(inst) {
          sj.read[Demographic](js)
        }
      }
      it("Use ClassNameHintModifier to modify trait/polymorphic type hint value") {
        val prependHintMod = ClassNameHintModifier((hint: String) => "co.blocke.scalajack.json.custom." + hint, (cname: String) => cname.split('.').last)
        val sj = ScalaJack().withHintModifiers((typeOf[Address], prependHintMod))
        val inst: Demographic = USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
        val js = sj.render(inst)
        assertResult("""{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""") { js }
        assertResult(inst) {
          sj.read[Demographic](js)
        }
      }
      it("Use StringMatchHintModifier to modify trait/polymorphic type hint value") {
        val strMatchHintMod = StringMatchHintModifier(Map("US" -> typeOf[USAddress]))
        val sj = ScalaJack().withHintModifiers((typeOf[Address], strMatchHintMod))
        val inst: Demographic = USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
        val js = sj.render(inst)
        assertResult("""{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"US","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""") { js }
        assertResult(inst) {
          sj.read[Demographic](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Use unspecified type hint") {
        val sj = ScalaJack().withDefaultHint("which")
        val js = """{"bogus":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}"""
        val msg = """[$.which]: Couldn't find expected type hint 'which' for trait co.blocke.scalajack.json.custom.Address
                    |...ity":"New York","state":"NY","postalCode":"39822"}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[Address](js) should have message msg
      }
      it("Ignore type-specific type hint (e.g. use default) when a specific hint is specified") {
        val sj = ScalaJack().withDefaultHint("which")
        val js = """{"which":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
        val msg = """[$.address.which]: Couldn't find expected type hint 'which' for trait co.blocke.scalajack.json.custom.Address
                    |...ity":"New York","state":"NY","postalCode":"39822"}}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[Address](js) should have message msg
      }
      it("Ignore type-specific type hint (e.g. use default) when a specific hint is specified -- newline test") {
        val sj = ScalaJack().withDefaultHint("which")
        val js = """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New
        |York","state":"NY","postalCode":"39822"}}""".stripMargin
        val msg = """[$.which]: Couldn't find expected type hint 'which' for trait co.blocke.scalajack.json.custom.Address
                    |...ty":"New~York","state":"NY","postalCode":"39822"}}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[Address](js) should have message msg
      }
      it("Hint value after modification doesn't resolve to known class name") {
        val prependHintMod = ClassNameHintModifier((hint: String) => "co.blocke.scalajack.bogus." + hint, (cname: String) => cname.split('.').last)
        val sj = ScalaJack().withHintModifiers((typeOf[Address], prependHintMod))
        val js = """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
        val msg = """[$.address]: Failed to apply type modifier to type member hint USAddress
                    |...emographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","cit...
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[Demographic](js) should have message msg
      }
      it("Unknown string given as type hint value (no cooresponding match to class in mapping)") {
        val strMatchHintMod = StringMatchHintModifier(Map("US" -> typeOf[USAddress]))
        val sj = ScalaJack().withHintModifiers((typeOf[Address], strMatchHintMod))
        val js = """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"Bogus","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
        val msg = """[$.address]: Failed to apply type modifier to type member hint Bogus
                    |....USDemographic","age":50,"address":{"_hint":"Bogus","street":"123 Main","cit...
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[Demographic](js) should have message msg
      }
      it("Serialize object with unmapped hint class") {
        val strMatchHintMod = StringMatchHintModifier(Map("US" -> typeOf[USAddress]))
        val sj = ScalaJack().withHintModifiers((typeOf[Address], strMatchHintMod))
        val inst: Demographic = USDemographic(50, CanadaAddress("123 Main", "New York", "NY", "39822"))
        val msg = """No hint value mapping (in hint modifier) given for Type co.blocke.scalajack.json.custom.CanadaAddress"""
        the[SJError] thrownBy sj.render(inst) should have message msg
      }
    }
  }
}
