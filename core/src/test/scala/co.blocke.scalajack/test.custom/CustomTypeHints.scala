package co.blocke.scalajack
package test
package custom

import org.scalatest.{ FunSpec, Matchers }
import scala.reflect.runtime.universe.typeOf

class CustomTypeHints() extends FunSpec with Matchers {

  describe("-----------------------------\n:  Custom Type Hints Tests  :\n-----------------------------") {
    describe("+++ Positive Tests +++") {
      it("Override default trait/polymorphic type hint") {
        val sj = ScalaJack().withDefaultHint("which")
        val inst: Address = USAddress("123 Main", "New York", "NY", "39822")
        val js = sj.render(inst)
        assertResult("""{"which":"co.blocke.scalajack.test.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}""") { js }
        assertResult(inst) {
          sj.read[Address](js)
        }
      }
      it("Override type-specific trait/polymorphic type hint") {
        val sj = ScalaJack().withHints((typeOf[Address] -> "addr_kind"))
        val inst: Demographic = USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
        val js = sj.render(inst)
        assertResult("""{"_hint":"co.blocke.scalajack.test.custom.USDemographic","age":50,"address":{"addr_kind":"co.blocke.scalajack.test.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""") { js }
        assertResult(inst) {
          sj.read[Demographic](js)
        }
      }
      it("Use ClassNameHintModifier to modify trait/polymorphic type hint value") {
        val prependHintMod = ClassNameHintModifier((hint: String) => "co.blocke.scalajack.test.custom." + hint, (cname: String) => cname.split('.').last)
        val sj = ScalaJack().withHintModifiers((typeOf[Address], prependHintMod))
        val inst: Demographic = USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
        val js = sj.render(inst)
        assertResult("""{"_hint":"co.blocke.scalajack.test.custom.USDemographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""") { js }
        assertResult(inst) {
          sj.read[Demographic](js)
        }
      }
      it("Use StringMatchHintModifier to modify trait/polymorphic type hint value") {
        val strMatchHintMod = StringMatchHintModifier(Map("US" -> typeOf[USAddress]))
        val sj = ScalaJack().withHintModifiers((typeOf[Address], strMatchHintMod))
        val inst: Demographic = USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
        val js = sj.render(inst)
        assertResult("""{"_hint":"co.blocke.scalajack.test.custom.USDemographic","age":50,"address":{"_hint":"US","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""") { js }
        assertResult(inst) {
          sj.read[Demographic](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Use unspecified type hint") {
        val sj = ScalaJack().withDefaultHint("which")
        val js = """{"bogus":"co.blocke.scalajack.test.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}"""
        val msg = """Could not find type field named "which"
          |ain","city":"New York","state":"NY","postalCode":"39822"}
          |--------------------------------------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[Address](js) should have message msg
      }
      it("Ignore type-specific type hint (e.g. use default) when a specific hint is specified") {
        val sj = ScalaJack().withDefaultHint("which")
        val js = """{"_hint":"co.blocke.scalajack.test.custom.USDemographic","age":50,"address":{"_hint":"co.blocke.scalajack.test.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
        val msg = """Could not find type field named "which"
          |city":"New York","state":"NY","postalCode":"39822"}}
          |--------------------------------------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[Address](js) should have message msg
      }
      it("Hint value after modification doesn't resolve to known class name") {
        val prependHintMod = ClassNameHintModifier((hint: String) => "co.blocke.scalajack.test.bogus." + hint, (cname: String) => cname.split('.').last)
        val sj = ScalaJack().withHintModifiers((typeOf[Address], prependHintMod))
        val js = """{"_hint":"co.blocke.scalajack.test.custom.USDemographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
        an[scala.ScalaReflectionException] should be thrownBy sj.read[Demographic](js)
      }
      it("Unknown string given as type hint value (no cooresponding match to class in mapping)") {
        val strMatchHintMod = StringMatchHintModifier(Map("US" -> typeOf[USAddress]))
        val sj = ScalaJack().withHintModifiers((typeOf[Address], strMatchHintMod))
        val js = """{"_hint":"co.blocke.scalajack.test.custom.USDemographic","age":50,"address":{"_hint":"Bogus","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}"""
        val msg = """No Type mapping given for hint Bogus"""
        the[java.lang.IllegalStateException] thrownBy sj.read[Demographic](js) should have message msg
      }
      it("Serialize object with unmapped hint class") {
        val strMatchHintMod = StringMatchHintModifier(Map("US" -> typeOf[USAddress]))
        val sj = ScalaJack().withHintModifiers((typeOf[Address], strMatchHintMod))
        val inst: Demographic = USDemographic(50, CanadaAddress("123 Main", "New York", "NY", "39822"))
        val msg = """No hint value mapping given for Type co.blocke.scalajack.test.custom.CanadaAddress"""
        the[java.lang.IllegalStateException] thrownBy sj.render(inst) should have message msg
      }
    }
  }
}