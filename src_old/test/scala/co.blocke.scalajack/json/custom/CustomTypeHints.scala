package co.blocke.scalajack
package json.custom

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import co.blocke.scalajack.model._

class CustomTypeHints() extends FunSuite:

  test("Override default trait/polymorphic type hint") {
    describe("-----------------------------\n:  Custom Type Hints Tests  :\n-----------------------------", Console.BLUE)
    describe("+++ Positive Tests +++")

    val sj = co.blocke.scalajack.ScalaJack().withDefaultHint("which")
    val inst: Address = USAddress("123 Main", "New York", "NY", "39822")
    val js = sj.render(inst)
    assertEquals(
      """{"which":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[Address](js))
  }

  test("Override type-specific trait/polymorphic type hint") {
    val sj = co.blocke.scalajack.ScalaJack().withHints(RType.of[Address] -> "addr_kind")
    val inst: Demographic =
      USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"addr_kind":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[Demographic](js))
  }

  test(
    "Use ClassNameHintModifier to modify trait/polymorphic type hint value"
  ) {
    val prependHintMod = ClassNameHintModifier(
      (hint: String) => "co.blocke.scalajack.json.custom." + hint,
      (cname: String) => cname.split('.').last
    )
    val sj =
      co.blocke.scalajack.ScalaJack().withHintModifiers((RType.of[Address], prependHintMod))
    val inst: Demographic =
      USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[Demographic](js))
  }

  test(
    "Use StringMatchHintModifier to modify trait/polymorphic type hint value"
  ) {
    val strMatchHintMod =
      StringMatchHintModifier(Map("US" -> classOf[USAddress].getName))
    val sj =
      co.blocke.scalajack.ScalaJack().withHintModifiers((RType.of[Address], strMatchHintMod))
    val inst: Demographic =
      USDemographic(50, USAddress("123 Main", "New York", "NY", "39822"))
    val js = sj.render(inst)
    assertEquals(
      """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"US","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[Demographic](js))
  }

  test("Use unspecified type hint") {
    describe("--- Negative Tests ---")
    val sj = co.blocke.scalajack.ScalaJack().withDefaultHint("which")
    val js =
      """{"bogus":"co.blocke.scalajack.json.custom.USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}""".asInstanceOf[JSON]
    val msg = """Type hint 'which' not found
              |...ity":"New York","state":"NY","postalCode":"39822"}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[Address](js)
    }
  }

  test("Hint value after modification doesn't resolve to known class name") {
    val prependHintMod = ClassNameHintModifier(
      (hint: String) => "co.blocke.scalajack.bogus." + hint,
      (cname: String) => cname.split('.').last
    )
    val sj =
      co.blocke.scalajack.ScalaJack().withHintModifiers((RType.of[Address], prependHintMod))
    val js =
      """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""".asInstanceOf[JSON]
    val msg =
      """Couldn't marshal class for USAddress
              |...mographic","age":50,"address":{"_hint":"USAddress","street":"123 Main","city...
              |----------------------------------------------------^""".stripMargin
    interceptMessage[java.lang.ClassNotFoundException]("""co.blocke.scalajack.bogus.USAddress"""){
      sj.read[Demographic](js)
    }
  }

  test(
    "Unknown string given as type hint value (no cooresponding match to class in mapping)"
  ) {
      val strMatchHintMod =
        StringMatchHintModifier(Map("US" -> "co.blocke.scalajack.json.custom.USDemographic"))
      val sj =
        co.blocke.scalajack.ScalaJack().withHintModifiers((RType.of[Address], strMatchHintMod))
      val js =
        """{"_hint":"co.blocke.scalajack.json.custom.USDemographic","age":50,"address":{"_hint":"Bogus","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}}""".asInstanceOf[JSON]
      val msg =
        """Couldn't marshal class for Bogus
              |...USDemographic","age":50,"address":{"_hint":"Bogus","street":"123 Main","city...
              |----------------------------------------------------^""".stripMargin
      interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
        sj.read[Demographic](js)
      }
    }

    test("Serialize object with unmapped hint class") {
    val strMatchHintMod =
      StringMatchHintModifier(Map("US" -> "co.blocke.scalajack.json.custom.USDemographic"))
    val sj =
      co.blocke.scalajack.ScalaJack().withHintModifiers((RType.of[Address], strMatchHintMod))
    val inst: Demographic = USDemographic(
      50,
      CanadaAddress("123 Main", "New York", "NY", "39822")
    )
    val msg =
      """key not found: co.blocke.scalajack.json.custom.CanadaAddress"""
    interceptMessage[java.util.NoSuchElementException](msg){
      sj.render(inst)
    }
  }
