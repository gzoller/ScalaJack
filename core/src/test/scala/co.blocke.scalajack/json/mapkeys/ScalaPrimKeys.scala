package co.blocke.scalajack
package json.mapkeys

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec
import json.JsonMatcher._

class ScalaPrimKeys() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------------------\n:  Scala Primitive Map Key Tests  :\n-----------------------------------") {
    describe("+++ Positive Tests +++") {
      it("With Any Key") {
        val inst = AnyShell(Map(List(1, 2, 3) -> List("a", "b", "c"), DogPet("Fido", Food.Meat, 4) -> DogPet("Fifi", Food.Meat, 4), Size.Small -> "ok", 123.456 -> true, 293845 -> "Greg", false -> "16", "Fred" -> "Wilma", 16.toByte -> null))
        val js = sj.render(inst)
        parseJValue(js) should matchJson(parseJValue("""{"m":{"false":"16","Small":"ok","123.456":true,"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}":{"_hint":"co.blocke.scalajack.json.mapkeys.DogPet","name":"Fifi","food":"Meat","numLegs":4},"Fred":"Wilma","[1,2,3]":["a","b","c"],"293845":"Greg","16":null}}"""))
        val read = sj.read[AnyShell](js).m.keySet.map(z => (z, z.getClass.getName))
        read.contains((16, "scala.math.BigInt")) should be(true)
        read.contains((293845, "scala.math.BigInt")) should be(true)
        read.contains((123.456, "scala.math.BigDecimal")) should be(true)
        read.contains(("Small", "java.lang.String")) should be(true)
        read.contains(("Fred", "java.lang.String")) should be(true)
        read.contains((false, "java.lang.Boolean")) should be(true)
        read.contains((DogPet("Fido", Food.Meat, 4), "co.blocke.scalajack.json.mapkeys.DogPet")) should be(true)
      }
      it("With BigDecimal Key") {
        val inst = SampleBigDecimal(Map(BigDecimal(123.456) -> BigDecimal(1), BigDecimal(789.123) -> BigDecimal(2)))
        val js = sj.render(inst)
        assertResult("""{"m":{"123.456":1,"789.123":2}}""") { js }
        assertResult(inst) {
          sj.read[SampleBigDecimal](js)
        }
      }
      it("With BigInt Key") {
        val inst = SampleBigInt(Map(BigInt(123) -> BigInt(1), BigInt(789) -> BigInt(2)))
        val js = sj.render(inst)
        assertResult("""{"m":{"123":1,"789":2}}""") { js }
        assertResult(inst) {
          sj.read[SampleBigInt](js)
        }
      }
      it("With Boolean Key") {
        val inst = SampleBoolean(Map(true -> false, false -> true))
        val js = sj.render(inst)
        assertResult("""{"m":{"true":false,"false":true}}""") { js }
        assertResult(inst) {
          sj.read[SampleBoolean](js)
        }
      }
      it("With Byte Key") {
        val inst = SampleByte(Map(16.toByte -> 2.toByte, 48.toByte -> 9.toByte))
        val js = sj.render(inst)
        assertResult("""{"m":{"16":2,"48":9}}""") { js }
        assertResult(inst) {
          sj.read[SampleByte](js)
        }
      }
      it("With Char Key") {
        val inst = SampleChar(Map('a' -> 'A', 'z' -> 'Z'))
        val js = sj.render(inst)
        assertResult("""{"m":{"a":"A","z":"Z"}}""") { js }
        assertResult(inst) {
          sj.read[SampleChar](js)
        }
      }
      it("With Double Key") {
        val inst = SampleDouble(Map(12.34 -> 56.78, 90.12 -> 34.56))
        val js = sj.render(inst)
        assertResult("""{"m":{"12.34":56.78,"90.12":34.56}}""") { js }
        assertResult(inst) {
          sj.read[SampleDouble](js)
        }
      }
      it("With Enumeration Key") {
        val inst = SampleEnumeration(Map(Size.Small -> Size.Large, Size.Large -> Size.Medium))
        val js = sj.render(inst)
        assertResult("""{"m":{"Small":"Large","Large":"Medium"}}""") { js }
        assertResult(inst) {
          sj.read[SampleEnumeration](js)
        }
      }
      it("With Float Key") {
        val inst = SampleFloat(Map(12.34F -> 56.78F, 90.12F -> 34.56F))
        val js = sj.render(inst)
        assertResult("""{"m":{"12.34":56.78,"90.12":34.56}}""") { js }
        assertResult(inst) {
          sj.read[SampleFloat](js)
        }
      }
      it("With Int Key") {
        val inst = SampleInt(Map(12 -> 56, 90 -> 34))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56,"90":34}}""") { js }
        assertResult(inst) {
          sj.read[SampleInt](js)
        }
      }
      it("With Long Key") {
        val inst = SampleLong(Map(12L -> 56L, 90L -> 34L))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56,"90":34}}""") { js }
        assertResult(inst) {
          sj.read[SampleLong](js)
        }
      }
      it("With Short Key") {
        val inst = SampleShort(Map(12.toShort -> 56.toShort, 90.toShort -> 34.toShort))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56,"90":34}}""") { js }
        assertResult(inst) {
          sj.read[SampleShort](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Bad BigDecimal Key") {
        val js = """{"m":{"789.123":1,"fred":2}}"""
        val msg = """[<tokenizing>]: Unexpected character 'f' at position 0
                    |fred
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBigDecimal](js) should have message msg
      }
      it("Bad BigInt Key") {
        val js = """{"m":{"fred":1,"789":2}}"""
        val msg = """[<tokenizing>]: Unexpected character 'f' at position 0
                    |fred
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBigInt](js) should have message msg
      }
      it("Bad Boolean Key") {
        val js = """{"m":{"true":false,"123":true}}"""
        val msg = """[$.m.(map key)]: Expected Boolean here but found Number
                    |123
                    |--^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBoolean](js) should have message msg
      }
      it("Bad Byte Key") {
        val js = """{"m":{"16":2,"x48":9}}"""
        val msg = """[<tokenizing>]: Unexpected character x at position 0
                    |x48
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleByte](js) should have message msg
      }
      it("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
        val js = """{"m":{null:"A","z":"Z"}}"""
        val msg = """[$.m.(map key)]: A Char typed value cannot be null
                    |{"m":{null:"A","z":"Z"}}
                    |---------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleChar](js) should have message msg
      }
      it("Bad Double Key") {
        val js = """{"m":{"12.34":56.78,"true":34.56}}"""
        val msg = """[$.m.(map key)]: Expected Number here but found Boolean
                    |true
                    |---^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleDouble](js) should have message msg
      }
      it("Bad Enumeration Key") {
        val js = """{"m":{"Small":"Large","Bogus":"Medium"}}"""
        val msg = """[$.m.(map key)]: No value found in enumeration co.blocke.scalajack.json.mapkeys.Size$ for Bogus
                    |{"m":{"Small":"Large","Bogus":"Medium"}}
                    |---------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleEnumeration](js) should have message msg
      }
      it("Bad Float Key") {
        val js = """{"m":{"12.34":56.78,"90.12.3":34.56}}"""
        val msg = """[$.m.(map key)]: Unable to read value (e.g. bad number format)
                    |90.12.3
                    |------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleFloat](js) should have message msg
      }
      it("Bad Int Key") {
        val js = """{"m":{"12.0":56,"90":34}}"""
        val msg = """[$.m.(map key)]: Unable to read value (e.g. bad number format)
                    |12.0
                    |---^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleInt](js) should have message msg
      }
      it("Bad Long Key") {
        val js = """{"m":{"12":56,"hey":34}}"""
        val msg = """[<tokenizing>]: Unexpected character h at position 0
                    |hey
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleLong](js) should have message msg
      }
      it("Bad Short Key") {
        val js = """{"m":{"p99999":56,"90":34}}"""
        val msg = """[<tokenizing>]: Unexpected character p at position 0
                    |p99999
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleShort](js) should have message msg
      }
    }
  }
}
