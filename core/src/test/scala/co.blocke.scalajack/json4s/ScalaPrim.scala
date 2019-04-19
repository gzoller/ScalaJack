package co.blocke.scalajack
package json4s

import org.scalatest.{ FunSpec, Matchers }
import scala.math.BigDecimal
import java.util.UUID
import TestUtil._
import org.json4s.JsonDSL._
import org.json4s._
//import org.json4s.jackson.JsonMethods._

class ScalaPrim() extends FunSpec with Matchers {

  val sj = ScalaJack(Json4sFlavor())

  describe("------------------------------------\n:  Scala Primitive Tests (Json4s)  :\n------------------------------------") {
    describe("+++ Positive Tests +++") {
      it("BigDecimal must work") {
        val inst = SampleBigDecimal(BigDecimal(123L), BigDecimal(1.23), BigDecimal(0), BigDecimal("123.456"), BigDecimal("0.1499999999999999944488848768742172978818416595458984375"), null)
        val js4s = sj.render(inst)
        val expected = JObject() ~ ("bd1" -> JDecimal(123L)) ~ ("bd2" -> JDecimal(1.23)) ~ ("bd3" -> JDecimal(0)) ~ ("bd4" -> JDecimal(123.456)) ~ ("bd5" -> JDecimal(BigDecimal("0.1499999999999999944488848768742172978818416595458984375"))) ~ ("bd6" -> JNull)
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        assertResult(inst) {
          sj.read[SampleBigDecimal](js4s)
        }
      }
      it("BigInt must work") {
        val inst = SampleBigInt(BigInt("-90182736451928374653345"), BigInt("90182736451928374653345"), BigInt(0), null)
        val js4s = sj.render(inst)
        val expected = JObject() ~ ("bi1" -> JInt(BigInt("-90182736451928374653345"))) ~ ("bi2" -> JInt(BigInt("90182736451928374653345"))) ~ ("bi3" -> JInt(0)) ~ ("bi4" -> JNull)
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        assertResult(inst) {
          sj.read[SampleBigInt](js4s)
        }
      }
      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(true, false)
        val js4s = sj.render(inst)
        val expected = JObject() ~ ("bool1" -> JBool(true)) ~ ("bool2" -> JBool(false))
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        assertResult(inst) {
          sj.read[SampleBoolean](js4s)
        }
      }
      it("Double must work (not nullable)") {
        val inst = SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val js4s = sj.render(inst)
        val expected = JObject() ~ ("d1" -> JDouble(Double.MaxValue)) ~ ("d2" -> JDouble(Double.MinValue)) ~ ("d3" -> JDouble(0.0)) ~ ("d4" -> JDouble(-123.4567))
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        assertResult(inst) {
          sj.read[SampleDouble](js4s)
        }
      }
      it("Enumeration must work (not nullable)") {
        val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
        val js4s = sj.render(inst)
        val expected = JObject() ~ ("e1" -> JString("Small")) ~ ("e2" -> JString("Medium")) ~ ("e3" -> JString("Large")) ~ ("e4" -> JNull) ~ ("e5" -> JString("Medium"))
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        // mutate e5 into an ordinal...
        val js2 = js4s.asInstanceOf[JObject] ~ ("e5" -> JInt(1))
        assertResult(inst) {
          sj.read[SampleEnum](js2)
        }
      }
      it("Int must work (not nullable)") {
        val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val js4s = sj.render(inst)
        val expected = JObject() ~ ("i1" -> JInt(Int.MaxValue)) ~ ("i2" -> JInt(Int.MinValue)) ~ ("i3" -> JInt(0)) ~ ("i4" -> JInt(123))
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        assertResult(inst) {
          sj.read[SampleInt](js4s)
        }
      }
      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val js4s = sj.render(inst)
        val expected = JObject() ~ ("l1" -> JLong(Long.MaxValue)) ~ ("l2" -> JLong(Long.MinValue)) ~ ("l3" -> JLong(0L)) ~ ("l4" -> JLong(123L))
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        assertResult(inst) {
          sj.read[SampleLong](js4s)
        }
      }
      it("Map of string-wrapped primitives work") {
        val inst = WrappedMaps(Map(3.toByte -> 2), Map(1 -> 2), Map(5L -> 7), Map(1.2 -> 3), Map(1.2F -> 3), Map(2.toShort -> 9), Map(BigInt(5) -> 6), Map(BigDecimal(4.9) -> 8), Map(true -> 1))
        val js4s = sj.render(inst)
        val expected = JObject(List(
          "a" -> JObject(List("3" -> JInt(2))),
          "b" -> JObject(List("1" -> JInt(2))),
          "c" -> JObject(List("5" -> JInt(7))),
          "d" -> JObject(List("1.2" -> JInt(3))),
          "e" -> JObject(List("1.2" -> JInt(3))),
          "f" -> JObject(List("2" -> JInt(9))),
          "g" -> JObject(List("5" -> JInt(6))),
          "h" -> JObject(List("4.9" -> JInt(8))),
          "i" -> JObject(List("true" -> JInt(1)))
        ))
        assertResult(Diff(JNothing, JNothing, JNothing)) { js4s.diff(expected) }
        assertResult(inst) {
          sj.read[WrappedMaps](js4s)
        }
      }
    }
    /*
  describe("--- Negative Tests ---") {
      it("BigDecimal must break") {
        val js = """{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.1499999999999999944488848768742172978818416595458984375","bd6":null}"""
        val msg = """[$.bd5]: Expected Number here but found String
                    |...99999999999944488848768742172978818416595458984375","bd6":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBigDecimal](js) should have message msg
      }
      it("BigInt must break") {
        val js = """{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4":null}"""
        val msg = """[$.bi1]: Expected Number here but found String
                      |{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4"...
                      |-------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBigInt](js) should have message msg
      }
      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":"false"}"""
        val msg = """[$.bool2]: Expected Boolean here but found String
                      |{"bool1":true,"bool2":"false"}
                      |---------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBoolean](js) should have message msg
        val js2 = """{"bool1":true,"bool2":123}"""
        val msg2 = """[$.bool2]: Expected Boolean here but found Number
                              |{"bool1":true,"bool2":123}
                              |------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBoolean](js2) should have message msg2
        val js3 = """{"bool1":true,"bool2":null}"""
        val msg3 = """[$.bool2]: Expected Boolean here but found Null
                              |{"bool1":true,"bool2":null}
                              |-------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBoolean](js3) should have message msg3
      }
      it("Byte must break") {
        val js = """{"b1":true,"b2":-128,"b3":0,"b4":64}"""
        val msg = """[$.b1]: Expected Number here but found Boolean
                      |{"b1":true,"b2":-128,"b3":0,"b4":64}
                      |---------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleByte](js) should have message msg
        val js2 = """{"b1":12,"b2":-128,"b3":0,"b4":null}"""
        val msg2 = """[$.b4]: Expected Number here but found Null
                      |{"b1":12,"b2":-128,"b3":0,"b4":null}
                      |----------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleByte](js2) should have message msg2
      }
      it("Char must break") {
        val js = """{"c1":null,"c2":"Y","c3":"Z"}"""
        val msg = """[$.c1]: A Char typed value cannot be null
                      |{"c1":null,"c2":"Y","c3":"Z"}
                      |---------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleChar](js) should have message msg
        val js2 = """{"c1":"","c2":"Y","c3":"Z"}"""
        val msg2 = """[$.c1]: Tried to read a Char but empty string found
                      |{"c1":"","c2":"Y","c3":"Z"}
                      |------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleChar](js2) should have message msg2
      }
      it("Double must break") {
        val js = """{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}"""
        val msg = """[$.d1]: Unable to read value (e.g. bad number format)
                      |{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123...
                      |----------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleDouble](js) should have message msg
      }
      it("Enumeration must break") {
        val js = """{"e1":"Small","e2":"Bogus","e3":"Large","e4":null,"e5":"Medium"}"""
        val msg = """[$.e2]: No value found in enumeration co.blocke.scalajack.json.primitives.Size$ for Bogus
                      |{"e1":"Small","e2":"Bogus","e3":"Large","e4":null,"e5":"Medium"}
                      |------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleEnum](js) should have message msg
        val js2 = """{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":9}"""
        val msg2 = """[$.e5]: No value found in enumeration co.blocke.scalajack.json.primitives.Size$ for 9
                      |...Small","e2":"Medium","e3":"Large","e4":null,"e5":9}
                      |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleEnum](js2) should have message msg2
        val js3 = """{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":false}"""
        val msg3 = """[$.e5]: Expected String or Int, not Boolean when reading Enumeration value
                      |...l","e2":"Medium","e3":"Large","e4":null,"e5":false}
                      |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleEnum](js3) should have message msg3
      }
      it("Float must break") {
        val js = """{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}"""
        val msg = """[$.f2]: Expected Number here but found String
                      |{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}
                      |-------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleFloat](js) should have message msg
      }
      it("Int must break") {
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}"""
        val msg = """[$.i3]: Expected Number here but found String
                      |{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}
                      |----------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleInt](js) should have message msg
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}"""
        val msg2 = """[$.i3]: Unable to read value (e.g. bad number format)
                      |{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}
                      |-----------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleInt](js2) should have message msg2
      }
      it("Long must break") {
        val js = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}"""
        val msg = """[$.l3]: Expected Number here but found Boolean
                      |...72036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}
                      |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleLong](js) should have message msg
        val js2 = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}"""
        val msg2 = """[$.l3]: Unable to read value (e.g. bad number format)
                      |...372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}
                      |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleLong](js2) should have message msg2
      }
      it("Short must break") {
        val js = """{"s1":32767,"s2":true,"s3":0,"s4":123}"""
        val msg = """[$.s2]: Expected Number here but found Boolean
                      |{"s1":32767,"s2":true,"s3":0,"s4":123}
                      |--------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleShort](js) should have message msg
        val js2 = """{"s1":32767,"s2":3.4,"s3":0,"s4":123}"""
        val msg2 = """[$.s2]: Unable to read value (e.g. bad number format)
                      |{"s1":32767,"s2":3.4,"s3":0,"s4":123}
                      |-------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleShort](js2) should have message msg2
      }
      it("String must break") {
        val js = """{"s1":"something","s2":-19,"s3":null}"""
        val msg = """[$.s2]: Expected String here but found Number
                      |{"s1":"something","s2":-19,"s3":null}
                      |-------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleString](js) should have message msg
      }
      it("UUID must break") {
        val js = """{"u1":"bogus","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}"""
        val msg = """[$.u1]: Failed to create UUID value from parsed text bogus
                      |{"u1":"bogus","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}
                      |-----------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleUUID](js) should have message msg
      }
      it("Can't find TypeAdapter for given type") {
        val js = """{"hey":"you"}"""
        val msg = "Unable to find a type adapter for InputStream (may be abstract or a dependency of an abstract class)"
        the[java.lang.IllegalArgumentException] thrownBy sj.read[java.lang.Process](js) should have message msg
      }
      }
 */
  }
}
