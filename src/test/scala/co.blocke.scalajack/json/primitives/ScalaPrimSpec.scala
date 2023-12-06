package co.blocke.scalajack
package json
package primitives

import ScalaJack.*
import co.blocke.scala_reflection.*
import scala.math.BigDecimal
import java.util.UUID
import TestUtil.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*

class ScalaPrimSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("---------------------------\n:  Scala Primitive Tests  :\n---------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("BigDecimal must work") {
        val inst = SampleBigDecimal(
          BigDecimal(123L),
          BigDecimal(1.23),
          BigDecimal(0),
          BigDecimal("123.456"),
          BigDecimal(
            "0.1499999999999999944488848768742172978818416595458984375"
          ),
          null
        )

        val js = sj[SampleBigDecimal].toJson(inst)
        js should matchJson("""{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":0.1499999999999999944488848768742172978818416595458984375,"bd6":null}""")
        // inst shouldEqual ScalaJack.read[SampleBigDecimal](js)
      }

      it("BigInt must work") {
        val inst = SampleBigInt(
          BigInt("-90182736451928374653345"),
          BigInt("90182736451928374653345"),
          BigInt(0),
          null
        )
        val js = sj[SampleBigInt].toJson(inst)
        js should matchJson("""{"bi1":-90182736451928374653345,"bi2":90182736451928374653345,"bi3":0,"bi4":null}""")
        // inst shouldEqual ScalaJack.read[SampleBigInt](js)
      }

      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(bool1 = true, bool2 = false)
        val js = sj[SampleBoolean].toJson(inst)
        js should matchJson("""{"bool1":true,"bool2":false}""")
        // inst shouldEqual ScalaJack.read[SampleBoolean](js)
      }

      it("Byte must work (not nullable)") {
        val inst = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
        val js = sj[SampleByte].toJson(inst)
        js should matchJson("""{"b1":127,"b2":-128,"b3":0,"b4":64}""")
        // inst shouldEqual ScalaJack.read[SampleByte](js)
      }

      it("Char must work (not nullable)") {
        val inst = SampleChar(Char.MaxValue, 'Z', '\u20A0')
        val js = sj[SampleChar].toJson(inst)
        js should matchJson("""{"c1":"\""" + """uffff","c2":"Z","c3":"\""" + """u20a0"}""")
        // inst shouldEqual ScalaJack.read[SampleChar](js)
      }

      it("Double must work (not nullable)") {
        val inst =
          SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val js = sj[SampleDouble].toJson(inst)
        js should matchJson("""{"d1":1.7976931348623157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}""")
        // inst shouldEqual ScalaJack.read[SampleDouble](js)
      }

      it("Float must work (not nullable)") {
        val inst = SampleFloat(Float.MaxValue, Float.MinValue, 0.0f, -123.4567f)
        val js = sj[SampleFloat].toJson(inst)
        js should matchJson("""{"f1":3.4028235E38,"f2":-3.4028235E38,"f3":0.0,"f4":-123.4567}""")
        // inst shouldEqual ScalaJack.read[SampleFloat](js)
      }

      it("Int must work (not nullable)") {
        val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val js = sj[SampleInt].toJson(inst)
        js should matchJson("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123}""")
        // inst shouldEqual ScalaJack.read[SampleInt](js)
      }

      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val js = sj[SampleLong].toJson(inst)
        js should matchJson("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123}""")
        // inst shouldEqual ScalaJack.read[SampleLong](js)
      }

      it("Short must work (not nullable)") {
        val inst = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
        val js = sj[SampleShort].toJson(inst)
        js should matchJson("""{"s1":32767,"s2":-32768,"s3":0,"s4":123}""")
        // inst shouldEqual ScalaJack.read[SampleShort](js)
      }

      it("String must work") {
        val inst = SampleString("something\b\n\f\r\t☆", "", null)
        val js = sj[SampleString].toJson(inst)
        // The weird '+' here is to break up the unicode so it won't be interpreted and wreck the test.
        js should matchJson("""{"s1":"something\b\n\f\r\t\""" + """u2606","s2":"","s3":null}""")
        // inst shouldEqual ScalaJack.read[SampleString](js)
      }

      /*
      it("UUID must work") {
        val inst = SampleUUID(
          null,
          UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9")
        )
        val js = ScalaJack.write(inst)
        js should matchJson("""{"u1":null,"u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}""")
        inst shouldEqual ScalaJack.read[SampleUUID](js)
      }
    }

    // --------------------------------------------------------

    describe(colorString("--- Negative Tests ---")) {
      it("BigDecimal must break") {
        val js =
          """{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.1499999999999999944488848768742172978818416595458984375","bd6":null}"""
        val msg: String =
          """Float/Double expected but couldn't parse from """" + "\"" + """" at position [50]
                  |{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.149999999999999994448884...
                  |--------------------------------------------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleBigDecimal](js)
        thrown.getMessage should equal(msg)
      }

      it("BigInt must break") {
        val js =
          """{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4":null}"""
        val msg =
          """Int/Long expected but couldn't parse from """" + "\"" + """" at position [7]
                  |{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4"...
                  |-------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleBigInt](js)
        thrown.getMessage should equal(msg)
      }

      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":"false"}"""
        val msg = """Unexpected character '"' where beginning of boolean value expected at position [22]
                  |{"bool1":true,"bool2":"false"}
                  |----------------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleBoolean](js)
        thrown.getMessage should equal(msg)
        val js2 = """{"bool1":true,"bool2":123}"""
        val msg2 = """Unexpected character '1' where beginning of boolean value expected at position [22]
                    |{"bool1":true,"bool2":123}
                    |----------------------^""".stripMargin
        val thrown2 = the[JsonParseError] thrownBy ScalaJack.read[SampleBoolean](js2)
        thrown2.getMessage should equal(msg2)
        val js3 = """{"bool1":true,"bool2":null}"""
        val msg3 = """Unexpected character 'n' where beginning of boolean value expected at position [22]
                    |{"bool1":true,"bool2":null}
                    |----------------------^""".stripMargin
        val thrown3 = the[JsonParseError] thrownBy ScalaJack.read[SampleBoolean](js3)
        thrown3.getMessage should equal(msg3)
      }

      it("Byte must break") {
        val js = """{"b1":true,"b2":-128,"b3":0,"b4":64}"""
        val msg = """Int/Long expected but couldn't parse from "t" at position [6]
                  |{"b1":true,"b2":-128,"b3":0,"b4":64}
                  |------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleByte](js)
        thrown.getMessage should equal(msg)
        val js2 = """{"b1":12,"b2":-128,"b3":0,"b4":null}"""
        val msg2 = """Int/Long expected but couldn't parse from "n" at position [31]
                  |{"b1":12,"b2":-128,"b3":0,"b4":null}
                  |-------------------------------^""".stripMargin
        val thrown2 = the[JsonParseError] thrownBy ScalaJack.read[SampleByte](js2)
        thrown2.getMessage should equal(msg2)
      }

      it("Char must break") {
        val js = """{"c1":null,"c2":"Y","c3":"Z"}"""
        val msg = """Char typed values cannot be null at position [10]
                |{"c1":null,"c2":"Y","c3":"Z"}
                |----------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleChar](js)
        thrown.getMessage should equal(msg)
        val js2 = """{"c1":"","c2":"Y","c3":"Z"}"""
        val msg2 = """Cannot convert value '' into a Char at position [8]
                |{"c1":"","c2":"Y","c3":"Z"}
                |--------^""".stripMargin
        val thrown2 = the[JsonParseError] thrownBy ScalaJack.read[SampleChar](js2)
        thrown2.getMessage should equal(msg2)
      }

      it("Double must break") {
        val js =
          """{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}"""
        val msg =
          """Float/Double expected but couldn't parse from "1.79769313486E23157E308" at position [29]
                  |{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123...
                  |------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleDouble](js)
        thrown.getMessage should equal(msg)
      }

      it("Float must break") {
        val js =
          """{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}"""
        val msg =
          """Float/Double expected but couldn't parse from """" + "\"" + """" at position [24]
                  |{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}
                  |------------------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleFloat](js)
        thrown.getMessage should equal(msg)
      }

      it("Int must break") {
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}"""
        val msg = """Int/Long expected but couldn't parse from """" + "\"" + """" at position [39]
                  |{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}
                  |---------------------------------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleInt](js)
        thrown.getMessage should equal(msg)
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}"""
        val msg2 = """Comma expected at position [40]
                    |{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}
                    |----------------------------------------^""".stripMargin
        val thrown2 = the[CommaExpected] thrownBy ScalaJack.read[SampleInt](js2)
        thrown2.getMessage should equal(msg2)
      }

      it("Long must break") {
        val js =
          """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}"""
        val msg =
          """Int/Long expected but couldn't parse from "t" at position [57]
                  |...23372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}
                  |----------------------------------------------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleLong](js)
        thrown.getMessage should equal(msg)
        val js2 =
          """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}"""
        val msg2 =
          """Comma expected at position [58]
            |...3372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}
            |----------------------------------------------------^""".stripMargin
        val thrown2 = the[CommaExpected] thrownBy ScalaJack.read[SampleLong](js2)
        thrown2.getMessage should equal(msg2)
      }

      it("Short must break") {
        val js = """{"s1":32767,"s2":true,"s3":0,"s4":123}"""
        val msg = """Int/Long expected but couldn't parse from "t" at position [17]
                  |{"s1":32767,"s2":true,"s3":0,"s4":123}
                  |-----------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleShort](js)
        thrown.getMessage should equal(msg)
        val js2 = """{"s1":32767,"s2":3.4,"s3":0,"s4":123}"""
        val msg2 = """Comma expected at position [18]
                    |{"s1":32767,"s2":3.4,"s3":0,"s4":123}
                    |------------------^""".stripMargin
        val thrown2 = the[CommaExpected] thrownBy ScalaJack.read[SampleShort](js2)
        thrown2.getMessage should equal(msg2)
      }

      it("String must break") {
        val js = """{"s1":"something","s2":-19,"s3":null}"""
        val msg = """Unexpected character '-' where beginning of a string expected at position [23]
                  |{"s1":"something","s2":-19,"s3":null}
                  |-----------------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleString](js)
        thrown.getMessage should equal(msg)
      }

      it("UUID must break") {
        val js =
          """{"u1":"bogus","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}"""
        val msg = """Unable to marshal UUID from value 'bogus' at position [13]
                  |{"u1":"bogus","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}
                  |-------------^""".stripMargin
        val thrown = the[JsonParseError] thrownBy ScalaJack.read[SampleUUID](js)
        thrown.getMessage should equal(msg)
      }
       */
    }
  }
