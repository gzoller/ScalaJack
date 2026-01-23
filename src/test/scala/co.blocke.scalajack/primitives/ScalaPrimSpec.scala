package co.blocke.scalajack
package json
package primitives

import ScalaJack.*
import scala.math.BigDecimal
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

        val sj = sjCodecOf[SampleBigDecimal]
        val js = sj.toJson(inst)
        js should matchJson("""{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":0.1499999999999999944488848768742172978818416595458984375,"bd6":null}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("BigInt must work") {
        val inst = SampleBigInt(
          BigInt("-90182736451928374653345"),
          BigInt("90182736451928374653345"),
          BigInt(0),
          null
        )
        val sj = sjCodecOf[SampleBigInt]
        val js = sj.toJson(inst)
        js should matchJson("""{"bi1":-90182736451928374653345,"bi2":90182736451928374653345,"bi3":0,"bi4":null}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(bool1 = true, bool2 = false)
        val sj = sjCodecOf[SampleBoolean]
        val js = sj.toJson(inst)
        js should matchJson("""{"bool1":true,"bool2":false}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Byte must work (not nullable)") {
        val inst = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
        val sj = sjCodecOf[SampleByte]
        val js = sj.toJson(inst)
        js should matchJson("""{"b1":127,"b2":-128,"b3":0,"b4":64}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Char must work (not nullable)") {
        val inst = SampleChar(Char.MaxValue, 'Z', '\u20A0')
        val sj = sjCodecOf[SampleChar]
        val js = sj.toJson(inst)
        js should matchJson("""{"c1":"\""" + """uffff","c2":"Z","c3":"\""" + """u20a0"}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Double must work (not nullable)") {
        val inst =
          SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val sj = sjCodecOf[SampleDouble]
        val js = sj.toJson(inst)
        js should matchJson("""{"d1":1.7976931348623157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Float must work (not nullable)") {
        val inst = SampleFloat(Float.MaxValue, Float.MinValue, 0.0f, -123.4567f)
        val sj = sjCodecOf[SampleFloat]
        val js = sj.toJson(inst)
        js should matchJson("""{"f1":3.4028235E38,"f2":-3.4028235E38,"f3":0.0,"f4":-123.4567}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Int must work (not nullable)") {
        val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val sj = sjCodecOf[SampleInt]
        val js = sj.toJson(inst)
        js should matchJson("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val sj = sjCodecOf[SampleLong]
        val js = sj.toJson(inst)
        js should matchJson("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Short must work (not nullable)") {
        val inst = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
        val sj = sjCodecOf[SampleShort]
        val js = sj.toJson(inst)
        js should matchJson("""{"s1":32767,"s2":-32768,"s3":0,"s4":123}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("String must work") {
        val inst = SampleString("something\b\n\f\r\t☆", "", null)
        val sj = sjCodecOf[SampleString]
        val js = sj.toJson(inst)
        // The weird '+' here is to break up the unicode so it won't be interpreted and wreck the test.
        js should matchJson("""{"s1":"something\b\n\f\r\t\""" + """u2606","s2":"","s3":null}""")
        sj.fromJson(js) shouldEqual inst
      }

      it("Any type for all primitives must work") {
        val sj = sjCodecOf[AnyShell]
        val prims: List[(Any, String, Option[Any => String])] = List(
          (null, """{"a":null}""", None),
          (scala.math.BigDecimal(5), """{"a":5}""", None),
          (scala.math.BigInt(5), """{"a":5}""", None),
          (true, """{"a":true}""", None),
          (5.toByte, """{"a":5}""", None),
          ('x', """{"a":"x"}""", Some((c: Any) => c.toString)),
          (5.0, """{"a":5.0}""", None),
          (5.0.toFloat, """{"a":5.0}""", None),
          (5, """{"a":5}""", None),
          (5L, """{"a":5}""", None),
          (5.toShort, """{"a":5}""", None),
          ("foo", """{"a":"foo"}""", None),
          (java.lang.Boolean.valueOf(true), """{"a":true}""", None),
          (java.lang.Byte.valueOf(5.toByte), """{"a":5}""", None),
          (java.lang.Character.valueOf('x'), """{"a":"x"}""", Some((c: Any) => c.toString)),
          (java.lang.Double.valueOf(5.0), """{"a":5.0}""", None),
          (java.lang.Float.valueOf(5.0.toFloat), """{"a":5.0}""", None),
          (java.lang.Integer.valueOf(5), """{"a":5}""", None),
          (java.lang.Long.valueOf(5), """{"a":5}""", None),
          (java.lang.Short.valueOf(5.toShort), """{"a":5}""", None),
          (java.lang.Integer.valueOf(5).asInstanceOf[java.lang.Number], """{"a":5}""", None),
          (java.time.Duration.ofHours(5), """{"a":"PT5H"}""", Some((c: Any) => c.toString)),
          (java.time.Instant.ofEpochSecond(1234567), """{"a":"1970-01-15T06:56:07Z"}""", Some((c: Any) => c.toString)),
          (java.time.LocalDate.of(2024, 3, 15), """{"a":"2024-03-15"}""", Some((c: Any) => c.toString)),
          (java.time.LocalDateTime.of(2024, 3, 15, 4, 15, 3), """{"a":"2024-03-15T04:15:03"}""", Some((c: Any) => c.toString)),
          (java.time.LocalTime.of(4, 15, 3), """{"a":"04:15:03"}""", Some((c: Any) => c.toString)),
          (java.time.MonthDay.of(12, 25), """{"a":"--12-25"}""", Some((c: Any) => c.toString)),
          (java.time.OffsetDateTime.of(2024, 3, 15, 9, 15, 1, 0, java.time.ZoneOffset.ofHours(5)), """{"a":"2024-03-15T09:15:01+05:00"}""", Some((c: Any) => c.toString)),
          (java.time.OffsetTime.of(9, 15, 1, 0, java.time.ZoneOffset.ofHours(5)), """{"a":"09:15:01+05:00"}""", Some((c: Any) => c.toString)),
          (java.time.Period.ofDays(5), """{"a":"P5D"}""", Some((c: Any) => c.toString)),
          (java.time.Year.of(2024), """{"a":"2024"}""", Some((c: Any) => c.toString)),
          (java.time.YearMonth.of(2024, 3), """{"a":"2024-03"}""", Some((c: Any) => c.toString)),
          (java.time.ZoneOffset.ofHours(5), """{"a":"+05:00"}""", Some((c: Any) => c.toString)),
          (java.time.ZonedDateTime.parse("2007-12-03T10:15:30+01:00"), """{"a":"2007-12-03T10:15:30+01:00"}""", Some((c: Any) => c.toString)),
          (java.time.ZoneId.of("GMT+2"), """{"a":"GMT+02:00"}""", Some((c: Any) => c.toString))
        )
        prims.map { case (v, j, fn) =>
          val inst = AnyShell(v)
          val js = sj.toJson(inst)
          js should matchJson(j)
          fn match {
            case Some(f) => sj.fromJson(js) shouldEqual (AnyShell(f(v)))
            case None    => sj.fromJson(js) shouldEqual inst
          }
        }
      }
    }

    // --------------------------------------------------------

    describe(colorString("--- Negative Tests ---")) {
      it("BigDecimal must break") {
        val js =
          """{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.1499999999999999944488848768742172978818416595458984375","bd6":null}"""
        val msg: String =
          """Expected a numerical value or null here at position [50]
                  |{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.149999999999999994448884...
                  |--------------------------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleBigDecimal].fromJson(js))
        ex.show shouldEqual msg
      }

      it("BigInt must break") {
        val js =
          """{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4":null}"""
        val msg =
          """Expected a numerical value or null here at position [7]
                  |{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4"...
                  |-------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleBigInt].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":"false"}"""
        val sj = sjCodecOf[SampleBoolean]
        val msg = """Expected 'true' or 'false' at position [22]
                  |{"bool1":true,"bool2":"false"}
                  |----------------------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"bool1":true,"bool2":123}"""
        val msg2 = """Expected 'true' or 'false' at position [22]
                    |{"bool1":true,"bool2":123}
                    |----------------------^""".stripMargin
        val ex2 = intercept[JsonParseError](sj.fromJson(js2))
        ex2.show shouldEqual msg2
        val js3 = """{"bool1":true,"bool2":null}"""
        val msg3 = """Expected 'true' or 'false' at position [22]
                    |{"bool1":true,"bool2":null}
                    |----------------------^""".stripMargin
        val ex3 = intercept[JsonParseError](sj.fromJson(js3))
        ex3.show shouldEqual msg3
      }

      it("Byte must break") {
        val js = """{"b1":true,"b2":-128,"b3":0,"b4":64}"""
        val sj = sjCodecOf[SampleByte]
        val msg = """Non-numeric character found when integer value expected at position [6]
                  |{"b1":true,"b2":-128,"b3":0,"b4":64}
                  |------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"b1":12,"b2":-128,"b3":0,"b4":null}"""
        val msg2 = """Non-numeric character found when integer value expected at position [31]
                  |{"b1":12,"b2":-128,"b3":0,"b4":null}
                  |-------------------------------^""".stripMargin
        val ex2 = intercept[JsonParseError](sj.fromJson(js2))
        ex2.show shouldEqual msg2
      }

      it("Char must break") {
        val js = """{"c1":null,"c2":"Y","c3":"Z"}"""
        val sj = sjCodecOf[SampleChar]
        val msg = """Char value cannot be null at position [6]
                |{"c1":null,"c2":"Y","c3":"Z"}
                |------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"c1":"","c2":"Y","c3":"Z"}"""
        val msg2 = """Char value expected but empty string found in json at position [6]
                |{"c1":"","c2":"Y","c3":"Z"}
                |------^""".stripMargin
        val ex2 = intercept[JsonParseError](sj.fromJson(js2))
        ex2.show shouldEqual msg2
      }

      it("Double must break") {
        val js =
          """{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}"""
        val msg =
          """Expected ',' or '}' but found 'E' at position [25]
                  |{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123...
                  |-------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleDouble].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Float must break") {
        val js =
          """{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}"""
        val msg =
          """Malformed Float/Double/BigDecimal at position [24]
                  |{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}
                  |------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleFloat].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Int must break") {
        val sj = sjCodecOf[SampleInt]
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}"""
        val msg = """Non-numeric character found when integer value expected at position [39]
                  |{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}
                  |---------------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}"""
        val msg2 = """Decimal digit 'e' or '.' found when integer value expected at position [40]
                    |{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}
                    |----------------------------------------^""".stripMargin
        val ex2 = intercept[JsonParseError](sj.fromJson(js2))
        ex2.show shouldEqual msg2
      }

      it("Long must break") {
        val sj = sjCodecOf[SampleLong]
        val js =
          """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}"""
        val msg =
          """Unexpected character in Int/Long value: t at position [57]
                  |...23372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}
                  |----------------------------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 =
          """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}"""
        val msg2 =
          """Expected ',' or '}' but found '.' at position [58]
            |...3372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}
            |----------------------------------------------------^""".stripMargin
        val ex2 = intercept[JsonParseError](sj.fromJson(js2))
        ex2.show shouldEqual msg2
      }

      it("Short must break") {
        val sj = sjCodecOf[SampleShort]
        val js = """{"s1":32767,"s2":true,"s3":0,"s4":123}"""
        val msg = """Non-numeric character found when integer value expected at position [17]
                  |{"s1":32767,"s2":true,"s3":0,"s4":123}
                  |-----------------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"s1":32767,"s2":3.4,"s3":0,"s4":123}"""
        val msg2 = """Decimal digit 'e' or '.' found when integer value expected at position [18]
                    |{"s1":32767,"s2":3.4,"s3":0,"s4":123}
                    |------------------^""".stripMargin
        val ex2 = intercept[JsonParseError](sj.fromJson(js2))
        ex2.show shouldEqual msg2
      }

      it("String must break") {
        val js = """{"s1":"something","s2":-19,"s3":null}"""
        val msg = """Expected a String value but got '-' at position [23]
                  |{"s1":"something","s2":-19,"s3":null}
                  |-----------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleString].fromJson(js))
        ex.show shouldEqual msg
      }
    }
  }
