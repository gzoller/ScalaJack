package co.blocke.scalajack
package json.test.primitives

import org.scalatest.{ FunSpec, Matchers }

class ScalaPrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("----------------------------\n:  Scala Primitives Tests  :\n----------------------------") {
    describe("+++ Positive Tests +++") {
      it("BigDecimal must work") {
        val inst = SampleBigDecimal(BigDecimal(123L), BigDecimal(1.23), BigDecimal(0), BigDecimal("123.456"), BigDecimal("0.1499999999999999944488848768742172978818416595458984375"), null)
        val js = sj.render(inst)
        assertResult("""{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":0.1499999999999999944488848768742172978818416595458984375,"bd6":null}""") { js }
        assertResult(inst) {
          sj.read[SampleBigDecimal](js)
        }
      }
      it("BigInt must work") {
        val inst = SampleBigInt(BigInt("-90182736451928374653345"), BigInt("90182736451928374653345"), BigInt(0), null)
        val js = sj.render(inst)
        assertResult("""{"bi1":-90182736451928374653345,"bi2":90182736451928374653345,"bi3":0,"bi4":null}""") { js }
        assertResult(inst) {
          sj.read[SampleBigInt](js)
        }
      }
      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(true, false)
        val js = sj.render(inst)
        assertResult("""{"bool1":true,"bool2":false}""") { js }
        assertResult(inst) {
          sj.read[SampleBoolean](js)
        }
      }
      it("Byte must work (not nullable)") {
        val inst = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
        val js = sj.render(inst)
        assertResult("""{"b1":127,"b2":-128,"b3":0,"b4":64}""") { js }
        assertResult(inst) {
          sj.read[SampleByte](js)
        }
      }
      it("Char must work (not nullable)") {
        val inst = SampleChar(Char.MaxValue, 'Z', '\u20A0')
        val js = sj.render(inst)
        assertResult("""{"c1":"\""" + """uFFFF","c2":"Z","c3":"\""" + """u20A0"}""") { js }
        assertResult(inst) {
          sj.read[SampleChar](js)
        }
      }
      it("Double must work (not nullable)") {
        val inst = SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val js = sj.render(inst)
        assertResult("""{"d1":1.7976931348623157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}""") { js }
        assertResult(inst) {
          sj.read[SampleDouble](js)
        }
      }
      it("Enumeration must work (not nullable)") {
        val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
        val js = sj.render(inst)
        assertResult("""{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":"Medium"}""") { js }
        // mutate e5 into an ordinal...
        val js2 = js.replaceAll(""""e5":"Medium"""", """"e5":1""")
        assertResult(inst) {
          sj.read[SampleEnum](js2)
        }
      }
      it("Float must work") {
        val inst = SampleFloat(Float.MaxValue, Float.MinValue, 0.0F, -123.4567F)
        val js = sj.render(inst)
        assertResult("""{"f1":3.4028235E38,"f2":-3.4028235E38,"f3":0.0,"f4":-123.4567}""") { js }
        assertResult(inst) {
          sj.read[SampleFloat](js)
        }
      }
      it("Int must work (not nullable)") {
        val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val js = sj.render(inst)
        assertResult("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123}""") { js }
        assertResult(inst) {
          sj.read[SampleInt](js)
        }
      }
      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val js = sj.render(inst)
        assertResult("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123}""") { js }
        assertResult(inst) {
          sj.read[SampleLong](js)
        }
      }
      it("Short must work (not nullable)") {
        val inst = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
        val js = sj.render(inst)
        assertResult("""{"s1":32767,"s2":-32768,"s3":0,"s4":123}""") { js }
        assertResult(inst) {
          sj.read[SampleShort](js)
        }
      }
      it("String must work") {
        val inst = SampleString("something\b\n\f\r\t☆", "", null)
        val js = sj.render(inst)
        // The weird '+' here is to break up the unicode so it won't be interpreted and wreck the test.
        assertResult("""{"s1":"something\b\n\f\r\t\""" + """u2606","s2":"","s3":null}""") { js }
        assertResult(inst) {
          sj.read[SampleString](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("BigDecimal must break") {
        val js = """{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.1499999999999999944488848768742172978818416595458984375","bd6":null}"""
        val msg = """DeserializationException(1 error):
                    |  [$.bd5] Expected a JSON number, not JString(0.1499999999999999944488848768742172978818416595458984375) (reported by: co.blocke.scalajack.typeadapter.BigDecimalDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBigDecimal](js) should have message msg
      }
      it("BigInt must break") {
        val js = """{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4":null}"""
        val msg = """DeserializationException(1 error):
                    |  [$.bi1] Expected a JSON number (integer value) (reported by: co.blocke.scalajack.typeadapter.BigIntDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBigInt](js) should have message msg
      }
      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":"false"}"""
        val msg = """DeserializationException(1 error):
                    |  [$.bool2] Expected a JSON boolean (reported by: co.blocke.scalajack.typeadapter.BooleanDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBoolean](js) should have message msg
        val js2 = """{"bool1":true,"bool2":123}"""
        val msg2 = """DeserializationException(1 error):
                     |  [$.bool2] Expected a JSON boolean (reported by: co.blocke.scalajack.typeadapter.BooleanDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBoolean](js2) should have message msg2
        val js3 = """{"bool1":true,"bool2":null}"""
        val msg3 = """DeserializationException(1 error):
                     |  [$.bool2] Expected a JSON boolean (reported by: co.blocke.scalajack.typeadapter.BooleanDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleBoolean](js3) should have message msg3
      }
      it("Byte must break") {
        val js = """{"b1":927,"b2":-128,"b3":0,"b4":64}"""
        val msg = """DeserializationException(1 error):
                    |  [$.b1] Byte value out of range (reported by: co.blocke.scalajack.typeadapter.ByteDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleByte](js) should have message msg
        val js2 = """{"b1":true,"b2":-128,"b3":0,"b4":64}"""
        val msg2 = """DeserializationException(1 error):
                     |  [$.b1] Expected a JSON number (byte) (reported by: co.blocke.scalajack.typeadapter.ByteDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleByte](js2) should have message msg2
      }
      it("Char must break") {
        val js = """{"c1":null,"c2":"Y","c3":"Z"}"""
        val msg = """DeserializationException(1 error):
                    |  [$.c1] Expected a char (JSON string of length 1) (reported by: co.blocke.scalajack.typeadapter.CharDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleChar](js) should have message msg
      }
      it("Double must break") {
        val js = """{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}"""
        val msg = """DeserializationException(1 error):
                    |  [???] Exception was thrown: java.lang.NumberFormatException (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleDouble](js) should have message msg
      }
      it("Enumeration must break") {
        val js = """{"e1":"Small","e2":"Bogus","e3":"Large","e4":null,"e5":"Medium"}"""
        val msg = """DeserializationException(1 error):
                    |  [$.e2] Enumeration co.blocke.scalajack.json.test.primitives.Size$ does not contain a value named Bogus (reported by: co.blocke.scalajack.typeadapter.EnumerationValueDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleEnum](js) should have message msg
        val js2 = """{"e1":"Small","e2":"Medium","e3":"Large","e4":null,"e5":9}"""
        val msg2 = """DeserializationException(1 error):
                     |  [$.e5] Enumeration co.blocke.scalajack.json.test.primitives.Size$ does not contain a value at index 9 (reported by: co.blocke.scalajack.typeadapter.EnumerationValueDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleEnum](js2) should have message msg2
      }
      it("Float must break") {
        val js = """{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}"""
        val msg = """DeserializationException(1 error):
                    |  [$.f2] Expected a JSON number (reported by: co.blocke.scalajack.typeadapter.FloatDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleFloat](js) should have message msg
      }
      it("Int must break") {
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}"""
        val msg = """DeserializationException(1 error):
                    |  [$.i3] Expected a JSON int, not JString(0) (reported by: co.blocke.scalajack.typeadapter.IntDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleInt](js) should have message msg
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}"""
        val msg2 = """DeserializationException(1 error):
                     |  [$.i3] Expected a JSON int, not JDecimal(2.3) (reported by: co.blocke.scalajack.typeadapter.IntDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleInt](js2) should have message msg2
      }
      it("Long must break") {
        val js = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}"""
        val msg = """DeserializationException(1 error):
                    |  [$.l3] Expected a JSON number (long) (reported by: co.blocke.scalajack.typeadapter.LongDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleLong](js) should have message msg
        val js2 = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}"""
        val msg2 = """DeserializationException(1 error):
                     |  [$.l3] Expected a JSON number (long) (reported by: co.blocke.scalajack.typeadapter.LongDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleLong](js2) should have message msg2
      }
      it("Short must break") {
        val js = """{"s1":32767,"s2":true,"s3":0,"s4":123}"""
        val msg = """DeserializationException(1 error):
                    |  [$.s2] Expected a JSON number (short), not JBool(true) (reported by: co.blocke.scalajack.typeadapter.ShortDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleShort](js) should have message msg
        val js2 = """{"s1":32767,"s2":3.4,"s3":0,"s4":123}"""
        val msg2 = """DeserializationException(1 error):
                     |  [$.s2] Expected a JSON number (short), not JDecimal(3.4) (reported by: co.blocke.scalajack.typeadapter.ShortDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleShort](js2) should have message msg2
      }
      it("String must break") {
        val js = """{"s1":"something","s2":-19,"s3":null}"""
        val msg = """DeserializationException(1 error):
                    |  [$.s2] Expected a JSON string (reported by: co.blocke.scalajack.typeadapter.StringDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleString](js) should have message msg
      }
    }
  }
}
