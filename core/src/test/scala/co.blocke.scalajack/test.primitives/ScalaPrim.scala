package co.blocke.scalajack
package test
package primitives

import org.scalatest.{ FunSpec, Matchers }
import scala.math.BigDecimal

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
        assertResult("""{"c1":"￿","c2":"Z","c3":"₠"}""") { js }
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
        val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null)
        val js = sj.render(inst)
        assertResult("""{"e1":"Small","e2":"Medium","e3":"Large","e4":null}""") { js }
        assertResult(inst) {
          sj.read[SampleEnum](js)
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
        val msg = """Expected value token of type Number, not String when reading BigDecimal value.  (Is your value wrapped in quotes?)
          |{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.14999999999999999444888487687421729788184165954
          |--------------------------------------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleBigDecimal](js) should have message msg
      }
      it("BigInt must break") {
        val js = """{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4":null}"""
        val msg = """Expected value token of type Number, not String when reading BigInt value.  (Is your value wrapped in quotes?)
          |{"bi1":"-90182736451928374653345","bi2":90182736451928374
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleBigInt](js) should have message msg
      }
      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":"false"}"""
        val msg = """Expected value token of type True or False, not String when reading Boolean value.  (Is your value wrapped in quotes or a number?)
          |{"bool1":true,"bool2":"false"}
          |----------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleBoolean](js) should have message msg
        val js2 = """{"bool1":true,"bool2":123}"""
        val msg2 = """Expected value token of type True or False, not Number when reading Boolean value.  (Is your value wrapped in quotes or a number?)
          |{"bool1":true,"bool2":123}
          |----------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleBoolean](js2) should have message msg2
        val js3 = """{"bool1":true,"bool2":null}"""
        val msg3 = """Expected token of type Boolean, not Null
          |{"bool1":true,"bool2":null}
          |--------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleBoolean](js3) should have message msg3
      }
      it("Byte must break") {
        val js = """{"b1":927,"b2":-128,"b3":0,"b4":64}"""
        val msg = """Value out of range. Value:"927" Radix:10
          |{"b1":927,"b2":-128,"b3":0,"b4":64}
          |------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleByte](js) should have message msg
        val js2 = """{"b1":true,"b2":-128,"b3":0,"b4":64}"""
        val msg2 = """Expected token of type Number, not True
          |{"b1":true,"b2":-128,"b3":0,"b4":64}
          |------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleByte](js2) should have message msg2
      }
      it("Char must break") {
        val js = """{"c1":null,"c2":"Y","c3":"Z"}"""
        val msg = """Expected token of type String, not Null
          |{"c1":null,"c2":"Y","c3":"Z"}
          |------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleChar](js) should have message msg
      }
      it("Double must break") {
        val js = """{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}"""
        val msg = """For input string: "1.79769313486E23157E308"
          |{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E3
          |------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleDouble](js) should have message msg
      }
      it("Enumeration must break") {
        val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null)
        val js = """{"e1":"Small","e2":9,"e3":"Large","e4":null}"""
        val msg = """Expected value token of type String, not Number when reading Enumeration value.
          |{"e1":"Small","e2":9,"e3":"Large","e4":null}
          |-------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleEnum](js) should have message msg
        val js2 = """{"e1":"Small","e2":"Bogus","e3":"Large","e4":null}"""
        val msg2 = """No value found in enumeration co.blocke.scalajack.test.primitives.Size$ for "Bogus"
          |{"e1":"Small","e2":"Bogus","e3":"Large","e4":null}
          |-------------------^""".stripMargin
        the[java.util.NoSuchElementException] thrownBy sj.read[SampleEnum](js2) should have message msg2
      }
      it("Float must break") {
        val js = """{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}"""
        val msg = """Expected token of type Number, not String
          |{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}
          |------------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleFloat](js) should have message msg
      }
      it("Int must break") {
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}"""
        val msg = """Expected token of type Number, not String
          |{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}
          |---------------------------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleInt](js) should have message msg
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}"""
        val msg2 = """For input string: "2.3"
          |{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}
          |---------------------------------------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleInt](js2) should have message msg2
      }
      it("Long must break") {
        val js = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}"""
        val msg = """Expected token of type Number, not True
          |223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}
          |--------------------------------------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleLong](js) should have message msg
        val js2 = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}"""
        val msg2 = """For input string: "0.3"
          |223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}
          |--------------------------------------------------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleLong](js2) should have message msg2
      }
      it("Short must break") {
        val js = """{"s1":32767,"s2":true,"s3":0,"s4":123}"""
        val msg = """Expected token of type Number, not True
          |{"s1":32767,"s2":true,"s3":0,"s4":123}
          |-----------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleShort](js) should have message msg
        val js2 = """{"s1":32767,"s2":3.4,"s3":0,"s4":123}"""
        val msg2 = """For input string: "3.4"
          |{"s1":32767,"s2":3.4,"s3":0,"s4":123}
          |-----------------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleShort](js2) should have message msg2
      }
      it("String must break") {
        val js = """{"s1":"something","s2":-19,"s3":null}"""
        val msg = """Expected value token of type String, not Number when reading String value.
          |{"s1":"something","s2":-19,"s3":null}
          |-----------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleString](js) should have message msg
      }
      it("Can't find TypeAdapter for given type") {
        val js = """{"hey":"you"}"""
        val msg = """Cannot find a type adapter for java.lang.Process"""
        the[java.lang.IllegalArgumentException] thrownBy sj.read[java.lang.Process](js) should have message msg
      }
    }
  }
}
