package co.blocke.scalajack
package json
package primitives

import ScalaJack.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

import java.lang.{Boolean as JBoolean, Byte as JByte, Double as JDouble, Float as JFloat, Integer as JInt, Long as JLong, Short as JShort}
import java.math.{BigDecimal as JBigDecimal, BigInteger as JBigInteger}

class JavaPrimSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("--------------------------\n:  Java Primitive Tests  :\n--------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("BigDecimal must work") {
        val inst = SampleJBigDecimal(
          JBigDecimal.ZERO,
          JBigDecimal.ONE,
          JBigDecimal.TEN,
          new JBigDecimal(
            "0.1499999999999999944488848768742172978818416595458984375"
          ),
          null
        )
        val js = sjCodecOf[SampleJBigDecimal].toJson(inst)
        js should matchJson("""{"bd1":0,"bd2":1,"bd3":10,"bd4":0.1499999999999999944488848768742172978818416595458984375,"bd5":null}""")
        sjCodecOf[SampleJBigDecimal].fromJson(js) shouldEqual inst
      }

      it("BigInteger must work") {
        val inst = SampleJBigInteger(
          JBigInteger.ZERO,
          JBigInteger.ONE,
          JBigInteger.TEN,
          new JBigInteger("-90182736451928374653345"),
          new JBigInteger("90182736451928374653345"),
          new JBigInteger("0"),
          null
        )
        val js = sjCodecOf[SampleJBigInteger].toJson(inst)
        js should matchJson("""{"bi1":0,"bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":90182736451928374653345,"bi6":0,"bi7":null}""")
        sjCodecOf[SampleJBigInteger].fromJson(js) shouldEqual inst
      }

      it("Boolean must work") {
        val inst = SampleJBoolean(JBoolean.TRUE, JBoolean.FALSE, true, false, null)
        val js = sjCodecOf[SampleJBoolean].toJson(inst)
        js should matchJson("""{"bool1":true,"bool2":false,"bool3":true,"bool4":false,"bool5":null}""")
        sjCodecOf[SampleJBoolean].fromJson(js) shouldEqual inst
      }

      it("Byte must work") {
        val inst = SampleJByte(
          JByte.MAX_VALUE,
          JByte.MIN_VALUE,
          0.asInstanceOf[Byte],
          64.asInstanceOf[Byte],
          null
        )
        val js = sjCodecOf[SampleJByte].toJson(inst)
        js should matchJson("""{"b1":127,"b2":-128,"b3":0,"b4":64,"b5":null}""")
        sjCodecOf[SampleJByte].fromJson(js) shouldEqual inst
      }

      it("Character must work") {
        val inst = SampleJChar('Z', '\u20A0', null)
        val js = sjCodecOf[SampleJChar].toJson(inst)
        js should matchJson("""{"c1":"Z","c2":"\""" + """u20a0","c3":null}""")
        sjCodecOf[SampleJChar].fromJson(js) shouldEqual inst
      }

      it("Double must work") {
        val inst = SampleJDouble(
          JDouble.MAX_VALUE,
          JDouble.MIN_VALUE,
          0.0,
          -123.4567,
          null
        )
        val js = sjCodecOf[SampleJDouble].toJson(inst)
        js should matchJson("""{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":0.0,"d4":-123.4567,"d5":null}""")
        sjCodecOf[SampleJDouble].fromJson(js) shouldEqual inst
      }

      it("Float must work") {
        val inst = SampleJFloat(
          JFloat.MAX_VALUE,
          JFloat.MIN_VALUE,
          0.0f,
          -123.4567f,
          null
        )
        val js = sjCodecOf[SampleJFloat].toJson(inst)
        js should matchJson("""{"f1":3.4028235E38,"f2":1.4E-45,"f3":0.0,"f4":-123.4567,"f5":null}""")
        sjCodecOf[SampleJFloat].fromJson(js) shouldEqual inst
      }

      it("Integer must work") {
        val inst = SampleJInt(JInt.MAX_VALUE, JInt.MIN_VALUE, 0, 123, null)
        val js = sjCodecOf[SampleJInt].toJson(inst)
        js should matchJson("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123,"i5":null}""")
        sjCodecOf[SampleJInt].fromJson(js) shouldEqual inst
      }

      it("Long must work") {
        val inst = SampleJLong(JLong.MAX_VALUE, JLong.MIN_VALUE, 0L, 123L, null)
        val js = sjCodecOf[SampleJLong].toJson(inst)
        js should matchJson("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123,"l5":null}""")
        sjCodecOf[SampleJLong].fromJson(js) shouldEqual inst
      }

      it("Number must work") {
        val inst = SampleJNumber(
          JByte.valueOf("-128"),
          JByte.valueOf("127"),
          JShort.valueOf("-32768"),
          JShort.valueOf("32767"),
          JInt.valueOf("-2147483648"),
          JInt.valueOf("2147483647"),
          JLong.valueOf("-9223372036854775808"),
          JLong.valueOf("9223372036854755807"),
          null, // new JBigInteger("9923372036854755810"),
          JByte.valueOf("0"),
          JFloat.valueOf("3.4e-038"),
          JFloat.valueOf("3.4e+038"),
          JDouble.valueOf("1.7e-308"),
          JDouble.valueOf("1.7e+308"),
          null, // new JBigDecimal("1.8e+308"),
          JFloat.valueOf("0.0"),
          null
        )
        val js = sjCodecOf[SampleJNumber].toJson(inst)
        js should matchJson(
          """{"n1":-128,"n2":127,"n3":-32768,"n4":32767,"n5":-2147483648,"n6":2147483647,"n7":-9223372036854775808,"n8":9223372036854755807,"n9":null,"n10":0,"n11":3.4E-38,"n12":3.4E38,"n13":1.7E-308,"n14":1.7E308,"n15":null,"n16":0.0,"n17":null}"""
        )
        sjCodecOf[SampleJNumber].fromJson(js) shouldEqual inst
      }

      it("Short must work") {
        val inst = SampleJShort(
          JShort.MAX_VALUE,
          JShort.MIN_VALUE,
          0.asInstanceOf[Short],
          123.asInstanceOf[Short],
          null
        )
        val js = sjCodecOf[SampleJShort].toJson(inst)
        js should matchJson("""{"s1":32767,"s2":-32768,"s3":0,"s4":123,"s5":null}""")
        sjCodecOf[SampleJShort].fromJson(js) shouldEqual inst
      }
    }

    // --------------------------------------------------------

    describe(colorString("--- Negative Tests ---")) {
      it("BigDecimal must break") {
        val js =
          """{"bd1":0,"bd2":1,"bd3":10,"bd4":"0.1499999999999999944488848768742172978818416595458984375","bd5":null}"""
        val msg =
          """Expected a numerical value or null here at position [32]
              |{"bd1":0,"bd2":1,"bd3":10,"bd4":"0.149999999999999994448884876874217297881841...
              |--------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJBigDecimal].fromJson(js))
        ex.show shouldEqual msg
      }

      it("BigInt must break") {
        val js =
          """{"bi1":"0","bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":90182736451928374653345,"bi6":0,"bi7":null}"""
        val msg =
          """Expected a numerical value or null here at position [7]
                |{"bi1":"0","bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":901827364519...
                |-------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJBigInteger].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":false,"bool3":true,"bool4":"false","bool5":null}"""
        val msg =
          """Expected 'true', 'false', or null here at position [49]
                      |{"bool1":true,"bool2":false,"bool3":true,"bool4":"false","bool5":null}
                      |-------------------------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJBoolean].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Byte must break") {
        val js = """{"b1":127,"b2":-128,"b3":false,"b4":64,"b5":null}"""
        val msg = """Expected a numerical value or null here at position [25]
              |{"b1":127,"b2":-128,"b3":false,"b4":64,"b5":null}
              |-------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJByte].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Char must break") {
        val sj = sjCodecOf[SampleJChar]
        val js = """{"c1":"Z","c2":3,"c3":null}"""
        val msg = """Expected a String value but got '3' at position [15]
          |{"c1":"Z","c2":3,"c3":null}
          |---------------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"c1":"Z","c2":"","c3":null}"""
        val msg2 = """Character value expected but empty string found in json at position [15]
          |{"c1":"Z","c2":"","c3":null}
          |---------------^""".stripMargin
        val ex2 = intercept[JsonParseError](sj.fromJson(js2))
        ex2.show shouldEqual msg2
      }

      it("Double must break") {
        val js =
          """{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":"0.0","d4":-123.4567,"d5":null}"""
        val msg =
          """Expected a numerical value or null here at position [48]
                  |{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":"0.0","d4":-123.4567,"d5":null}
                  |------------------------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJDouble].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Float must break") {
        val js =
          """{"f1":3.4028235E38,"f2":"1.4E-45","f3":0.0,"f4":-123.4567,"f5":null}"""
        val msg =
          """Expected a numerical value or null here at position [24]
                  |{"f1":3.4028235E38,"f2":"1.4E-45","f3":0.0,"f4":-123.4567,"f5":null}
                  |------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJFloat].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Int must break") {
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":false,"i4":123,"i5":null}"""
        val msg =
          """Expected a numerical value or null here at position [39]
                  |{"i1":2147483647,"i2":-2147483648,"i3":false,"i4":123,"i5":null}
                  |---------------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJInt].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":0.3,"i4":123,"i5":null}"""
        the[java.lang.NumberFormatException] thrownBy sjCodecOf[SampleJInt].fromJson(js2) should have message "For input string: \"0.3\""
      }

      it("Long must break") {
        val js =
          """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":"0","l4":123,"l5":null}"""
        val msg =
          """Expected a numerical value or null here at position [57]
                  |...23372036854775807,"l2":-9223372036854775808,"l3":"0","l4":123,"l5":null}
                  |----------------------------------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJLong].fromJson(js))
        ex.show shouldEqual msg
        val js2 =
          """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123,"l5":null}"""
        the[java.lang.NumberFormatException] thrownBy sjCodecOf[SampleJLong].fromJson(js2) should have message "For input string: \"0.3\""
      }

      it("Number must break") {
        val js =
          """{"n1":-128,"n2":127,"n3":"-32768","n4":32767,"n5":-2147483648,"n6":2147483647,"n7":-9223372036854775808,"n8":9223372036854755807,"n9":9923372036854755810,"n10":0,"n11":3.4E-38,"n12":3.4E38,"n13":1.7E-308,"n14":1.7E308,"n15":1.8E+308,"n16":0.0,"n17":null}"""
        val msg =
          """Expected a numerical value or null here at position [25]
                    |{"n1":-128,"n2":127,"n3":"-32768","n4":32767,"n5":-2147483648,"n6":2147483647...
                    |-------------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[SampleJNumber].fromJson(js))
        ex.show shouldEqual msg
      }

      it("Short must break") {
        val sj = sjCodecOf[SampleJShort]
        val js = """{"s1":false,"s2":-32768,"s3":0,"s4":123,"s5":null}"""
        val msg = """Expected a numerical value or null here at position [6]
            |{"s1":false,"s2":-32768,"s3":0,"s4":123,"s5":null}
            |------^""".stripMargin
        val ex = intercept[JsonParseError](sj.fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"s1":2.3,"s2":-32768,"s3":0,"s4":123,"s5":null}"""
        the[java.lang.NumberFormatException] thrownBy sj.fromJson(js2) should have message "For input string: \"2.3\""
      }
    }
  }
