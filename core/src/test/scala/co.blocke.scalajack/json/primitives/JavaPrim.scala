package co.blocke.scalajack
package json.primitives

import org.scalatest.{ FunSpec, Matchers }
import java.lang.{ Boolean => JBoolean, Byte => JByte, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }

class JavaPrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("--------------------------\n:  Java Primitive Tests  :\n--------------------------") {
    describe("+++ Positive Tests +++") {
      it("BigDecimal must work") {
        val inst = SampleJBigDecimal(JBigDecimal.ZERO, JBigDecimal.ONE, JBigDecimal.TEN, new JBigDecimal("0.1499999999999999944488848768742172978818416595458984375"), null)
        val js = sj.render(inst)
        assertResult("""{"bd1":0,"bd2":1,"bd3":10,"bd4":0.1499999999999999944488848768742172978818416595458984375,"bd5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJBigDecimal](js)
        }
      }
      it("BigInteger must work") {
        val inst = SampleJBigInteger(JBigInteger.ZERO, JBigInteger.ONE, JBigInteger.TEN, new JBigInteger("-90182736451928374653345"), new JBigInteger("90182736451928374653345"), new JBigInteger("0"), null)
        val js = sj.render(inst)
        assertResult("""{"bi1":0,"bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":90182736451928374653345,"bi6":0,"bi7":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJBigInteger](js)
        }
      }
      it("Boolean must work") {
        val inst = SampleJBoolean(JBoolean.TRUE, JBoolean.FALSE, true, false, null)
        val js = sj.render(inst)
        assertResult("""{"bool1":true,"bool2":false,"bool3":true,"bool4":false,"bool5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJBoolean](js)
        }
      }
      it("Byte must work") {
        val inst = SampleJByte(JByte.MAX_VALUE, JByte.MIN_VALUE, 0.asInstanceOf[Byte], 64.asInstanceOf[Byte], null)
        val js = sj.render(inst)
        assertResult("""{"b1":127,"b2":-128,"b3":0,"b4":64,"b5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJByte](js)
        }
      }
      it("Char must work") {
        val inst = SampleJChar('Z', '\u20A0', null)
        val js = sj.render(inst)
        assertResult("""{"c1":"Z","c2":"\""" + """u20a0","c3":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJChar](js)
        }
      }
      it("Double must work") {
        val inst = SampleJDouble(JDouble.MAX_VALUE, JDouble.MIN_VALUE, 0.0, -123.4567, null)
        val js = sj.render(inst)
        assertResult("""{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":0.0,"d4":-123.4567,"d5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJDouble](js)
        }
      }
      it("Float must work") {
        val inst = SampleJFloat(JFloat.MAX_VALUE, JFloat.MIN_VALUE, 0.0F, -123.4567F, null)
        val js = sj.render(inst)
        assertResult("""{"f1":3.4028235E38,"f2":1.4E-45,"f3":0.0,"f4":-123.4567,"f5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJFloat](js)
        }
      }
      it("Int must work") {
        val inst = SampleJInt(JInt.MAX_VALUE, JInt.MIN_VALUE, 0, 123, null)
        val js = sj.render(inst)
        assertResult("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123,"i5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJInt](js)
        }
      }
      it("Long must work") {
        val inst = SampleJLong(JLong.MAX_VALUE, JLong.MIN_VALUE, 0L, 123L, null)
        val js = sj.render(inst)
        assertResult("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123,"l5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJLong](js)
        }
      }
      it("Number must work") {
        val inst = SampleJNumber(
          JByte.valueOf("-128"), JByte.valueOf("127"),
          JShort.valueOf("-32768"), JShort.valueOf("32767"),
          JInt.valueOf("-2147483648"), JInt.valueOf("2147483647"),
          JLong.valueOf("-9223372036854775808"), JLong.valueOf("9223372036854755807"),
          null, //new JBigInteger("9923372036854755810"),
          JByte.valueOf("0"),
          JFloat.valueOf("3.4e-038"), JFloat.valueOf("3.4e+038"),
          JDouble.valueOf("1.7e-308"), JDouble.valueOf("1.7e+308"),
          null, //new JBigDecimal("1.8e+308"),
          JFloat.valueOf("0.0"),
          null)
        val js = sj.render(inst)
        assertResult("""{"n1":-128,"n2":127,"n3":-32768,"n4":32767,"n5":-2147483648,"n6":2147483647,"n7":-9223372036854775808,"n8":9223372036854755807,"n9":null,"n10":0,"n11":3.4E-38,"n12":3.4E38,"n13":1.7E-308,"n14":1.7E308,"n15":null,"n16":0.0,"n17":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJNumber](js)
        }
      }
      it("Short must work") {
        val inst = SampleJShort(JShort.MAX_VALUE, JShort.MIN_VALUE, 0.asInstanceOf[Short], 123.asInstanceOf[Short], null)
        val js = sj.render(inst)
        assertResult("""{"s1":32767,"s2":-32768,"s3":0,"s4":123,"s5":null}""") { js }
        assertResult(inst) {
          sj.read[SampleJShort](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("BigDecimal must break") {
        val js = """{"bd1":0,"bd2":1,"bd3":10,"bd4":"0.1499999999999999944488848768742172978818416595458984375","bd5":null}"""
        val msg = """[$.bd4]: Expected Number here but found String
                    |...99999999999944488848768742172978818416595458984375","bd5":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleBigDecimal](js) should have message msg
      }
      it("BigInteger must break") {
        val js = """{"bi1":"0","bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":90182736451928374653345,"bi6":0,"bi7":null}"""
        val msg = """[$.bi1]: Expected Number here but found String
                    |{"bi1":"0","bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":901827364519...
                    |--------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJBigInteger](js) should have message msg
      }
      it("Boolean must break") {
        val js = """{"bool1":true,"bool2":false,"bool3":true,"bool4":"false","bool5":null}"""
        val msg = """[$.bool4]: Expected Boolean here but found String
                    |...l1":true,"bool2":false,"bool3":true,"bool4":"false","bool5":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJBoolean](js) should have message msg
      }
      it("Byte must break") {
        val js = """{"b1":127,"b2":-128,"b3":false,"b4":64,"b5":null}"""
        val msg = """[$.b3]: Expected Number here but found Boolean
                    |{"b1":127,"b2":-128,"b3":false,"b4":64,"b5":null}
                    |-----------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJByte](js) should have message msg
      }
      it("Char must break") {
        val js = """{"c1":"Z","c2":3,"c3":null}"""
        val msg = """[$.c2]: Expected String here but found Number
                    |{"c1":"Z","c2":3,"c3":null}
                    |---------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJChar](js) should have message msg
        val js2 = """{"c1":"Z","c2":"","c3":null}"""
        val msg2 = """[$.c2]: Tried to read a Character but empty string found
                    |{"c1":"Z","c2":"","c3":null}
                    |---------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleJChar](js2) should have message msg2
      }
      it("Double must break") {
        val js = """{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":"0.0","d4":-123.4567,"d5":null}"""
        val msg = """[$.d3]: Expected Number here but found String
                    |...d1":1.7976931348623157E308,"d2":4.9E-324,"d3":"0.0","d4":-123.4567,"d5":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJDouble](js) should have message msg
      }
      it("Float must break") {
        val js = """{"f1":3.4028235E38,"f2":"1.4E-45","f3":0.0,"f4":-123.4567,"f5":null}"""
        val msg = """[$.f2]: Expected Number here but found String
                    |{"f1":3.4028235E38,"f2":"1.4E-45","f3":0.0,"f4":-123.4567,"f5":null}
                    |-------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJFloat](js) should have message msg
      }
      it("Int must break") {
        val js = """{"i1":2147483647,"i2":-2147483648,"i3":false,"i4":123,"i5":null}"""
        val msg = """[$.i3]: Expected Number here but found Boolean
                    |{"i1":2147483647,"i2":-2147483648,"i3":false,"i4":123,"i5":null}
                    |-------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJInt](js) should have message msg
        val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":0.3,"i4":123,"i5":null}"""
        val msg2 = """[$.i3]: Unable to read value (e.g. bad number format)
                    |{"i1":2147483647,"i2":-2147483648,"i3":0.3,"i4":123,"i5":null}
                    |-----------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleJInt](js2) should have message msg2
      }
      it("Long must break") {
        val js = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":"0","l4":123,"l5":null}"""
        val msg = """[$.l3]: Expected Number here but found String
                    |...3372036854775807,"l2":-9223372036854775808,"l3":"0","l4":123,"l5":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJLong](js) should have message msg
        val js2 = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123,"l5":null}"""
        val msg2 = """[$.l3]: Unable to read value (e.g. bad number format)
                    |...372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123,"l5":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleJLong](js2) should have message msg2
      }
      it("Number must break") {
        val js = """{"n1":-128,"n2":127,"n3":"-32768","n4":32767,"n5":-2147483648,"n6":2147483647,"n7":-9223372036854775808,"n8":9223372036854755807,"n9":9923372036854755810,"n10":0,"n11":3.4E-38,"n12":3.4E38,"n13":1.7E-308,"n14":1.7E308,"n15":1.8E+308,"n16":0.0,"n17":null}"""
        val msg = """[$.n3]: Expected Number here but found String
                    |{"n1":-128,"n2":127,"n3":"-32768","n4":32767,"n5":-2147483648,"n6":2147483647...
                    |-------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJNumber](js) should have message msg
      }
      it("Short must break") {
        val js = """{"s1":false,"s2":-32768,"s3":0,"s4":123,"s5":null}"""
        val msg = """[$.s1]: Expected Number here but found Boolean
                    |{"s1":false,"s2":-32768,"s3":0,"s4":123,"s5":null}
                    |----------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJShort](js) should have message msg
        val js2 = """{"s1":2.3,"s2":-32768,"s3":0,"s4":123,"s5":null}"""
        val msg2 = """[$.s1]: Unable to read value (e.g. bad number format)
                    |{"s1":2.3,"s2":-32768,"s3":0,"s4":123,"s5":null}
                    |--------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleJShort](js2) should have message msg2
      }
    }
  }
}
