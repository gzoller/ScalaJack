package co.blocke.scalajack
package xml
package primitives

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import TestUtil.*

import java.lang.{Boolean as JBoolean, Byte as JByte, Double as JDouble, Float as JFloat, Integer as JInt, Long as JLong, Short as JShort}
import java.math.{BigDecimal as JBigDecimal, BigInteger as JBigInteger}

class JavaPrimSpec() extends AnyFunSpec:

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
        val x = sjXmlCodecOf[SampleJBigDecimal].toXml(inst)
        x should equal("""<SampleJBigDecimal><bd1>0</bd1><bd2>1</bd2><bd3>10</bd3><bd4>0.1499999999999999944488848768742172978818416595458984375</bd4><bd5>null</bd5></SampleJBigDecimal>""")
        sjXmlCodecOf[SampleJBigDecimal].fromXml(x) shouldEqual inst
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
        val x = sjXmlCodecOf[SampleJBigInteger].toXml(inst)
        x should equal("""<SampleJBigInteger><bi1>0</bi1><bi2>1</bi2><bi3>10</bi3><bi4>-90182736451928374653345</bi4><bi5>90182736451928374653345</bi5><bi6>0</bi6><bi7>null</bi7></SampleJBigInteger>""")
        sjXmlCodecOf[SampleJBigInteger].fromXml(x) shouldEqual inst
      }

      it("Boolean must work") {
        val inst = SampleJBoolean(JBoolean.TRUE, JBoolean.FALSE, true, false, null)
        val x = sjXmlCodecOf[SampleJBoolean].toXml(inst)
        x should equal("""<SampleJBoolean><bool1>true</bool1><bool2>false</bool2><bool3>true</bool3><bool4>false</bool4><bool5>null</bool5></SampleJBoolean>""")
        sjXmlCodecOf[SampleJBoolean].fromXml(x) shouldEqual inst
      }

      it("Byte must work") {
        val inst = SampleJByte(
          JByte.MAX_VALUE,
          JByte.MIN_VALUE,
          0.asInstanceOf[Byte],
          64.asInstanceOf[Byte],
          null
        )
        val x = sjXmlCodecOf[SampleJByte].toXml(inst)
        x should equal("""<SampleJByte><b1>127</b1><b2>-128</b2><b3>0</b3><b4>64</b4><b5>null</b5></SampleJByte>""")
        sjXmlCodecOf[SampleJByte].fromXml(x) shouldEqual inst
      }

      it("Character must work") {
        val inst = SampleJChar('Z', '\u20A0', null)
        val x = sjXmlCodecOf[SampleJChar].toXml(inst)
        x should equal("""<SampleJChar><c1>Z</c1><c2>₠</c2><c3>null</c3></SampleJChar>""")
        sjXmlCodecOf[SampleJChar].fromXml(x) shouldEqual inst
      }

      it("Double must work") {
        val inst = SampleJDouble(
          JDouble.MAX_VALUE,
          JDouble.MIN_VALUE,
          0.0,
          -123.4567,
          null
        )
        val x = sjXmlCodecOf[SampleJDouble].toXml(inst)
        x should equal("""<SampleJDouble><d1>1.7976931348623157E308</d1><d2>4.9E-324</d2><d3>0.0</d3><d4>-123.4567</d4><d5>null</d5></SampleJDouble>""")
        sjXmlCodecOf[SampleJDouble].fromXml(x) shouldEqual inst
      }

      it("Float must work") {
        val inst = SampleJFloat(
          JFloat.MAX_VALUE,
          JFloat.MIN_VALUE,
          0.0f,
          -123.4567f,
          null
        )
        val x = sjXmlCodecOf[SampleJFloat].toXml(inst)
        x should equal("""<SampleJFloat><f1>3.4028235E38</f1><f2>1.4E-45</f2><f3>0.0</f3><f4>-123.4567</f4><f5>null</f5></SampleJFloat>""")
        sjXmlCodecOf[SampleJFloat].fromXml(x) shouldEqual inst
      }

      it("Integer must work") {
        val inst = SampleJInt(JInt.MAX_VALUE, JInt.MIN_VALUE, 0, 123, null)
        val x = sjXmlCodecOf[SampleJInt].toXml(inst)
        x should equal("""<SampleJInt><i1>2147483647</i1><i2>-2147483648</i2><i3>0</i3><i4>123</i4><i5>null</i5></SampleJInt>""")
        sjXmlCodecOf[SampleJInt].fromXml(x) shouldEqual inst
      }

      it("Long must work") {
        val inst = SampleJLong(JLong.MAX_VALUE, JLong.MIN_VALUE, 0L, 123L, null)
        val x = sjXmlCodecOf[SampleJLong].toXml(inst)
        x should equal("""<SampleJLong><l1>9223372036854775807</l1><l2>-9223372036854775808</l2><l3>0</l3><l4>123</l4><l5>null</l5></SampleJLong>""")
        sjXmlCodecOf[SampleJLong].fromXml(x) shouldEqual inst
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
        val x = sjXmlCodecOf[SampleJNumber].toXml(inst)
        x should equal(
          """<SampleJNumber><n1>-128</n1><n2>127</n2><n3>-32768</n3><n4>32767</n4><n5>-2147483648</n5><n6>2147483647</n6><n7>-9223372036854775808</n7><n8>9223372036854755807</n8><n9>null</n9><n10>0</n10><n11>3.4E-38</n11><n12>3.4E38</n12><n13>1.7E-308</n13><n14>1.7E308</n14><n15>null</n15><n16>0.0</n16><n17>null</n17></SampleJNumber>"""
        )
        sjXmlCodecOf[SampleJNumber].fromXml(x) shouldEqual inst
      }

      it("Short must work") {
        val inst = SampleJShort(
          JShort.MAX_VALUE,
          JShort.MIN_VALUE,
          0.asInstanceOf[Short],
          123.asInstanceOf[Short],
          null
        )
        val x = sjXmlCodecOf[SampleJShort].toXml(inst)
        x should equal("""<SampleJShort><s1>32767</s1><s2>-32768</s2><s3>0</s3><s4>123</s4><s5>null</s5></SampleJShort>""")
        sjXmlCodecOf[SampleJShort].fromXml(x) shouldEqual inst
      }
    }

    // --------------------------------------------------------

    describe(colorString("--- Negative Tests ---")) {
      it("BigDecimal must break") {
        val x =
          """<SampleJBigDecimal><bd1>0</bd1><bd2>1</bd2><bd3>true</bd3><bd4>0.1499999999999999944488848768742172978818416595458984375</bd4><bd5>null</bd5></SampleJBigDecimal>"""
        val msg = """Can't parse BigDecimal (Java): 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJBigDecimal].fromXml(x))
        ex.show shouldEqual msg
      }

      it("BigInt must break") {
        val x =
          """<SampleJBigInteger><bi1>0</bi1><bi2>1</bi2><bi3>true</bi3><bi4>-90182736451928374653345</bi4><bi5>90182736451928374653345</bi5><bi6>0</bi6><bi7>null</bi7></SampleJBigInteger>"""
        val msg = """Can't parse BigInteger (Java): 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJBigInteger].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Boolean must break") {
        val x = """<SampleJBoolean><bool1>true</bool1><bool2/><bool3>true</bool3><bool4>false</bool4><bool5>null</bool5></SampleJBoolean>"""
        val msg = """Expected a value for Boolean (Java) but found nothing"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJBoolean].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Byte must break") {
        val x = """<SampleJByte><b1>127</b1><b2/><b3>0</b3><b4>64</b4><b5>null</b5></SampleJByte>"""
        val msg = """Expected a value for Byte (Java) but found nothing"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJByte].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Char must break") {
        val sj = sjXmlCodecOf[SampleJChar]
        val x = """<SampleJChar><c1/><c2>₠</c2><c3>null</c3></SampleJChar>"""
        val msg = """Expected a value for Char (Java) but found nothing"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
      }

      it("Double must break") {
        val x = """<SampleJDouble><d1>1.7976931348623157E308</d1><d2>blah</d2><d3>0.0</d3><d4>-123.4567</d4><d5>null</d5></SampleJDouble>"""
        val msg = """Can't parse Double (Java): 'blah'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJDouble].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Float must break") {
        val x = """<SampleJFloat><f1>3.4028235E38</f1><f2>blah</f2><f3>0.0</f3><f4>-123.4567</f4><f5>null</f5></SampleJFloat>"""
        val msg = """Can't parse Float (Java): 'blah'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJFloat].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Int must break") {
        val x = """<SampleJInt><i1>2147483647</i1><i2>false</i2><i3>0</i3><i4>123</i4><i5>null</i5></SampleJInt>"""
        val msg = """Can't parse Integer (Java): 'false'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJInt].fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleJInt><i1>2147483647</i1><i2>-2147483648</i2><i3>0.5</i3><i4>123</i4><i5>null</i5></SampleJInt>"""
        the[co.blocke.scalajack.ParseError] thrownBy sjXmlCodecOf[SampleJInt].fromXml(x2) should have message """Can't parse Integer (Java): '0.5'"""
      }

      it("Long must break") {
        val x =
          """<SampleJLong><l1>9223372036854775807</l1><l2>-9223372036854775808</l2><l3>0.7</l3><l4>123</l4><l5>null</l5></SampleJLong>"""
        val msg = """Can't parse Long (Java): '0.7'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJLong].fromXml(x))
      }

      it("Number must break") {
        val x =
          """<SampleJNumber><n1>-128</n1><n2>boom</n2><n3>-32768</n3><n4>32767</n4><n5>-2147483648</n5><n6>2147483647</n6><n7>-9223372036854775808</n7><n8>9223372036854755807</n8><n9>null</n9><n10>0</n10><n11>3.4E-38</n11><n12>3.4E38</n12><n13>1.7E-308</n13><n14>1.7E308</n14><n15>null</n15><n16>0.0</n16><n17>null</n17></SampleJNumber>"""
        val msg = """Can't parse Number (Java): 'boom'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleJNumber].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Short must break") {
        val sj = sjXmlCodecOf[SampleJShort]
        val x = """<SampleJShort><s1>false</s1><s2>-32768</s2><s3>0</s3><s4>123</s4><s5>null</s5></SampleJShort>""""
        val msg = """Can't parse Short (Java): 'false'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleJShort><s1/><s2>-32768</s2><s3>0</s3><s4>123</s4><s5>null</s5></SampleJShort>"""
        the[co.blocke.scalajack.ParseError] thrownBy sj.fromXml(x2) should have message """Expected a value for Short (Java) but found nothing"""
      }
    }
  }
