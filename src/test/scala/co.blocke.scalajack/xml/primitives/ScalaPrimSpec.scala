package co.blocke.scalajack
package xml
package primitives

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import TestUtil.*

import scala.math.BigDecimal

class ScalaPrimSpec() extends AnyFunSpec:

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

        val sj = sjXmlCodecOf[SampleBigDecimal]
        val x = sj.toXml(inst)
        x should equal("""<SampleBigDecimal><bd1>123</bd1><bd2>1.23</bd2><bd3>0</bd3><bd4>123.456</bd4><bd5>0.1499999999999999944488848768742172978818416595458984375</bd5><bd6>null</bd6></SampleBigDecimal>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("BigInt must work") {
        val inst = SampleBigInt(
          BigInt("-90182736451928374653345"),
          BigInt("90182736451928374653345"),
          BigInt(0),
          null
        )
        val sj = sjXmlCodecOf[SampleBigInt]
        val x = sj.toXml(inst)
        x should equal("""<SampleBigInt><bi1>-90182736451928374653345</bi1><bi2>90182736451928374653345</bi2><bi3>0</bi3><bi4>null</bi4></SampleBigInt>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(bool1 = true, bool2 = false)
        val sj = sjXmlCodecOf[SampleBoolean]
        val x = sj.toXml(inst)
        x should equal("""<SampleBoolean><bool1>true</bool1><bool2>false</bool2></SampleBoolean>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Byte must work (not nullable)") {
        val inst = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
        val sj = sjXmlCodecOf[SampleByte]
        val x = sj.toXml(inst)
        x should equal("""<SampleByte><b1>127</b1><b2>-128</b2><b3>0</b3><b4>64</b4></SampleByte>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Char must work (not nullable)") {
        val inst = SampleChar('-', 'Z', '\u20A0')
        val sj = sjXmlCodecOf[SampleChar]
        val x = sj.toXml(inst)
        x should equal("""<SampleChar><c1>-</c1><c2>Z</c2><c3>₠</c3></SampleChar>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Double must work (not nullable)") {
        val inst =
          SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val sj = sjXmlCodecOf[SampleDouble]
        val x = sj.toXml(inst)
        x should equal("""<SampleDouble><d1>1.7976931348623157E308</d1><d2>-1.7976931348623157E308</d2><d3>0.0</d3><d4>-123.4567</d4></SampleDouble>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Float must work (not nullable)") {
        val inst = SampleFloat(Float.MaxValue, Float.MinValue, 0.0f, -123.4567f)
        val sj = sjXmlCodecOf[SampleFloat]
        val x = sj.toXml(inst)
        x should equal("""<SampleFloat><f1>3.4028235E38</f1><f2>-3.4028235E38</f2><f3>0.0</f3><f4>-123.4567</f4></SampleFloat>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Int must work (not nullable)") {
        val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val sj = sjXmlCodecOf[SampleInt]
        val x = sj.toXml(inst)
        x should equal("""<SampleInt><i1>2147483647</i1><i2>-2147483648</i2><i3>0</i3><i4>123</i4></SampleInt>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val sj = sjXmlCodecOf[SampleLong]
        val x = sj.toXml(inst)
        x should equal("""<SampleLong><l1>9223372036854775807</l1><l2>-9223372036854775808</l2><l3>0</l3><l4>123</l4></SampleLong>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("Short must work (not nullable)") {
        val inst = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
        val sj = sjXmlCodecOf[SampleShort]
        val x = sj.toXml(inst)
        x should equal("""<SampleShort><s1>32767</s1><s2>-32768</s2><s3>0</s3><s4>123</s4></SampleShort>""")
        sj.fromXml(x) shouldEqual inst
      }

      it("String must work") {
        val inst = SampleString("something", "", null)
        val sj = sjXmlCodecOf[SampleString]
        val x = sj.toXml(inst)
        x should equal("""<SampleString><s1>something</s1><s2/><s3>null</s3></SampleString>""")
        sj.fromXml(x) shouldEqual inst
      }

      /*
      it("Any type for all primitives must work") {
        val sj = sjXmlCodecOf[AnyShell]
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
          val x = sj.toXml(inst)
          x should equal(j)
          fn match {
            case Some(f) => sj.fromXml(x) shouldEqual (AnyShell(f(v)))
            case None    => sj.fromXml(x) shouldEqual inst
          }
        }
      }
       */
    }

    // --------------------------------------------------------

    describe(colorString("--- Negative Tests ---")) {
      it("BigDecimal must break") {
        val x = """<SampleBigDecimal><bd1>123</bd1><bd2>1.23</bd2><bd3>0</bd3><bd4>123.456</bd4><bd5>bogus</bd5><bd6>null</bd6></SampleBigDecimal>"""
        val msg: String = """Can't parse BigDecimal (Scala): 'bogus'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleBigDecimal].fromXml(x))
        ex.show shouldEqual msg
      }

      it("BigInt must break") {
        val x =
          """<SampleBigInt><bi1>foom</bi1><bi2>90182736451928374653345</bi2><bi3>0</bi3><bi4>null</bi4></SampleBigInt>"""
        val msg = """Can't parse BigInt (Scala): 'foom'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleBigInt].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Boolean must break") {
        val x = """<SampleBoolean><bool1>123</bool1><bool2>false</bool2></SampleBoolean>"""
        val sj = sjXmlCodecOf[SampleBoolean]
        val msg = """Can't parse Boolean: '123'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleBoolean><bool1>true</bool1><bool2>null</bool2></SampleBoolean>"""
        val msg2 = """Boolean not allowed to have a null value"""
        val ex2 = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x2))
        ex2.show shouldEqual msg2
      }

      it("Byte must break") {
        val x = """<SampleByte><b1>true</b1><b2>-128</b2><b3>0</b3><b4>64</b4></SampleByte>"""
        val sj = sjXmlCodecOf[SampleByte]
        val msg = """Can't parse Byte: 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleByte><b1>null</b1><b2>-128</b2><b3>0</b3><b4>64</b4></SampleByte>"""
        val msg2 = """Byte not allowed to have a null value"""
        val ex2 = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x2))
        ex2.show shouldEqual msg2
      }

      it("Char must break") {
        val x = """<SampleChar><c1>null</c1><c2>Z</c2><c3>₠</c3></SampleChar>"""
        val sj = sjXmlCodecOf[SampleChar]
        val msg = """Char not allowed to have a null value"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleChar><c1/><c2>Z</c2><c3>₠</c3></SampleChar>"""
        val msg2 = """Expected a value for Char but found nothing"""
        val ex2 = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x2))
        ex2.show shouldEqual msg2
      }

      it("Double must break") {
        val x = """<SampleDouble><d1>1.79769313486E23157E308</d1><d2>-1.7976931348623157E308</d2><d3>0.0</d3><d4>-123.4567</d4></SampleDouble>"""
        val msg = """Can't parse Double: '1.79769313486E23157E308'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleDouble].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Float must break") {
        val x = """<SampleFloat><f1>3.4028235E38</f1><f2>X-34028235E38</f2><f3>0.0</f3><f4>-123.4567</f4></SampleFloat>"""
        val msg = """Can't parse Float: 'X-34028235E38'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sjXmlCodecOf[SampleFloat].fromXml(x))
        ex.show shouldEqual msg
      }

      it("Int must break") {
        val sj = sjXmlCodecOf[SampleInt]
        val x = """<SampleInt><i1>2147483647</i1><i2>-2147483648</i2><i3>true</i3><i4>123</i4></SampleInt>"""
        val msg = """Can't parse Int: 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleInt><i1>2147483647</i1><i2>-2147483648</i2><i3>2.3</i3><i4>123</i4></SampleInt>"""
        val msg2 = """Can't parse Int: '2.3'"""
        val ex2 = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x2))
        ex2.show shouldEqual msg2
      }

      it("Long must break") {
        val sj = sjXmlCodecOf[SampleLong]
        val x = """<SampleLong><l1>9223372036854775807</l1><l2>-9223372036854775808</l2><l3>true</l3><l4>123</l4></SampleLong>"""
        val msg = """Can't parse Long: 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleLong><l1>9223372036854775807</l1><l2>-9223372036854775808</l2><l3>0.3</l3><l4>123</l4></SampleLong>"""
        val msg2 = """Can't parse Long: '0.3'"""
        val ex2 = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x2))
        ex2.show shouldEqual msg2
      }

      it("Short must break") {
        val sj = sjXmlCodecOf[SampleShort]
        val x = """<SampleShort><s1>32767</s1><s2>true</s2><s3>0</s3><s4>123</s4></SampleShort>"""
        val msg = """Can't parse Short: 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show shouldEqual msg
        val x2 = """<SampleShort><s1>32767</s1><s2>3.4</s2><s3>0</s3><s4>123</s4></SampleShort>"""
        val msg2 = """Can't parse Short: '3.4'"""
        val ex2 = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x2))
        ex2.show shouldEqual msg2
      }
    }
  }
