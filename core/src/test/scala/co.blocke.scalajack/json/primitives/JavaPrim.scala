package co.blocke.scalajack
package json.test.primitives

import org.scalatest.{ FunSpec, Matchers }
import java.lang.{ Boolean => JBoolean, Byte => JByte, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._

import TestUtil._
import co.blocke.scalajack.util.Path

class JavaPrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("---------------------------\n:  Java Primitives Tests  :\n---------------------------") {
    describe("+++ Positive Tests +++") {
      describe("Simple Primitives:") {
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
            new JByte("-128"), new JByte("127"),
            new JShort("-32768"), new JShort("32767"),
            new JInt("-2147483648"), new JInt("2147483647"),
            new JLong("-9223372036854775808"), new JLong("9223372036854755807"),
            null, //new JBigInteger("9923372036854755810"),
            new JByte("0"),
            new JFloat("3.4e-038"), new JFloat("3.4e+038"),
            new JDouble("1.7e-308"), new JDouble("1.7e+308"),
            null, //new JBigDecimal("1.8e+308"),
            new JFloat("0.0"),
            null
          )
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

      /*
      describe("Time Primitives:") {
        it("Duration must work") {
          val inst = SampleDuration(Duration.ZERO, Duration.parse("P2DT3H4M"), null)
          val js = sj.render(inst)
          assertResult("""{"d1":"PT0S","d2":"PT51H4M","d3":null}""") { js }
          assertResult(inst) {
            sj.read[SampleDuration](js)
          }
        }
        it("Instant must work") {
          val inst = SampleInstant(Instant.EPOCH, Instant.MAX, Instant.MIN, Instant.parse("2007-12-03T10:15:30.00Z"), null)
          val js = sj.render(inst)
          assertResult("""{"i1":"1970-01-01T00:00:00Z","i2":"+1000000000-12-31T23:59:59.999999999Z","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}""") { js }
          assertResult(inst) {
            sj.read[SampleInstant](js)
          }
        }
        it("LocalDateTime must work") {
          val inst = SampleLocalDateTime(LocalDateTime.MAX, LocalDateTime.MIN, LocalDateTime.parse("2007-12-03T10:15:30"), null)
          val js = sj.render(inst)
          assertResult("""{"d1":"+999999999-12-31T23:59:59.999999999","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}""") { js }
          assertResult(inst) {
            sj.read[SampleLocalDateTime](js)
          }
        }
        it("LocalDate must work") {
          val inst = SampleLocalDate(LocalDate.MAX, LocalDate.MIN, LocalDate.parse("2007-12-03"), null)
          val js = sj.render(inst)
          assertResult("""{"d1":"+999999999-12-31","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}""") { js }
          assertResult(inst) {
            sj.read[SampleLocalDate](js)
          }
        }
        it("LocalTime must work") {
          val inst = SampleLocalTime(LocalTime.MAX, LocalTime.MIN, LocalTime.MIDNIGHT, LocalTime.NOON, LocalTime.parse("10:15:30"), null)
          val js = sj.render(inst)
          assertResult("""{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"10:15:30","d6":null}""") { js }
          assertResult(inst) {
            sj.read[SampleLocalTime](js)
          }
        }
        it("OffsetDateTime must work") {
          val inst = SampleOffsetDateTime(OffsetDateTime.MAX, OffsetDateTime.MIN, OffsetDateTime.parse("2007-12-03T10:15:30+01:00"), null)
          val js = sj.render(inst)
          assertResult("""{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}""") { js }
          assertResult(inst) {
            sj.read[SampleOffsetDateTime](js)
          }
        }
        it("OffsetTime must work") {
          val inst = SampleOffsetTime(OffsetTime.MAX, OffsetTime.MIN, OffsetTime.parse("10:15:30+01:00"), null)
          val js = sj.render(inst)
          assertResult("""{"o1":"23:59:59.999999999-18:00","o2":"00:00:00+18:00","o3":"10:15:30+01:00","o4":null}""") { js }
          assertResult(inst) {
            sj.read[SampleOffsetTime](js)
          }
        }
        it("Period must work") {
          val inst = SamplePeriod(Period.ZERO, Period.parse("P1Y2M3D"), null)
          val js = sj.render(inst)
          assertResult("""{"p1":"P0D","p2":"P1Y2M3D","p3":null}""") { js }
          assertResult(inst) {
            sj.read[SamplePeriod](js)
          }
        }
        it("ZonedDateTime must work") {
          val inst = SampleZonedDateTime(ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]"), null)
          val js = sj.render(inst)
          assertResult("""{"o1":"2007-12-03T10:15:30+01:00[Europe\/Paris]","o2":null}""") { js }
          assertResult(inst) {
            sj.read[SampleZonedDateTime](js)
          }
        }
      }
      */
    }
    describe("--- Negative Tests ---") {
      describe("Simple Primitives:") {
        it("BigDecimal must break") {
          val js = """{"bd1":0,"bd2":1,"bd3":10,"bd4":"0.1499999999999999944488848768742172978818416595458984375","bd5":null}"""
          assert(expectUnexpected(() => sj.read[SampleBigDecimal](js), Path.Root \ "bd4", List("String")))
        }
        it("BigInteger must break") {
          val js = """{"bi1":"0","bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":90182736451928374653345,"bi6":0,"bi7":null}"""
          assert(expectUnexpected(() => sj.read[SampleJBigInteger](js), Path.Root \ "bi1", List("String")))
        }
        it("Boolean must break") {
          val js = """{"bool1":true,"bool2":false,"bool3":true,"bool4":"false","bool5":null}"""
          assert(expectUnexpected(() => sj.read[SampleJBoolean](js), Path.Root \ "bool4", List("String")))
        }
        it("Byte must break") {
          val js = """{"b1":127,"b2":-128,"b3":false,"b4":64,"b5":null}"""
          assert(expectUnexpected(() => sj.read[SampleJByte](js), Path.Root \ "b3", List("False")))
        }
        it("Char must break") {
          val js = """{"c1":"Z","c2":3,"c3":null}"""
          assert(expectUnexpected(() => sj.read[SampleJChar](js), Path.Root \ "c2", List("Number")))
        }
        it("Double must break") {
          val js = """{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":"0.0","d4":-123.4567,"d5":null}"""
          assert(expectUnexpected(() => sj.read[SampleJDouble](js), Path.Root \ "d3", List("String")))
        }
        it("Float must break") {
          val js = """{"f1":3.4028235E38,"f2":"1.4E-45","f3":0.0,"f4":-123.4567,"f5":null}"""
          assert(expectUnexpected(() => sj.read[SampleJFloat](js), Path.Root \ "f2", List("String")))
        }
        it("Int must break") {
          val js = """{"i1":2147483647,"i2":-2147483648,"i3":false,"i4":123,"i5":null}"""
          assert(expectUnexpected(() => sj.read[SampleJInt](js), Path.Root \ "i3", List("False")))
          val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":0.3,"i4":123,"i5":null}"""
          assert(expectMalformed[NumberFormatException](() => sj.read[SampleJInt](js2), Path.Root \ "i3", List.empty[String]))
        }
        it("Long must break") {
          val js = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":"0","l4":123,"l5":null}"""
          assert(expectUnexpected(() => sj.read[SampleJLong](js), Path.Root \ "l3", List("String")))
          val js2 = """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123,"l5":null}"""
          assert(expectMalformed[NumberFormatException](() => sj.read[SampleJLong](js2), Path.Root \ "l3", List.empty[String]))
        }
        it("Number must break") {
          val js = """{"n1":-128,"n2":127,"n3":"-32768","n4":32767,"n5":-2147483648,"n6":2147483647,"n7":-9223372036854775808,"n8":9223372036854755807,"n9":9923372036854755810,"n10":0,"n11":3.4E-38,"n12":3.4E38,"n13":1.7E-308,"n14":1.7E308,"n15":1.8E+308,"n16":0.0,"n17":null}"""
          assert(expectUnexpected(() => sj.read[SampleJNumber](js), Path.Root \ "n3", List("String")))
        }
        it("Short must break") {
          val js = """{"s1":false,"s2":-32768,"s3":0,"s4":123,"s5":null}"""
          assert(expectUnexpected(() => sj.read[SampleJShort](js), Path.Root \ "s1", List("False")))
          val js2 = """{"s1":2.3,"s2":-32768,"s3":0,"s4":123,"s5":null}"""
          assert(expectMalformed[NumberFormatException](() => sj.read[SampleJShort](js2), Path.Root \ "s1", List.empty[String]))
        }
      }
      describe("Time Primitives:") {
        /*
        it("Duration must break") {
          val js = """{"d1":"PT0S","d2":21,"d3":null}"""
          val msg = """Expected value token of type String, not Number when reading Duration value.  (Is your value wrapped in quotes?)
            |{"d1":"PT0S","d2":21,"d3":null}
            |------------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleDuration](js) should have message msg
          val js2 = """{"d1":"PT0S","d2":"bogus","d3":null}"""
          val msg2 = """Text cannot be parsed to a Duration
            |{"d1":"PT0S","d2":"bogus","d3":null}
            |------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleDuration](js2) should have message msg2
        }
        it("Instant must break") {
          val js = """{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
          val msg = """Expected value token of type String, not False when reading Instant value.  (Is your value wrapped in quotes?)
            |{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i4":"200
            |----------------------------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleInstant](js) should have message msg
          val js2 = """{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
          val msg2 = """Text 'bogus' could not be parsed at index 0
            |{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z","i4":"2
            |----------------------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleInstant](js2) should have message msg2
        }
        it("LocalDateTime must break") {
          val js = """{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}"""
          val msg = """Expected value token of type String, not Number when reading LocalDateTime value.  (Is your value wrapped in quotes?)
            |{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-
            |------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleLocalDateTime](js) should have message msg
          val js2 = """{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}"""
          val msg2 = """Text 'bogus' could not be parsed at index 0
            |{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"200
            |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleLocalDateTime](js2) should have message msg2
        }
        it("LocalDate must break") {
          val js = """{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
          val msg = """Expected value token of type String, not Number when reading LocalDate value.  (Is your value wrapped in quotes?)
            |{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":
            |------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleLocalDate](js) should have message msg
          val js2 = """{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
          val msg2 = """Text 'bogus' could not be parsed at index 0
            |{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03",
            |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleLocalDate](js2) should have message msg2
        }
        it("LocalTime must break") {
          val js = """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}"""
          val msg = """Expected value token of type String, not False when reading LocalTime value.  (Is your value wrapped in quotes?)
            |":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}
            |--------------------------------------------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleLocalTime](js) should have message msg
          val js2 = """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}"""
          val msg2 = """Text 'Bogus' could not be parsed at index 0
            |":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}
            |--------------------------------------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleLocalTime](js2) should have message msg2
        }
        it("OffsetDateTime must break") {
          val js = """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30+01:00","o4":null}"""
          val msg = """Expected value token of type String, not Number when reading OffsetDateTime value.  (Is your value wrapped in quotes?)
            |:"+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30+01:00","o4":null}
            |--------------------------------------------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleOffsetDateTime](js) should have message msg
          val js2 = """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}"""
          val msg2 = """Text '-999999999-01T00:00:00+18:00' could not be parsed at index 13
            |:"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10
            |--------------------------------------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleOffsetDateTime](js2) should have message msg2
        }
        it("OffsetTime must break") {
          val js = """{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}"""
          val msg = """Expected value token of type String, not False when reading OffsetTime value.  (Is your value wrapped in quotes?)
            |{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}
            |--------------------------------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleOffsetTime](js) should have message msg
          val js2 = """{"o1":"23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o4":null}"""
          val msg2 = """Text '00:00:00:00+18:00' could not be parsed at index 8
            |{"o1":"23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o4":nul
            |--------------------------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleOffsetTime](js2) should have message msg2
        }
        it("Period must break") {
          val js = """{"p1":"P0D","p2":5,"p3":null}"""
          val msg = """Expected value token of type String, not Number when reading Period value.  (Is your value wrapped in quotes?)
            |{"p1":"P0D","p2":5,"p3":null}
            |-----------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SamplePeriod](js) should have message msg
          val js2 = """{"p1":"P0D","p2":"bogus","p3":null}"""
          val msg2 = """Text cannot be parsed to a Period
            |{"p1":"P0D","p2":"bogus","p3":null}
            |-----------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SamplePeriod](js2) should have message msg2
        }
        it("ZonedDateTime must break") {
          val js = """{"o1":true,"o2":null}"""
          val msg = """Expected value token of type String, not True when reading ZonedDateTime value.  (Is your value wrapped in quotes?)
            |{"o1":true,"o2":null}
            |------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleZonedDateTime](js) should have message msg
          val js2 = """{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}"""
          val msg2 = """Text '2007-12-03T10:15:30+01:00 Earth' could not be parsed, unparsed text found at index 25
            |{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}
            |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleZonedDateTime](js2) should have message msg2
        }
        */
      }
    }
  }
}
