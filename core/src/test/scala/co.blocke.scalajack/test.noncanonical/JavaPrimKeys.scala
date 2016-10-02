package co.blocke.scalajack
package test
package noncanonical

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import java.lang.{ Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong, Number => JNumber, Short => JShort }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._

class JavaPrimKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("---------------------------------------\n:  Java Primitive Noncanonical Tests  :\n---------------------------------------") {
    describe("+++ Positive Tests +++") {
      describe("Simple Primitives:") {
        it("With BigDecimal Key") {
          val inst = SampleJBigDecimal(Map(new JBigDecimal("123.456") -> new JBigDecimal("1"), new JBigDecimal("789.123") -> new JBigDecimal("2")))
          val js = sj.render(inst)
          assertResult("""{"m":{"123.456":1,"789.123":2}}""") { js }
          assertResult(inst) {
            sj.read[SampleJBigDecimal](js)
          }
        }
        it("With BigInteger Key") {
          val inst = SampleJBigInteger(Map(new JBigInteger("123") -> new JBigInteger("1"), new JBigInteger("789") -> new JBigInteger("2")))
          val js = sj.render(inst)
          assertResult("""{"m":{"123":1,"789":2}}""") { js }
          assertResult(inst) {
            sj.read[SampleJBigInteger](js)
          }
        }
        it("With Boolean Key") {
          val inst = SampleJBoolean(Map(true.asInstanceOf[JBoolean] -> false.asInstanceOf[JBoolean], false.asInstanceOf[JBoolean] -> true.asInstanceOf[JBoolean]))
          val js = sj.render(inst)
          assertResult("""{"m":{"true":false,"false":true}}""") { js }
          assertResult(inst) {
            sj.read[SampleJBoolean](js)
          }
        }
        it("With Byte Key") {
          val inst = SampleJByte(Map(16.toByte.asInstanceOf[JByte] -> 2.toByte.asInstanceOf[JByte], 48.toByte.asInstanceOf[JByte] -> 9.toByte.asInstanceOf[JByte]))
          val js = sj.render(inst)
          assertResult("""{"m":{"16":2,"48":9}}""") { js }
          assertResult(inst) {
            sj.read[SampleJByte](js)
          }
        }
        it("With Char Key") {
          val inst = SampleJChar(Map('a'.asInstanceOf[JChar] -> 'A'.asInstanceOf[JChar], 'z'.asInstanceOf[JChar] -> 'Z'.asInstanceOf[JChar]))
          val js = sj.render(inst)
          assertResult("""{"m":{"a":"A","z":"Z"}}""") { js }
          assertResult(inst) {
            sj.read[SampleJChar](js)
          }
        }
        it("With Double Key") {
          val inst = SampleJDouble(Map(12.34.asInstanceOf[JDouble] -> 56.78.asInstanceOf[JDouble], 90.12.asInstanceOf[JDouble] -> 34.56.asInstanceOf[JDouble]))
          val js = sj.render(inst)
          assertResult("""{"m":{"12.34":56.78,"90.12":34.56}}""") { js }
          assertResult(inst) {
            sj.read[SampleJDouble](js)
          }
        }
        it("With Float Key") {
          val inst = SampleJFloat(Map(12.34F.asInstanceOf[JFloat] -> 56.78F.asInstanceOf[JFloat], 90.12F.asInstanceOf[JFloat] -> 34.56F.asInstanceOf[JFloat]))
          val js = sj.render(inst)
          assertResult("""{"m":{"12.34":56.78,"90.12":34.56}}""") { js }
          assertResult(inst) {
            sj.read[SampleJFloat](js)
          }
        }
        it("With Integer Key") {
          val inst = SampleJInteger(Map(12.asInstanceOf[JInteger] -> 56.asInstanceOf[JInteger], 90.asInstanceOf[JInteger] -> 34.asInstanceOf[JInteger]))
          val js = sj.render(inst)
          assertResult("""{"m":{"12":56,"90":34}}""") { js }
          assertResult(inst) {
            sj.read[SampleJInteger](js)
          }
        }
        it("With Long Key") {
          val inst = SampleJLong(Map(12L.asInstanceOf[JLong] -> 56L.asInstanceOf[JLong], 90L.asInstanceOf[JLong] -> 34L.asInstanceOf[JLong]))
          val js = sj.render(inst)
          assertResult("""{"m":{"12":56,"90":34}}""") { js }
          assertResult(inst) {
            sj.read[SampleJLong](js)
          }
        }
        it("With Number Key") {
          val inst = SampleJNumber(Map(
            new JByte("-128") -> new JByte("127"),
            new JShort("-32768") -> new JShort("32767"),
            new JInteger("-2147483648") -> new JInteger("2147483647"),
            new JLong("-9223372036854775808") -> new JLong("9223372036854755807"),
            new JByte("0") -> new JBigInteger("9923372036854755810"),
            new JFloat("3.4e-038") -> new JFloat("3.4e+038"),
            new JDouble("1.7e-308") -> new JDouble("1.7e+308"),
            new JBigDecimal("1.8e+308") -> new JFloat("0.0")
          ))
          val js = sj.render(inst)
          assertResult("""{"m":{"0":9923372036854755810,"-2147483648":2147483647,"-9223372036854775808":9223372036854755807,"-128":127,"3.4E-38":3.4E38,"-32768":32767,"1.8E+308":0.0,"1.7E-308":1.7E308}}""") { js }
          val read = sj.read[SampleJNumber](js)
          assertResult(inst) { read }
          assertResult("List((1.8E+308,java.math.BigDecimal), (-32768,java.lang.Short), (-9223372036854775808,java.lang.Long), (-2147483648,java.lang.Integer), (3.4E-38,java.lang.Float), (1.7E-308,java.lang.Double), (0,java.lang.Byte), (-128,java.lang.Byte))") {
            // Hokem to ensure consistent ordering of keys from Map for success comparison
            read.m.keySet.map(z => (z, z.getClass.getName)).toList.sortWith((a, b) => a._2 > b._2).toString
          }
        }
        it("With Short Key") {
          val inst = SampleJShort(Map(12.toShort.asInstanceOf[JShort] -> 56.toShort.asInstanceOf[JShort], 90.toShort.asInstanceOf[JShort] -> 34.toShort.asInstanceOf[JShort]))
          val js = sj.render(inst)
          assertResult("""{"m":{"12":56,"90":34}}""") { js }
          assertResult(inst) {
            sj.read[SampleJShort](js)
          }
        }
      }
      describe("Time Primitives:") {
        it("With Duration Key") {
          val inst = SampleDuration(Map(Duration.ZERO -> Duration.parse("P2DT3H4M")))
          val js = sj.render(inst)
          assertResult("""{"m":{"PT0S":"PT51H4M"}}""") { js }
          assertResult(inst) {
            sj.read[SampleDuration](js)
          }
        }
        it("With Instant Key") {
          val inst = SampleInstant(Map(Instant.EPOCH -> Instant.MAX, Instant.MIN -> Instant.parse("2007-12-03T10:15:30.00Z")))
          val js = sj.render(inst)
          assertResult("""{"m":{"1970-01-01T00:00:00Z":"+1000000000-12-31T23:59:59.999999999Z","-1000000000-01-01T00:00:00Z":"2007-12-03T10:15:30Z"}}""") { js }
          assertResult(inst) {
            sj.read[SampleInstant](js)
          }
        }
        it("With LocalDateTime Key") {
          val inst = SampleLocalDateTime(Map(LocalDateTime.MAX -> LocalDateTime.MIN, LocalDateTime.parse("2007-12-03T10:15:30") -> null))
          val js = sj.render(inst)
          assertResult("""{"m":{"+999999999-12-31T23:59:59.999999999":"-999999999-01-01T00:00:00","2007-12-03T10:15:30":null}}""") { js }
          assertResult(inst) {
            sj.read[SampleLocalDateTime](js)
          }
        }
        it("With LocalDate Key") {
          val inst = SampleLocalDate(Map(LocalDate.MAX -> LocalDate.MIN, LocalDate.parse("2007-12-03") -> null))
          val js = sj.render(inst)
          assertResult("""{"m":{"+999999999-12-31":"-999999999-01-01","2007-12-03":null}}""") { js }
          assertResult(inst) {
            sj.read[SampleLocalDate](js)
          }
        }
        it("With LocalTime Key") {
          val inst = SampleLocalTime(Map(LocalTime.MAX -> LocalTime.MIN, LocalTime.MIDNIGHT -> LocalTime.NOON, LocalTime.parse("10:15:30") -> null))
          val js = sj.render(inst)
          assertResult("""{"m":{"23:59:59.999999999":"00:00:00","00:00:00":"12:00:00","10:15:30":null}}""") { js }
          assertResult(inst) {
            sj.read[SampleLocalTime](js)
          }
        }
        it("With OffsetDateTime Key") {
          val inst = SampleOffsetDateTime(Map(OffsetDateTime.MAX -> OffsetDateTime.MIN, OffsetDateTime.parse("2007-12-03T10:15:30+01:00") -> null))
          val js = sj.render(inst)
          assertResult("""{"m":{"+999999999-12-31T23:59:59.999999999-18:00":"-999999999-01-01T00:00:00+18:00","2007-12-03T10:15:30+01:00":null}}""") { js }
          assertResult(inst) {
            sj.read[SampleOffsetDateTime](js)
          }
        }
        it("With OffsetTime Key") {
          val inst = SampleOffsetTime(Map(OffsetTime.MAX -> OffsetTime.MIN, OffsetTime.parse("10:15:30+01:00") -> null))
          val js = sj.render(inst)
          assertResult("""{"m":{"23:59:59.999999999-18:00":"00:00:00+18:00","10:15:30+01:00":null}}""") { js }
          assertResult(inst) {
            sj.read[SampleOffsetTime](js)
          }
        }
        it("With Period Key") {
          val inst = SamplePeriod(Map(Period.ZERO -> Period.parse("P1Y2M3D")))
          val js = sj.render(inst)
          assertResult("""{"m":{"P0D":"P1Y2M3D"}}""") { js }
          assertResult(inst) {
            sj.read[SamplePeriod](js)
          }
        }
        it("With ZonedDateTime Key") {
          val inst = SampleZonedDateTime(Map(ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]") -> null))
          val js = sj.render(inst)
          assertResult("""{"m":{"2007-12-03T10:15:30+01:00[Europe\/Paris]":null}}""") { js }
          assertResult(inst) {
            sj.read[SampleZonedDateTime](js)
          }
        }
      }
    }
    describe("--- Negative Tests ---") {
      describe("Simple Primitives:") {
        it("Bad BigDecimal Key") {
          val js = """{"m":{"fred":1,"789.123":2}}"""
          val msg = """Expected value token of type Number, not String when reading BigDecimal value.  (Is your value wrapped in quotes?)
          |{"m":{"fred":1,"789.123":2}}
          |------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleJBigDecimal](js) should have message msg
        }
        it("Bad BigInt Key") {
          val js = """{"m":{"fred":1,"789":2}}"""
          val msg = """Expected value token of type Number, not String when reading BigInteger value.  (Is your value wrapped in quotes?)
          |{"m":{"fred":1,"789":2}}
          |------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleJBigInteger](js) should have message msg
        }
        it("Bad Boolean Key") {
          val js = """{"m":{"true":false,"123":true}}"""
          val msg = """Expected value token of type True or False, not Number when reading Boolean value.  (Is your value wrapped in quotes?)
          |{"m":{"true":false,"123":true}}
          |-------------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleJBoolean](js) should have message msg
        }
        it("Bad Byte Key") {
          val js = """{"m":{"16":2,"x48":9}}"""
          val msg = """Expected value token of type Number, not String when reading Byte value.  (Is your value wrapped in quotes?)
          |{"m":{"16":2,"x48":9}}
          |-------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleJByte](js) should have message msg
        }
        it("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
          val js = """{"m":{null:"A","z":"Z"}}"""
          val msg = """Character out of place. Un-quoted literal not expected here.  (Possile un-terminated string earlier in your JSON.)
          |{"m":{null:"A","z":"Z"}}
          |------^""".stripMargin
          the[java.lang.IllegalArgumentException] thrownBy sj.read[SampleJChar](js) should have message msg
        }
        it("Bad Double Key") {
          val js = """{"m":{"12.34":56.78,"true":34.56}}"""
          val msg = """Expected value token of type Number, not True when reading Double value.  (Is your value wrapped in quotes?)
          |{"m":{"12.34":56.78,"true":34.56}}
          |--------------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleJDouble](js) should have message msg
        }
        it("Bad Float Key") {
          val js = """{"m":{"12.34":56.78,"90.12.3":34.56}}"""
          val msg = """multiple points
          |{"m":{"12.34":56.78,"90.12.3":34.56}}
          |--------------------^""".stripMargin
          the[java.lang.NumberFormatException] thrownBy sj.read[SampleJFloat](js) should have message msg
        }
        it("Bad Int Key") {
          val js = """{"m":{"12.0":56,"90":34}}"""
          val msg = """For input string: "12.0"
          |{"m":{"12.0":56,"90":34}}
          |------^""".stripMargin
          the[java.lang.NumberFormatException] thrownBy sj.read[SampleJInteger](js) should have message msg
        }
        it("Bad Long Key") {
          val js = """{"m":{"12":56,"hey":34}}"""
          val msg = """Expected value token of type Number, not String when reading Long value.  (Is your value wrapped in quotes?)
          |{"m":{"12":56,"hey":34}}
          |--------------^""".stripMargin
          the[java.lang.IllegalStateException] thrownBy sj.read[SampleJLong](js) should have message msg
        }
        it("Bad Number Key") {
          val js = """{"m":{"0x":9923372036854755810,"-2147483648":2147483647,"-9223372036854775808":9223372036854755807,"-128":127,"3.4E-38":3.4E38,"-32768":32767,"1.8E+308":0.0,"1.7E-308":1.7E308}}"""
          val msg = """Character out of place. Un-quoted literal not expected here.  (Possile un-terminated string earlier in your JSON.)
          |0x
          |-^ Extracted from source here:
          |{"m":{"0x":9923372036854755810,"-2147483648":2147483647,
          |------^""".stripMargin
          the[java.lang.IllegalArgumentException] thrownBy sj.read[SampleJNumber](js) should have message msg
        }
        it("Bad Short Key") {
          val js = """{"m":{"99999":56,"90":34}}"""
          val msg = """Value out of range. Value:"99999" Radix:10
          |{"m":{"99999":56,"90":34}}
          |------^""".stripMargin
          the[java.lang.NumberFormatException] thrownBy sj.read[SampleJShort](js) should have message msg
        }
      }
      describe("Time Primitives:") {
        it("Bad Duration Key") {
          val js = """{"m":{"PT0SXXX":"PT51H4M"}}"""
          val msg = """Text cannot be parsed to a Duration
          |{"m":{"PT0SXXX":"PT51H4M"}}
          |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleDuration](js) should have message msg
        }
        it("Bad Instant Key") {
          val js = """{"m":{"1970-01-01T00:00:00Z":"+1000000000-12-31T23:59:59.999999999Z","bogus":"2007-12-03T10:15:30Z"}}"""
          val msg = """Text 'bogus' could not be parsed at index 0
          |0:00:00Z":"+1000000000-12-31T23:59:59.999999999Z","bogus":"2007-12-03T10:15:30Z"}}
          |--------------------------------------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleInstant](js) should have message msg
        }
        it("Bad LocalDateTime Key") {
          val js = """{"m":{"+999999999-12-31T23:59:59.999999999":"-999999999-01-01T00:00:00","bogus":null}}"""
          val msg = """Text 'bogus' could not be parsed at index 0
          |1T23:59:59.999999999":"-999999999-01-01T00:00:00","bogus":null}}
          |--------------------------------------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleLocalDateTime](js) should have message msg
        }
        it("Bad LocalDate Key") {
          val js = """{"m":{"bogus":"-999999999-01-01","2007-12-03":null}}"""
          val msg = """Text 'bogus' could not be parsed at index 0
          |{"m":{"bogus":"-999999999-01-01","2007-12-03":null}}
          |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleLocalDate](js) should have message msg
        }
        it("Bad LocalTime Key") {
          val js = """{"m":{"23:59:59.999999999":"00:00:00","nada":"12:00:00","10:15:30":null}}"""
          val msg = """Text 'nada' could not be parsed at index 0
          |{"m":{"23:59:59.999999999":"00:00:00","nada":"12:00:00","10:15:30":null}}
          |--------------------------------------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleLocalTime](js) should have message msg
        }
        it("Bad OffsetDateTime Key") {
          val js = """{"m":{"false":"-999999999-01-01T00:00:00+18:00","2007-12-03T10:15:30+01:00":null}}"""
          val msg = """Text 'false' could not be parsed at index 0
          |{"m":{"false":"-999999999-01-01T00:00:00+18:00","2007-12
          |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleOffsetDateTime](js) should have message msg
        }
        it("Bad OffsetTime Key") {
          val js = """{"m":{"2007-12-03T10:15:30+01:00[Europe\/Bogus]":null}}"""
          val msg = """Text '2007-12-03T10:15:30+01:00[Europe/Bogus]' could not be parsed at index 2
          |{"m":{"2007-12-03T10:15:30+01:00[Europe\/Bogus]":null}}
          |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleOffsetTime](js) should have message msg
        }
        it("Bad Period Key") {
          val js = """{"m":{"P0D???":"P1Y2M3D"}}"""
          val msg = """Text cannot be parsed to a Period
          |{"m":{"P0D???":"P1Y2M3D"}}
          |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SamplePeriod](js) should have message msg
        }
        it("Bad ZonedDateTime Key") {
          val js = """{"m":{"FRED23:59:59.999999999-18:00":"00:00:00+18:00","10:15:30+01:00":null}}"""
          val msg = """Text 'FRED23:59:59.999999999-18:00' could not be parsed at index 0
          |{"m":{"FRED23:59:59.999999999-18:00":"00:00:00+18:00","1
          |------^""".stripMargin
          the[java.time.format.DateTimeParseException] thrownBy sj.read[SampleZonedDateTime](js) should have message msg
        }
      }
    }
  }
}