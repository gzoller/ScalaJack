package co.blocke.scalajack
package json.mapkeys

import org.scalatest.{ FunSpec, Matchers }
import java.lang.{ Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong, Short => JShort }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._

import co.blocke.scalajack.json.JsonMatcher.{ matchJson, parseJValue }

class JavaPrimKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("----------------------------------\n:  Java Primitive Map Key Tests  :\n----------------------------------") {
    describe("+++ Positive Tests +++") {
      describe("Simple DelimSpec:") {
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
            JByte.valueOf("-128") -> JByte.valueOf("127"),
            JShort.valueOf("-32768") -> JShort.valueOf("32767"),
            JInteger.valueOf("-2147483648") -> JInteger.valueOf("2147483647"),
            JLong.valueOf("-9223372036854775808") -> JLong.valueOf("9223372036854755807"),
            JByte.valueOf("0") -> new JBigInteger("9923372036854755810"),
            JFloat.valueOf("3.4e-038") -> JFloat.valueOf("3.4e+038"),
            JDouble.valueOf("1.7e-308") -> JDouble.valueOf("1.7e+308"),
            new JBigDecimal("1.8e+308") -> JFloat.valueOf("0.0")))
          val result = SampleJNumber(Map(
            JByte.valueOf("0") -> new JBigDecimal("9923372036854755810"),
            JInteger.valueOf("-2147483648") -> JInteger.valueOf("2147483647"),
            JLong.valueOf("-9223372036854775808") -> JLong.valueOf("9223372036854755807"),
            JByte.valueOf("-128") -> JByte.valueOf("127"),
            JFloat.valueOf("3.4E-38") -> JFloat.valueOf("3.4E38"),
            JShort.valueOf("-32768") -> JShort.valueOf("32767"),
            new JBigDecimal("1.8E+308") -> JByte.valueOf("0"),
            JDouble.valueOf("1.7E-308") -> JDouble.valueOf("1.7E308")))
          val js = sj.render(inst)
          parseJValue(js) should matchJson(parseJValue("""{"m":{"0":9923372036854755810,"-2147483648":2147483647,"-9223372036854775808":9223372036854755807,"-128":127,"3.4E-38":3.4E38,"-32768":32767,"1.8E+308":0.0,"1.7E-308":1.7E308}}"""))
          val read = sj.read[SampleJNumber](js)
          //          val sb = new StringBuffer()
          //          read.m.map {
          //            case (k, v) =>
          //              sb.append(k + " / " + k.getClass.getName + " --> " + v + " / " + v.getClass.getName+"\n")
          //          }
          assertResult(result) { read }
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
      describe("Time DelimSpec:") {
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
          assertResult("""{"m":{"2007-12-03T10:15:30+01:00[Europe/Paris]":null}}""") { js }
          assertResult(inst) {
            sj.read[SampleZonedDateTime](js)
          }
        }
      }
    }
    describe("--- Negative Tests ---") {
      describe("Simple DelimSpec:") {
        it("Bad BigDecimal Key") {
          val js = """{"m":{"fred":1,"789.123":2}}"""
          val msg = """[<tokenizing>]: Unexpected character 'f' at position 0
                      |fred
                      |^""".stripMargin
          the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJBigDecimal](js) should have message msg
        }
        it("Bad BigInt Key") {
          val js = """{"m":{"fred":1,"789":2}}"""
          val msg = """[<tokenizing>]: Unexpected character 'f' at position 0
                      |fred
                      |^""".stripMargin
          the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJBigInteger](js) should have message msg
        }
        it("Bad Boolean Key") {
          val js = """{"m":{"true":false,"123":true}}"""
          val msg = """[$.m.(map key)]: Expected Boolean here but found Number
                      |123
                      |--^""".stripMargin
          the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJBoolean](js) should have message msg
        }
        it("Bad Byte Key") {
          val js = """{"m":{"16":2,"4x8":9}}"""
          val msg = """[<tokenizing>]: Unexpected character x at position 1
                      |4x8
                      |-^""".stripMargin
          the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJByte](js) should have message msg
        }
        it("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
          val js = """{"m":{null:"A","z":"Z"}}"""
          val msg = """[$.m]: Map keys cannot be null
                      |{"m":{null:"A","z":"Z"}}
                      |----------^""".stripMargin
          the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleJChar](js) should have message msg
        }
        it("Bad Double Key") {
          val js = """{"m":{"12.34":56.78,"true":34.56}}"""
          val msg = """[$.m.(map key)]: Expected Number here but found Boolean
                      |true
                      |---^""".stripMargin
          the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJDouble](js) should have message msg
        }
        it("Bad Float Key") {
          val js = """{"m":{"12.34":56.78,"90.12.3":34.56}}"""
          val msg = """[$.m.(map key)]: Unable to read value (e.g. bad number format)
                      |90.12.3
                      |------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleJFloat](js) should have message msg
        }
        it("Bad Int Key") {
          val js = """{"m":{"12.0":56,"90":34}}"""
          val msg = """[$.m.(map key)]: Unable to read value (e.g. bad number format)
                      |12.0
                      |---^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleJInteger](js) should have message msg
        }
        it("Bad Long Key") {
          val js = """{"m":{"12":56,"hey":34}}"""
          val msg = """[<tokenizing>]: Unexpected character h at position 0
                      |hey
                      |^""".stripMargin
          the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJLong](js) should have message msg
        }
        it("Bad Number Key") {
          val js = """{"m":{"flume":9923372036854755810,"-2147483648":2147483647,"-9223372036854775808":9223372036854755807,"-128":127,"3.4E-38":3.4E38,"-32768":32767,"1.8E+308":0.0,"1.7E-308":1.7E308}}"""
          val msg = """[<tokenizing>]: Unexpected character 'f' at position 0
                      |flume
                      |^""".stripMargin
          the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleJNumber](js) should have message msg
        }
      }
      describe("Time DelimSpec:") {
        it("Bad Duration Key") {
          val js = """{"m":{"PT0SXXX":"PT51H4M"}}"""
          val msg = """[$.m.(map key)]: Failed to parse Duration from input 'PT0SXXX'
                      |{"m":{"PT0SXXX":"PT51H4M"}}
                      |-------------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleDuration](js) should have message msg
        }
        it("Bad Instant Key") {
          val js = """{"m":{"1970-01-01T00:00:00Z":"+1000000000-12-31T23:59:59.999999999Z","bogus":"2007-12-03T10:15:30Z"}}"""
          val msg = """[$.m.(map key)]: Failed to parse Instant from input 'bogus'
                      |...0Z":"+1000000000-12-31T23:59:59.999999999Z","bogus":"2007-12-03T10:15:30Z"}}
                      |----------------------------------------------------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleInstant](js) should have message msg
        }
        it("Bad LocalDateTime Key") {
          val js = """{"m":{"+999999999-12-31T23:59:59.999999999":"-999999999-01-01T00:00:00","bogus":null}}"""
          val msg = """[$.m.(map key)]: Failed to parse LocalDateTime from input 'bogus'
                      |...9:59.999999999":"-999999999-01-01T00:00:00","bogus":null}}
                      |----------------------------------------------------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleLocalDateTime](js) should have message msg
        }
        it("Bad LocalDate Key") {
          val js = """{"m":{"bogus":"-999999999-01-01","2007-12-03":null}}"""
          val msg = """[$.m.(map key)]: Failed to parse LocalDate from input 'bogus'
                      |{"m":{"bogus":"-999999999-01-01","2007-12-03":null}}
                      |-----------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleLocalDate](js) should have message msg
        }
        it("Bad LocalTime Key") {
          val js = """{"m":{"23:59:59.999999999":"00:00:00","nada":"12:00:00","10:15:30":null}}"""
          val msg = """[$.m.(map key)]: Failed to parse LocalTime from input 'nada'
                      |{"m":{"23:59:59.999999999":"00:00:00","nada":"12:00:00","10:15:30":null}}
                      |------------------------------------------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleLocalTime](js) should have message msg
        }
        it("Bad OffsetDateTime Key") {
          val js = """{"m":{"false":"-999999999-01-01T00:00:00+18:00","2007-12-03T10:15:30+01:00":null}}"""
          val msg = """[$.m.(map key)]: Failed to parse OffsetDateTime from input 'false'
                      |{"m":{"false":"-999999999-01-01T00:00:00+18:00","2007-12-03T10:15:30+01:00":n...
                      |-----------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleOffsetDateTime](js) should have message msg
        }
        it("Bad OffsetTime Key") {
          val js = """{"m":{"2007-12-03T10:15:30+01:00[Europe\/Bogus]":null}}"""
          val msg = """[$.m.(map key)]: Unable to read value (e.g. bad number format)
                      |{"m":{"2007-12-03T10:15:30+01:00[Europe\/Bogus]":null}}
                      |----------------------------------------------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleOffsetTime](js) should have message msg
        }
        it("Bad Period Key") {
          val js = """{"m":{"P0D???":"P1Y2M3D"}}"""
          val msg = """[$.m.(map key)]: Failed to parse Period from input 'P0D???'
                      |{"m":{"P0D???":"P1Y2M3D"}}
                      |------------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SamplePeriod](js) should have message msg
        }
        it("Bad ZonedDateTime Key") {
          val js = """{"m":{"FRED23:59:59.999999999-18:00":"00:00:00+18:00","10:15:30+01:00":null}}"""
          val msg = """[$.m.(map key)]: Failed to parse ZonedDateTime from input 'FRED23:59:59.999999999-18:00'
                      |{"m":{"FRED23:59:59.999999999-18:00":"00:00:00+18:00","10:15:30+01:00":null}}
                      |----------------------------------^""".stripMargin
          the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleZonedDateTime](js) should have message msg
        }
      }
    }
  }
}
