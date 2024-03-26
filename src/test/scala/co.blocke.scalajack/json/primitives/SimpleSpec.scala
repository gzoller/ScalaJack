package co.blocke.scalajack
package json
package primitives

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*
import java.time.*
import java.util.UUID
import java.net.{URI, URL}

class SimpleSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-----------------------\n:  Simple Type Tests  :\n-----------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {

      it("Duration must work") {
        val inst = SampleDuration(Duration.ZERO, Duration.parse("P2DT3H4M"), null)
        val js = sjCodecOf[SampleDuration].toJson(inst)
        js should matchJson("""{"d1":"PT0S","d2":"PT51H4M","d3":null}""")
        sjCodecOf[SampleDuration].fromJson(js) shouldEqual (inst)
      }

      it("Instant must work") {
        val inst = SampleInstant(
          Instant.EPOCH,
          Instant.MAX,
          Instant.MIN,
          Instant.parse("2007-12-03T10:15:30.00Z"),
          null
        )
        val js = sjCodecOf[SampleInstant].toJson(inst)
        js should matchJson("""{"i1":"1970-01-01T00:00:00Z","i2":"+1000000000-12-31T23:59:59.999999999Z","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}""")
        sjCodecOf[SampleInstant].fromJson(js) shouldEqual (inst)
      }

      it("LocalDateTime must work") {
        val inst = SampleLocalDateTime(
          LocalDateTime.MAX,
          LocalDateTime.MIN,
          LocalDateTime.parse("2007-12-03T10:15:30"),
          null
        )
        val js = sjCodecOf[SampleLocalDateTime].toJson(inst)
        js should matchJson("""{"d1":"+999999999-12-31T23:59:59.999999999","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}""")
        sjCodecOf[SampleLocalDateTime].fromJson(js) shouldEqual (inst)
      }

      it("LocalDate must work") {
        val inst = SampleLocalDate(
          LocalDate.MAX,
          LocalDate.MIN,
          LocalDate.parse("2007-12-03"),
          null
        )
        val js = sjCodecOf[SampleLocalDate].toJson(inst)
        js should matchJson("""{"d1":"+999999999-12-31","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}""")
        sjCodecOf[SampleLocalDate].fromJson(js) shouldEqual (inst)
      }

      it("LocalTime must work") {
        val inst = SampleLocalTime(
          LocalTime.MAX,
          LocalTime.MIN,
          LocalTime.MIDNIGHT,
          LocalTime.NOON,
          LocalTime.parse("10:15:30"),
          null
        )
        val js = sjCodecOf[SampleLocalTime].toJson(inst)
        js should matchJson("""{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"10:15:30","d6":null}""")
        sjCodecOf[SampleLocalTime].fromJson(js) shouldEqual (inst)
      }

      it("MonthDay must work") {
        val inst = SampleMonthDay(
          MonthDay.of(7, 1),
          null
        )
        val js = sjCodecOf[SampleMonthDay].toJson(inst)
        js should matchJson("""{"m1":"--07-01","m2":null}""")
        sjCodecOf[SampleMonthDay].fromJson(js) shouldEqual (inst)
      }

      it("OffsetDateTime must work") {
        val inst = SampleOffsetDateTime(
          OffsetDateTime.MAX,
          OffsetDateTime.MIN,
          OffsetDateTime.parse("2007-12-03T10:15:30+01:00"),
          null
        )
        val js = sjCodecOf[SampleOffsetDateTime].toJson(inst)
        js should matchJson("""{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}""")
        sjCodecOf[SampleOffsetDateTime].fromJson(js) shouldEqual (inst)
      }

      it("OffsetTime must work") {
        val inst = SampleOffsetTime(
          OffsetTime.MAX,
          OffsetTime.MIN,
          OffsetTime.parse("10:15:30+01:00"),
          null
        )
        val js = sjCodecOf[SampleOffsetTime].toJson(inst)
        js should matchJson("""{"o1":"23:59:59.999999999-18:00","o2":"00:00:00+18:00","o3":"10:15:30+01:00","o4":null}""")
        sjCodecOf[SampleOffsetTime].fromJson(js) shouldEqual (inst)
      }

      it("Period must work") {
        val inst = SamplePeriod(Period.ZERO, Period.parse("P1Y2M3D"), null)
        val js = sjCodecOf[SamplePeriod].toJson(inst)
        js should matchJson("""{"p1":"P0D","p2":"P1Y2M3D","p3":null}""")
        sjCodecOf[SamplePeriod].fromJson(js) shouldEqual (inst)
      }

      it("Year must work") {
        val inst = SampleYear(Year.of(Year.MAX_VALUE), Year.of(Year.MIN_VALUE), Year.parse("2020"), null)
        val js = sjCodecOf[SampleYear].toJson(inst)
        js should matchJson("""{"y1":"999999999","y2":"-999999999","y3":"2020","y4":null}""")
        sjCodecOf[SampleYear].fromJson(js) shouldEqual (inst)
      }

      it("YearMonth must work") {
        val inst = SampleYearMonth(YearMonth.of(2020, 7), null)
        val js = sjCodecOf[SampleYearMonth].toJson(inst)
        js should matchJson("""{"y1":"2020-07","y2":null}""")
        sjCodecOf[SampleYearMonth].fromJson(js) shouldEqual (inst)
      }

      it("ZonedDateTime must work") {
        val inst = SampleZonedDateTime(
          ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]"),
          null
        )
        val js = sjCodecOf[SampleZonedDateTime].toJson(inst)
        js should matchJson("""{"o1":"2007-12-03T10:15:30+01:00[Europe/Paris]","o2":null}""")
        sjCodecOf[SampleZonedDateTime].fromJson(js) shouldEqual (inst)
      }

      it("ZonedId must work") {
        val inst = SampleZoneId(
          ZoneId.of("America/Puerto_Rico"),
          null
        )
        val js = sjCodecOf[SampleZoneId].toJson(inst)
        js should matchJson("""{"z1":"America/Puerto_Rico","z2":null}""")
        sjCodecOf[SampleZoneId].fromJson(js) shouldEqual (inst)
      }

      it("ZoneOffset must work") {
        val ldt = LocalDateTime.parse("2007-12-03T10:15:30")
        val zone = ZoneId.of("Europe/Berlin")
        val zoneOffSet = zone.getRules().getOffset(ldt)
        val inst = SampleZoneOffset(
          null,
          zoneOffSet
        )
        val js = sjCodecOf[SampleZoneOffset].toJson(inst)
        js should matchJson("""{"z1":null,"z2":"+01:00"}""")
        sjCodecOf[SampleZoneOffset].fromJson(js) shouldEqual (inst)
      }

      it("Net types URL and URI must work") {
        val inst = SampleNet(
          null,
          new URL("https://www.foom.com"),
          null,
          new URI("https://www.foom.com")
        )
        val js = sjCodecOf[SampleNet].toJson(inst)
        js should matchJson("""{"u1":null,"u2":"https://www.foom.com","u3":null,"u4":"https://www.foom.com"}""")
        sjCodecOf[SampleNet].fromJson(js) shouldEqual (inst)
      }

      it("UUID must work") {
        val inst = SampleUUID(
          null,
          UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9")
        )
        val js = sjCodecOf[SampleUUID].toJson(inst)
        js should matchJson("""{"u1":null,"u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}""")
        sjCodecOf[SampleUUID].fromJson(js) shouldEqual (inst)
      }
    }

    describe(colorString("+++ Negative Tests +++")) {
      it("Duration must break") {
        val js = """{"d1":"PT0S","d2":21,"d3":null}"""
        val msg = """Expected a String value but got '2' at position [18]
                  |{"d1":"PT0S","d2":21,"d3":null}
                  |------------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleDuration].fromJson(js))
        ex.show shouldEqual msg

        val js2 = """{"d1":"PT0S","d2":"bogus","d3":null}"""
        val msg2 = """Failed to parse Duration from input 'bogus'
                    |{"d1":"PT0S","d2":"bogus","d3":null}
                    |------------------------^""".stripMargin
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleDuration].fromJson(js2) should have message """Text cannot be parsed to a Duration"""
      }

      it("Instant must break") {
        val js =
          """{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
        val msg =
          """Expected a String value but got 'f' at position [34]
                  |{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i...
                  |----------------------------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleInstant].fromJson(js))
        ex.show shouldEqual msg

        val js2 =
          """{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleInstant].fromJson(js2) should have message """Text 'bogus' could not be parsed at index 0"""
      }

      it("LocalDateTime must break") {
        val js =
          """{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}"""
        val msg =
          """Expected a String value but got '-' at position [6]
                  |{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}
                  |------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleLocalDateTime].fromJson(js))
        ex.show shouldEqual msg
        val js2 =
          """{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d1":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleLocalDateTime].fromJson(js2) should have message """Text 'bogus' could not be parsed at index 0"""
      }

      it("LocalDate must break") {
        val js =
          """{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
        val msg = """Expected a String value but got '-' at position [6]
                  |{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}
                  |------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleLocalDate].fromJson(js))
        ex.show shouldEqual msg
        val js2 =
          """{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleLocalDate].fromJson(js2) should have message """Text 'bogus' could not be parsed at index 0"""
      }

      it("LocalTime must break") {
        val js =
          """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}"""
        val msg =
          """Expected a String value but got 'f' at position [80]
                  |...:"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}
                  |----------------------------------------------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleLocalTime].fromJson(js))
        ex.show shouldEqual msg
        val js2 =
          """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleLocalTime].fromJson(js2) should have message """Text 'Bogus' could not be parsed at index 0"""
      }

      it("MonthDay must break") {
        val js = """{"m1":25,"m2":null}"""
        val msg = """Expected a String value but got '2' at position [6]
                  |{"m1":25,"m2":null}
                  |------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleMonthDay].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"m1":"R-07-01","m2":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleMonthDay].fromJson(js2) should have message """Text 'R-07-01' could not be parsed at index 0"""
      }

      it("OffsetDateTime must break") {
        val js =
          """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30+01:00","o4":null}"""
        val msg =
          """Expected a String value but got '2' at position [55]
                  |..."+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30...
                  |----------------------------------------------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleOffsetDateTime].fromJson(js))
        ex.show shouldEqual msg
        val js2 =
          """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleOffsetDateTime].fromJson(js2) should have message """Text '-999999999-01T00:00:00+18:00' could not be parsed at index 13"""
      }

      it("OffsetTime must break") {
        val js =
          """{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}"""
        val msg =
          """Expected a String value but got 'f' at position [38]
                  |{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}
                  |--------------------------------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleOffsetTime].fromJson(js))
        ex.show shouldEqual msg
        val js2 =
          """{"o1":"23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o4":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleOffsetTime].fromJson(js2) should have message """Text '00:00:00:00+18:00' could not be parsed at index 8"""
      }

      it("Period must break") {
        val js = """{"p1":"P0D","p2":5,"p3":null}"""
        val msg = """Expected a String value but got '5' at position [17]
                  |{"p1":"P0D","p2":5,"p3":null}
                  |-----------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SamplePeriod].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"p1":"P0D","p2":"bogus","p3":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SamplePeriod].fromJson(js2) should have message """Text cannot be parsed to a Period"""
      }

      it("Year must break") {
        val js = """{"y1":"999999999","y2":5,"y3":"2020","y4":null}"""
        val msg = """Expected a String value but got '5' at position [23]
                  |{"y1":"999999999","y2":5,"y3":"2020","y4":null}
                  |-----------------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleYear].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"y1":"999999999","y2":"bogus","y3":"2020","y4":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleYear].fromJson(js2) should have message """Text 'bogus' could not be parsed at index 0"""
      }

      it("YearMonth must break") {
        val js = """{"y1":true,"y2":null}"""
        val msg = """Expected a String value but got 't' at position [6]
                  |{"y1":true,"y2":null}
                  |------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleYearMonth].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"y1":"bogus","y2":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleYearMonth].fromJson(js2) should have message """Text 'bogus' could not be parsed at index 0"""
      }

      it("ZonedId must break") {
        val js = """{"z1":-3,"z2":null}"""
        val msg = """Expected a String value but got '-' at position [6]
                  |{"z1":-3,"z2":null}
                  |------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleZoneId].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"z1":"Mars/Puerto_Rico","z2":null}"""
        the[java.time.zone.ZoneRulesException] thrownBy sjCodecOf[SampleZoneId].fromJson(js2) should have message """Unknown time-zone ID: Mars/Puerto_Rico"""
      }

      it("ZonedDateTime must break") {
        val js = """{"o1":true,"o2":null}"""
        val msg = """Expected a String value but got 't' at position [6]
                  |{"o1":true,"o2":null}
                  |------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleZonedDateTime].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}"""
        the[java.time.format.DateTimeParseException] thrownBy sjCodecOf[SampleZonedDateTime].fromJson(js2) should have message """Text '2007-12-03T10:15:30+01:00 Earth' could not be parsed, unparsed text found at index 25"""
      }

      it("Net types URL and URI must break") {
        val js = """{"u1":null,"u2":13,"u3":null,"u4":"https://www.foom.com"}"""
        val msg = """Expected a String value but got '1' at position [16]
                  |{"u1":null,"u2":13,"u3":null,"u4":"https://www.foom.com"}
                  |----------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleNet].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"u1":null,"u2":"httpwww.foom.com","u3":null,"u4":"https://www.foom.com"}"""
        the[java.net.MalformedURLException] thrownBy sjCodecOf[SampleNet].fromJson(js2) should have message """no protocol: httpwww.foom.com"""
      }

      it("UUID must break") {
        val js = """{"u1":null,"u2":[1,2,3]}"""
        val msg = """Expected a String value but got '[' at position [16]
                  |{"u1":null,"u2":[1,2,3]}
                  |----------------^""".stripMargin
        val ex = intercept[co.blocke.scalajack.json.JsonParseError](sjCodecOf[SampleUUID].fromJson(js))
        ex.show shouldEqual msg
        val js2 = """{"u1":null,"u2":"bogus"}"""
        the[java.lang.IllegalArgumentException] thrownBy sjCodecOf[SampleUUID].fromJson(js2) should have message """Invalid UUID string: bogus"""
      }
    }
  }
