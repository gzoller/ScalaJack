package co.blocke.scalajack
package xml
package primitives

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import TestUtil.*

import java.net.{URI, URL}
import java.time.*
import java.util.UUID

class SimpleSpec() extends AnyFunSpec:

  describe(colorString("-----------------------\n:  Simple Type (XML)  :\n-----------------------", Console.YELLOW)) {

    describe(colorString("+++ Positive Tests +++")) {

      it("Duration must work") {
        val inst = SampleDuration(Duration.ZERO, Duration.parse("P2DT3H4M"), null)
        val sj = sjXmlCodecOf[SampleDuration]
        val x = sj.toXml(inst)
        x should equal("""<SampleDuration><d1>PT0S</d1><d2>PT51H4M</d2><d3>null</d3></SampleDuration>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("Instant must work") {
        val inst = SampleInstant(
          Instant.EPOCH,
          Instant.MAX,
          Instant.MIN,
          Instant.parse("2007-12-03T10:15:30.00Z"),
          null
        )
        val sj = sjXmlCodecOf[SampleInstant]
        val x = sj.toXml(inst)
        x should equal("""<SampleInstant><i1>1970-01-01T00:00:00Z</i1><i2>+1000000000-12-31T23:59:59.999999999Z</i2><i3>-1000000000-01-01T00:00:00Z</i3><i4>2007-12-03T10:15:30Z</i4><i5>null</i5></SampleInstant>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("LocalDateTime must work") {
        val inst = SampleLocalDateTime(
          LocalDateTime.MAX,
          LocalDateTime.MIN,
          LocalDateTime.parse("2007-12-03T10:15:30"),
          null
        )
        val sj = sjXmlCodecOf[SampleLocalDateTime]
        val x = sj.toXml(inst)
        x should equal("""<SampleLocalDateTime><d1>+999999999-12-31T23:59:59.999999999</d1><d2>-999999999-01-01T00:00</d2><d3>2007-12-03T10:15:30</d3><d4>null</d4></SampleLocalDateTime>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("LocalDate must work") {
        val inst = SampleLocalDate(
          LocalDate.MAX,
          LocalDate.MIN,
          LocalDate.parse("2007-12-03"),
          null
        )
        val sj = sjXmlCodecOf[SampleLocalDate]
        val x = sj.toXml(inst)
        x should equal("""<SampleLocalDate><d1>+999999999-12-31</d1><d2>-999999999-01-01</d2><d3>2007-12-03</d3><d4>null</d4></SampleLocalDate>""")
        sj.fromXml(x) shouldEqual (inst)
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
        val sj = sjXmlCodecOf[SampleLocalTime]
        val x = sj.toXml(inst)
        x should equal("""<SampleLocalTime><d1>23:59:59.999999999</d1><d2>00:00</d2><d3>00:00</d3><d4>12:00</d4><d5>10:15:30</d5><d6>null</d6></SampleLocalTime>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("MonthDay must work") {
        val inst = SampleMonthDay(
          MonthDay.of(7, 1),
          null
        )
        val sj = sjXmlCodecOf[SampleMonthDay]
        val x = sj.toXml(inst)
        x should equal("""<SampleMonthDay><m1>--07-01</m1><m2>null</m2></SampleMonthDay>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("OffsetDateTime must work") {
        val inst = SampleOffsetDateTime(
          OffsetDateTime.MAX,
          OffsetDateTime.MIN,
          OffsetDateTime.parse("2007-12-03T10:15:30+01:00"),
          null
        )
        val sj = sjXmlCodecOf[SampleOffsetDateTime]
        val x = sj.toXml(inst)
        x should equal("""<SampleOffsetDateTime><o1>+999999999-12-31T23:59:59.999999999-18:00</o1><o2>-999999999-01-01T00:00+18:00</o2><o3>2007-12-03T10:15:30+01:00</o3><o4>null</o4></SampleOffsetDateTime>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("OffsetTime must work") {
        val inst = SampleOffsetTime(
          OffsetTime.MAX,
          OffsetTime.MIN,
          OffsetTime.parse("10:15:30+01:00"),
          null
        )
        val sj = sjXmlCodecOf[SampleOffsetTime]
        val x = sj.toXml(inst)
        x should equal("""<SampleOffsetTime><o1>23:59:59.999999999-18:00</o1><o2>00:00+18:00</o2><o3>10:15:30+01:00</o3><o4>null</o4></SampleOffsetTime>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("Period must work") {
        val inst = SamplePeriod(Period.ZERO, Period.parse("P1Y2M3D"), null)
        val sj = sjXmlCodecOf[SamplePeriod]
        val x = sj.toXml(inst)
        x should equal("""<SamplePeriod><p1>P0D</p1><p2>P1Y2M3D</p2><p3>null</p3></SamplePeriod>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("Year must work") {
        val inst = SampleYear(Year.of((Year.MAX_VALUE)), Year.of((Year.MIN_VALUE)), Year.parse("2020"), null)
        val sj = sjXmlCodecOf[SampleYear]
        val x = sj.toXml(inst)
        x should equal("""<SampleYear><y1>999999999</y1><y2>-999999999</y2><y3>2020</y3><y4>null</y4></SampleYear>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("YearMonth must work") {
        val inst = SampleYearMonth(YearMonth.of(2020, 7), null)
        val sj = sjXmlCodecOf[SampleYearMonth]
        val x = sj.toXml(inst)
        x should equal("""<SampleYearMonth><y1>2020-07</y1><y2>null</y2></SampleYearMonth>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("ZonedDateTime must work") {
        val inst = SampleZonedDateTime(
          ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]"),
          null
        )
        val sj = sjXmlCodecOf[SampleZonedDateTime]
        val x = sj.toXml(inst)
        x should equal("""<SampleZonedDateTime><o1>2007-12-03T10:15:30+01:00[Europe/Paris]</o1><o2>null</o2></SampleZonedDateTime>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("ZonedId must work") {
        val inst = SampleZoneId(
          ZoneId.of("America/Puerto_Rico"),
          null
        )
        val sj = sjXmlCodecOf[SampleZoneId]
        val x = sj.toXml(inst)
        x should equal("""<SampleZoneId><z1>America/Puerto_Rico</z1><z2>null</z2></SampleZoneId>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("ZoneOffset must work") {
        val ldt = LocalDateTime.parse("2007-12-03T10:15:30")
        val zone = ZoneId.of("Europe/Berlin")
        val zoneOffSet = zone.getRules.getOffset(ldt)
        val inst = SampleZoneOffset(
          null,
          zoneOffSet
        )
        val sj = sjXmlCodecOf[SampleZoneOffset]
        val x = sj.toXml(inst)
        x should equal("""<SampleZoneOffset><z1>null</z1><z2>+01:00</z2></SampleZoneOffset>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("Net types URL and URI must work") {
        val inst = SampleNet(
          null,
          new URI("https://www.foom.com").toURL,
          null,
          new URI("https://www.foom.com")
        )
        val sj = sjXmlCodecOf[SampleNet]
        val x = sj.toXml(inst)
        x should equal("""<SampleNet><u1>null</u1><u2>https://www.foom.com</u2><u3>null</u3><u4>https://www.foom.com</u4></SampleNet>""")
        sj.fromXml(x) shouldEqual (inst)
      }

      it("UUID must work") {
        val inst = SampleUUID(
          null,
          UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9")
        )
        val sj = sjXmlCodecOf[SampleUUID]
        val x = sj.toXml(inst)
        x should equal("""<SampleUUID><u1>null</u1><u2>580afe0d-81c0-458f-9e09-4486c7af0fe9</u2></SampleUUID>""")
        sj.fromXml(x) shouldEqual (inst)
      }
    }

    describe(colorString("+++ Negative Tests +++")) {
      it("Duration must break") {
        val sj = sjXmlCodecOf[SampleDuration]
        val x = """<SampleDuration><d1>PT0S</d1><d2>21</d2><d3>null</d3></SampleDuration>"""
        val msg = """Can't parse Duration: '21'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("Instant must break") {
        val sj = sjXmlCodecOf[SampleInstant]
        val x = """<SampleInstant><i1>1970-01-01T00:00:00Z</i1><i2>false</i2><i3>-1000000000-01-01T00:00:00Z</i3><i4>2007-12-03T10:15:30Z</i4><i5>null</i5></SampleInstant>"""
        val msg = """Can't parse Instant: 'false'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("LocalDateTime must break") {
        val sj = sjXmlCodecOf[SampleLocalDateTime]
        val x = """<SampleLocalDateTime><d1>-1</d1><d2>-999999999-01-01T00:00</d2><d3>2007-12-03T10:15:30</d3><d4>null</d4></SampleLocalDateTime>"""
        val msg = """Can't parse LocalDateTime: '-1'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("LocalDate must break") {
        val sj = sjXmlCodecOf[SampleLocalDate]
        val x = """<SampleLocalDate><d1>-1</d1><d2>-999999999-01-01</d2><d3>2007-12-03</d3><d4>null</d4></SampleLocalDate>"""
        val msg = """Can't parse LocalDate: '-1'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("LocalTime must break") {
        val sj = sjXmlCodecOf[SampleLocalTime]
        val x =
          """<SampleLocalTime><d1>23:59:59.999999999</d1><d2>00:00</d2><d3>00:00</d3><d4>12:00</d4><d5>false</d5><d6>null</d6></SampleLocalTime>"""
        val msg = """Can't parse LocalTime: 'false'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("MonthDay must break") {
        val sj = sjXmlCodecOf[SampleMonthDay]
        val x = """<SampleMonthDay><m1>25</m1><m2>null</m2></SampleMonthDay>"""
        val msg = """Can't parse MonthDay: '26'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
      }

      it("OffsetDateTime must break") {
        val sj = sjXmlCodecOf[SampleOffsetDateTime]
        val x =
          """<SampleOffsetDateTime><o1>+999999999-12-31T23:59:59.999999999-18:00</o1><o2>2</o2><o3>2007-12-03T10:15:30+01:00</o3><o4>null</o4></SampleOffsetDateTime>"""
        val msg = """Can't parse OffsetDateTime: '2'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
      }

      it("OffsetTime must break") {
        val sj = sjXmlCodecOf[SampleOffsetTime]
        val x =
          """<SampleOffsetTime><o1>23:59:59.999999999-18:00</o1><o2>false</o2><o3>10:15:30+01:00</o3><o4>null</o4></SampleOffsetTime>"""
        val msg = """Can't parse OffsetTime: 'false'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("Period must break") {
        val sj = sjXmlCodecOf[SamplePeriod]
        val x = """<SamplePeriod><p1>P0D</p1><p2>5</p2><p3>null</p3></SamplePeriod>"""
        val msg = """Can't parse Period: '5'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("Year must break") {
        val sj = sjXmlCodecOf[SampleYear]
        val x = """<SampleYear><y1>999999999</y1><y2>x5</y2><y3>2020</y3><y4>null</y4></SampleYear>"""
        val msg = """Can't parse Year: 'x5'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("YearMonth must break") {
        val sj = sjXmlCodecOf[SampleYearMonth]
        val x = """<SampleYearMonth><y1>true</y1><y2>null</y2></SampleYearMonth>"""
        val msg = """Can't parse YearMonth: 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("ZoneId must break") {
        val sj = sjXmlCodecOf[SampleZoneId]
        val x = """<SampleZoneId><z1>foo</z1><z2>null</z2></SampleZoneId>"""
        val msg = """Can't parse ZoneId: 'foo'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("ZonedDateTime must break") {
        val sj = sjXmlCodecOf[SampleZonedDateTime]
        val x = """<SampleZonedDateTime><o1>true</o1><o2>null</o2></SampleZonedDateTime>"""
        val msg = """Can't parse ZonedDateTime: 'true'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("Net types URL and URI must break") {
        val sj = sjXmlCodecOf[SampleNet]
        val x = """<SampleNet><u1>null</u1><u2>13</u2><u3>null</u3><u4>https://www.foom.com</u4></SampleNet>"""
        val msg = """Can't parse URL: '13'"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

      it("UUID must break") {
        val sj = sjXmlCodecOf[SampleUUID]
        val x = """<SampleUUID><u1>null</u1><u2><boom>stuff</boom></u2></SampleUUID>"""
        val msg = """Expected a value for UUID but found nothing"""
        val ex = intercept[co.blocke.scalajack.ParseError](sj.fromXml(x))
        ex.show.shouldEqual(msg)
      }

    }
  }
