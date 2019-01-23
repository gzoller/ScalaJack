package co.blocke.scalajack
package json.test.primitives

import org.scalatest.{ FunSpec, Matchers }
import java.time._

import TestUtil._
import co.blocke.scalajack.util.Path

class TimePrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("---------------------------\n:  Time Primitives Tests  :\n---------------------------") {
    describe("+++ Positive Tests +++") {
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
    describe("--- Negative Tests ---") {
      it("Duration must break") {
        val js = """{"d1":"PT0S","d2":21,"d3":null}"""
        assert(expectUnexpected(() => sj.read[SampleDuration](js), Path.Root \ "d2", List("Number")))
        val js2 = """{"d1":"PT0S","d2":"bogus","d3":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleDuration](js2), Path.Root \ "d2", List.empty[String]))
      }
      it("Instant must break") {
        val js = """{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
        assert(expectUnexpected(() => sj.read[SampleInstant](js), Path.Root \ "i2", List("False")))
        val js2 = """{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleInstant](js2), Path.Root \ "i2", List.empty[String]))
      }
      it("LocalDateTime must break") {
        val js = """{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}"""
        assert(expectUnexpected(() => sj.read[SampleLocalDateTime](js), Path.Root \ "d1", List("Number")))
        val js2 = """{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d1":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleLocalDateTime](js2), Path.Root \ "d1", List.empty[String]))
      }
      it("LocalDate must break") {
        val js = """{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
        assert(expectUnexpected(() => sj.read[SampleLocalDate](js), Path.Root \ "d1", List("Number")))
        val js2 = """{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleLocalDate](js2), Path.Root \ "d1", List.empty[String]))
      }
      it("LocalTime must break") {
        val js = """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}"""
        assert(expectUnexpected(() => sj.read[SampleLocalTime](js), Path.Root \ "d5", List("False")))
        val js2 = """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleLocalTime](js2), Path.Root \ "d5", List.empty[String]))
      }
      it("OffsetDateTime must break") {
        val js = """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30+01:00","o4":null}"""
        assert(expectUnexpected(() => sj.read[SampleOffsetDateTime](js), Path.Root \ "o2", List("Number")))
        val js2 = """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleOffsetDateTime](js2), Path.Root \ "o2", List.empty[String]))
      }
      it("OffsetTime must break") {
        val js = """{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}"""
        assert(expectUnexpected(() => sj.read[SampleOffsetTime](js), Path.Root \ "o2", List("False")))
        val js2 = """{"o1":"23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o4":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleOffsetTime](js2), Path.Root \ "o2", List.empty[String]))
      }
      it("Period must break") {
        val js = """{"p1":"P0D","p2":5,"p3":null}"""
        assert(expectUnexpected(() => sj.read[SamplePeriod](js), Path.Root \ "p2", List("Number")))
        val js2 = """{"p1":"P0D","p2":"bogus","p3":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SamplePeriod](js2), Path.Root \ "p2", List.empty[String]))
      }
      it("ZonedDateTime must break") {
        val js = """{"o1":true,"o2":null}"""
        assert(expectUnexpected(() => sj.read[SampleZonedDateTime](js), Path.Root \ "o1", List("True")))
        val js2 = """{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}"""
        assert(expectMalformed[java.time.format.DateTimeParseException](() => sj.read[SampleZonedDateTime](js2), Path.Root \ "o1", List.empty[String]))
      }
    }
  }
}
