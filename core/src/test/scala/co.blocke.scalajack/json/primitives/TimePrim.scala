package co.blocke.scalajack
package json.primitives

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec
import java.time._

class TimePrim() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe("--------------------------\n:  Time Primitive Tests  :\n--------------------------") {
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
        assertResult("""{"o1":"2007-12-03T10:15:30+01:00[Europe/Paris]","o2":null}""") { js }
        assertResult(inst) {
          sj.read[SampleZonedDateTime](js)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Duration must break") {
        val js = """{"d1":"PT0S","d2":21,"d3":null}"""
        val msg = """[$.d2]: Expected String here but found Number
                    |{"d1":"PT0S","d2":21,"d3":null}
                    |-------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleDuration](js) should have message msg
        val js2 = """{"d1":"PT0S","d2":"bogus","d3":null}"""
        val msg2 = """[$.d2]: Failed to parse Duration from input 'bogus'
                    |{"d1":"PT0S","d2":"bogus","d3":null}
                    |-----------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleDuration](js2) should have message msg2
      }
      it("Instant must break") {
        val js = """{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
        val msg = """[$.i2]: Expected String here but found Boolean
                    |{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i...
                    |--------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleInstant](js) should have message msg
        val js2 = """{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}"""
        val msg2 = """[$.i2]: Failed to parse Instant from input 'bogus'
                    |{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z",...
                    |---------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleInstant](js2) should have message msg2
      }
      it("LocalDateTime must break") {
        val js = """{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}"""
        val msg = """[$.d1]: Expected String here but found Number
                    |{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}
                    |-------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleLocalDateTime](js) should have message msg
        val js2 = """{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d1":null}"""
        val msg2 = """[$.d1]: Failed to parse LocalDateTime from input 'bogus'
                    |{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d1...
                    |-----------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleLocalDateTime](js2) should have message msg2
      }
      it("LocalDate must break") {
        val js = """{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
        val msg = """[$.d1]: Expected String here but found Number
                    |{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}
                    |-------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleLocalDate](js) should have message msg
        val js2 = """{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}"""
        val msg2 = """[$.d1]: Failed to parse LocalDate from input 'bogus'
                    |{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}
                    |-----------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleLocalDate](js2) should have message msg2
      }
      it("LocalTime must break") {
        val js = """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}"""
        val msg = """[$.d5]: Expected String here but found Boolean
                    |...:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleLocalTime](js) should have message msg
        val js2 = """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}"""
        val msg2 = """[$.d5]: Failed to parse LocalTime from input 'Bogus'
                    |...00:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleLocalTime](js2) should have message msg2
      }
      it("OffsetDateTime must break") {
        val js = """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30+01:00","o4":null}"""
        val msg = """[$.o2]: Expected String here but found Number
                    |..."+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30...
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleOffsetDateTime](js) should have message msg
        val js2 = """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}"""
        val msg2 = """[$.o2]: Failed to parse OffsetDateTime from input '-999999999-01T00:00:00+18:00'
                    |...99999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10:15:3...
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleOffsetDateTime](js2) should have message msg2
      }
      it("OffsetTime must break") {
        val js = """{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}"""
        val msg = """[$.o2]: Expected String here but found Boolean
                    |{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}
                    |------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleOffsetTime](js) should have message msg
        val js2 = """{"o1":"23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o4":null}"""
        val msg2 = """[$.o2]: Failed to parse OffsetTime from input '00:00:00:00+18:00'
                    |..."23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o...
                    |----------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleOffsetTime](js2) should have message msg2
      }
      it("Period must break") {
        val js = """{"p1":"P0D","p2":5,"p3":null}"""
        val msg = """[$.p2]: Expected String here but found Number
                    |{"p1":"P0D","p2":5,"p3":null}
                    |-----------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SamplePeriod](js) should have message msg
        val js2 = """{"p1":"P0D","p2":"bogus","p3":null}"""
        val msg2 = """[$.p2]: Failed to parse Period from input 'bogus'
                    |{"p1":"P0D","p2":"bogus","p3":null}
                    |----------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SamplePeriod](js2) should have message msg2
      }
      it("ZonedDateTime must break") {
        val js = """{"o1":true,"o2":null}"""
        val msg = """[$.o1]: Expected String here but found Boolean
                    |{"o1":true,"o2":null}
                    |---------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleZonedDateTime](js) should have message msg
        val js2 = """{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}"""
        val msg2 = """[$.o1]: Failed to parse ZonedDateTime from input '2007-12-03T10:15:30+01:00 Earth'
                    |{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}
                    |-------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleZonedDateTime](js2) should have message msg2
      }
    }
  }
}
