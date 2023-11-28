package co.blocke.scalajack
package json
package primitives

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*
import java.time._
import java.util.UUID

class Simple() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-----------------------\n:  Simple Type Tests  :\n-----------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Duration must work") {
        val inst = SampleDuration(Duration.ZERO, Duration.parse("P2DT3H4M"), null)
        val js = sj[SampleDuration].toJson(inst)
        js should matchJson("""{"d1":"PT0S","d2":"PT51H4M","d3":null}""")
        // inst shouldEqual ScalaJack.read[SampleDuration](js)
      }

      it("Instant must work") {
        val inst = SampleInstant(
            Instant.EPOCH,
            Instant.MAX,
            Instant.MIN,
            Instant.parse("2007-12-03T10:15:30.00Z"),
            null
            )
        val js = sj[SampleInstant].toJson(inst)
        js should matchJson("""{"i1":"1970-01-01T00:00:00Z","i2":"+1000000000-12-31T23:59:59.999999999Z","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}""")
        // inst shouldEqual ScalaJack.read[SampleInstant](js)
      }

      it("LocalDate must work") {
        val inst = SampleLocalDate(
            LocalDate.MAX,
            LocalDate.MIN,
            LocalDate.parse("2007-12-03"),
            null
            )
        val js = sj[SampleLocalDate].toJson(inst)
        js should matchJson("""{"d1":"+999999999-12-31","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}""")
        // inst shouldEqual ScalaJack.read[SampleLocalDate](js)
      }

      it("LocalDateTime must work") {
        val inst = SampleLocalDateTime(
            LocalDateTime.MAX,
            LocalDateTime.MIN,
            LocalDateTime.parse("2007-12-03T10:15:30"),
            null
            )
        val js = sj[SampleLocalDateTime].toJson(inst)
        js should matchJson("""{"d1":"+999999999-12-31T23:59:59.999999999","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}""")
        // inst shouldEqual ScalaJack.read[SampleLocalDateTime](js)
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
        val js = sj[SampleLocalTime].toJson(inst)
        js should matchJson("""{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"10:15:30","d6":null}""")
        // inst shouldEqual ScalaJack.read[SampleLocalTime](js)
      }

      it("MonthDay must work") {
        val inst = SampleMonthDay(
            MonthDay.of(7,1),
            null
            )
        val js = sj[SampleMonthDay].toJson(inst)
        js should matchJson("""{"m1":"--07-01","m2":null}""")
        // inst shouldEqual ScalaJack.read[SampleMonthDay](js)
      }

      it("OffsetDateTime must work") {
        val inst = SampleOffsetDateTime(
            OffsetDateTime.MAX,
            OffsetDateTime.MIN,
            OffsetDateTime.parse("2007-12-03T10:15:30+01:00"),
            null
            )
        val js = sj[SampleOffsetDateTime].toJson(inst)
        js should matchJson("""{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}""")
        // inst shouldEqual ScalaJack.read[SampleOffsetDateTime](js)
      }

      it("OffsetTime must work") {
        val inst = SampleOffsetTime(
            OffsetTime.MAX,
            OffsetTime.MIN,
            OffsetTime.parse("10:15:30+01:00"),
            null
            )
        val js = sj[SampleOffsetTime].toJson(inst)
        js should matchJson("""{"o1":"23:59:59.999999999-18:00","o2":"00:00:00+18:00","o3":"10:15:30+01:00","o4":null}""")
        // inst shouldEqual ScalaJack.read[SampleOffsetTime](js)
      }

      it("Period must work") {
        val inst = SamplePeriod(Period.ZERO, Period.parse("P1Y2M3D"), null)
        val js = sj[SamplePeriod].toJson(inst)
        js should matchJson("""{"p1":"P0D","p2":"P1Y2M3D","p3":null}""")
        // inst shouldEqual ScalaJack.read[SamplePeriod](js)
      }

      it("Year must work") {
        val inst = SampleYear(Year.of(Year.MAX_VALUE), Year.of(Year.MIN_VALUE), Year.parse("2020"), null)
        val js = sj[SampleYear].toJson(inst)
        js should matchJson("""{"y1":"999999999","y2":"-999999999","y3":"2020","y4":null}""")
        // inst shouldEqual ScalaJack.read[SampleYear](js)
      }

      it("YearMonth must work") {
        val inst = SampleYearMonth(YearMonth.of(2020,7), null)
        val js = sj[SampleYearMonth].toJson(inst)
        js should matchJson("""{"y1":"2020-07","y2":null}""")
        // inst shouldEqual ScalaJack.read[SampleYearMonth](js)
      }

      it("ZonedDateTime must work") {
        val inst = SampleZonedDateTime(
            ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]"),
            null
            )
        val js = sj[SampleZonedDateTime].toJson(inst)
        js should matchJson("""{"o1":"2007-12-03T10:15:30+01:00[Europe/Paris]","o2":null}""")
        // inst shouldEqual ScalaJack.read[SampleZonedDateTime](js)
      }

      it("ZonedId must work") {
        val inst = SampleZoneId(
            ZoneId.of("America/Puerto_Rico"),
            null
            )
        val js = sj[SampleZoneId].toJson(inst)
        js should matchJson("""{"z1":"America/Puerto_Rico","z2":null}""")
        // inst shouldEqual ScalaJack.read[SampleZoneId](js)
      }

      it("ZoneOffset must work") {
        val ldt = LocalDateTime.parse("2007-12-03T10:15:30")
        val zone = ZoneId.of("Europe/Berlin")
        val zoneOffSet = zone.getRules().getOffset(ldt)
        val inst = SampleZoneOffset(
            null,
            zoneOffSet
            )
        val js = sj[SampleZoneOffset].toJson(inst)
        js should matchJson("""{"z1":null,"z2":"+01:00"}""")
        // inst shouldEqual ScalaJack.read[SampleZoneOffset](js)
      }

      it("UUID must work") {
        val inst = SampleUUID(
            null,
            UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9")
            )
        val js = sj[SampleUUID].toJson(inst)
        js should matchJson("""{"u1":null,"u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}""")
        // inst shouldEqual ScalaJack.read[SampleUUID](js)
      }
    }
  }

/*


  test("Duration must break") {
    describe("--- Negative Tests ---")

    val js = """{"d1":"PT0S","d2":21,"d3":null}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{"d1":"PT0S","d2":21,"d3":null}
              |------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleDuration](js)
    }              
    val js2 = """{"d1":"PT0S","d2":"bogus","d3":null}""".asInstanceOf[JSON]
    val msg2 = """Failed to parse Duration from input 'bogus'
                |{"d1":"PT0S","d2":"bogus","d3":null}
                |------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleDuration](js2)
    }              
  }

  test("Instant must break") {
    val js =
      """{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a String here
              |{"i1":"1970-01-01T00:00:00Z","i2":false,"i3":"-1000000000-01-01T00:00:00Z","i...
              |----------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleInstant](js)
    }              
    val js2 =
      """{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z","i4":"2007-12-03T10:15:30Z","i5":null}""".asInstanceOf[JSON]
    val msg2 =
      """Failed to parse Instant from input 'bogus'
                |{"i1":"1970-01-01T00:00:00Z","i2":"bogus","i3":"-1000000000-01-01T00:00:00Z",...
                |----------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleInstant](js2)
    }     
  }

  test("LocalDateTime must break") {
    val js =
      """{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a String here
              |{"d1":-1,"d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d4":null}
              |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLocalDateTime](js)
    }
    val js2 =
      """{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d1":null}""".asInstanceOf[JSON]
    val msg2 =
      """Failed to parse LocalDateTime from input 'bogus'
                |{"d1":"bogus","d2":"-999999999-01-01T00:00:00","d3":"2007-12-03T10:15:30","d1...
                |------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleLocalDateTime](js2)
    }
  }

  test("LocalDate must break") {
    val js =
      """{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{"d1":-1,"d2":"-999999999-01-01","d3":"2007-12-03","d4":null}
              |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLocalDate](js)
    }
    val js2 =
      """{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}""".asInstanceOf[JSON]
    val msg2 =
      """Failed to parse LocalDate from input 'bogus'
                |{"d1":"bogus","d2":"-999999999-01-01","d3":"2007-12-03","d4":null}
                |------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleLocalDate](js2)
    }
  }

  test("LocalTime must break") {
    val js =
      """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a String here
              |...:"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":false,"d6":null}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLocalTime](js)
    }
    val js2 =
      """{"d1":"23:59:59.999999999","d2":"00:00:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}""".asInstanceOf[JSON]
    val msg2 =
      """Failed to parse LocalTime from input 'Bogus'
                |...0:00","d3":"00:00:00","d4":"12:00:00","d5":"Bogus","d6":null}
                |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleLocalTime](js2)
    }
  }

  test("OffsetDateTime must break") {
    val js =
      """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30+01:00","o4":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a String here
              |..."+999999999-12-31T23:59:59.999999999-18:00","o2":2,"o3":"2007-12-03T10:15:30...
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleOffsetDateTime](js)
    }
    val js2 =
      """{"o1":"+999999999-12-31T23:59:59.999999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10:15:30+01:00","o4":null}""".asInstanceOf[JSON]
    val msg2 =
      """Failed to parse OffsetDateTime from input '-999999999-01T00:00:00+18:00'
                |...9999999-18:00","o2":"-999999999-01T00:00:00+18:00","o3":"2007-12-03T10:15:30...
                |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleOffsetDateTime](js2)
    }
  }

  test("OffsetTime must break") {
    val js =
      """{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a String here
              |{"o1":"23:59:59.999999999-18:00","o2":false,"o3":"10:15:30+01:00","o4":null}
              |--------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleOffsetTime](js)
    }
    val js2 =
      """{"o1":"23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o4":null}""".asInstanceOf[JSON]
    val msg2 =
      """Failed to parse OffsetTime from input '00:00:00:00+18:00'
                |...23:59:59.999999999-18:00","o2":"00:00:00:00+18:00","o3":"10:15:30+01:00","o4...
                |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleOffsetTime](js2)
    }
  }

  test("Period must break") {
    val js = """{"p1":"P0D","p2":5,"p3":null}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{"p1":"P0D","p2":5,"p3":null}
              |-----------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SamplePeriod](js)
    }
    val js2 = """{"p1":"P0D","p2":"bogus","p3":null}""".asInstanceOf[JSON]
    val msg2 = """Failed to parse Period from input 'bogus'
                |{"p1":"P0D","p2":"bogus","p3":null}
                |-----------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SamplePeriod](js2)
    }
  }

  test("ZonedDateTime must break") {
    val js = """{"o1":true,"o2":null}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{"o1":true,"o2":null}
              |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleZonedDateTime](js)
    }
    val js2 = """{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}""".asInstanceOf[JSON]
    val msg2 =
      """Failed to parse ZonedDateTime from input '2007-12-03T10:15:30+01:00 Earth'
                |{"o1":"2007-12-03T10:15:30+01:00 Earth","o2":null}
                |--------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleZonedDateTime](js2)
    }
  }
*/