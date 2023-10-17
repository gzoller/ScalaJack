package co.blocke.scalajack
package yaml
package primitives

import TestUtil._
import munit._
import munit.internal.console
import java.time._

class TimePrim() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Duration must work") {
    describe(
      "---------------------------------\n:  Time Primitive Tests (YAML)  :\n---------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
    val inst =
      SampleDuration(Duration.ZERO, Duration.parse("P2DT3H4M"), null)
    val yaml       = sj.render(inst)
    val comparison = """d1: PT0S
                        |d2: PT51H4M
                        |d3: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleDuration](yaml))
  }

  test("Instant must work") {
    val inst = SampleInstant(
      Instant.EPOCH,
      Instant.MAX,
      Instant.MIN,
      Instant.parse("2007-12-03T10:15:30.00Z"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """i1: 1970-01-01T00:00:00Z
                        |i5: null
                        |i2: +1000000000-12-31T23:59:59.999999999Z
                        |i3: -1000000000-01-01T00:00:00Z
                        |i4: 2007-12-03T10:15:30Z""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleInstant](yaml))
  }

  test("LocalDateTime must work") {
    val inst = SampleLocalDateTime(
      LocalDateTime.MAX,
      LocalDateTime.MIN,
      LocalDateTime.parse("2007-12-03T10:15:30"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """d1: +999999999-12-31T23:59:59.999999999
                        |d2: -999999999-01-01T00:00:00
                        |d3: 2007-12-03T10:15:30
                        |d4: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleLocalDateTime](yaml))
  }

  test("LocalDate must work") {
    val inst = SampleLocalDate(
      LocalDate.MAX,
      LocalDate.MIN,
      LocalDate.parse("2007-12-03"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """d1: +999999999-12-31
                        |d2: -999999999-01-01
                        |d3: 2007-12-03
                        |d4: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleLocalDate](yaml))
  }

  test("LocalTime must work") {
    val inst = SampleLocalTime(
      LocalTime.MAX,
      LocalTime.MIN,
      LocalTime.MIDNIGHT,
      LocalTime.NOON,
      LocalTime.parse("10:15:30"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """d1: 23:59:59.999999999
                        |d6: null
                        |d2: 00:00:00
                        |d5: 10:15:30
                        |d3: 00:00:00
                        |d4: 12:00:00""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleLocalTime](yaml))
  }

  test("OffsetDateTime must work") {
    val inst = SampleOffsetDateTime(
      OffsetDateTime.MAX,
      OffsetDateTime.MIN,
      OffsetDateTime.parse("2007-12-03T10:15:30+01:00"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """o1: +999999999-12-31T23:59:59.999999999-18:00
                        |o2: -999999999-01-01T00:00:00+18:00
                        |o3: 2007-12-03T10:15:30+01:00
                        |o4: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleOffsetDateTime](yaml))
  }

  test("OffsetTime must work") {
    val inst = SampleOffsetTime(
      OffsetTime.MAX,
      OffsetTime.MIN,
      OffsetTime.parse("10:15:30+01:00"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """o1: 23:59:59.999999999-18:00
                        |o2: 00:00:00+18:00
                        |o3: 10:15:30+01:00
                        |o4: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleOffsetTime](yaml))
  }

  test("Period must work") {
    val inst       = SamplePeriod(Period.ZERO, Period.parse("P1Y2M3D"), null)
    val yaml       = sj.render(inst)
    val comparison = """p1: P0D
                        |p2: P1Y2M3D
                        |p3: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SamplePeriod](yaml))
  }

  test("ZonedDateTime must work") {
    val inst = SampleZonedDateTime(
      ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """o1: 2007-12-03T10:15:30+01:00[Europe/Paris]
                        |o2: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleZonedDateTime](yaml))
  }

  test("Duration must break") {
    describe("--- Negative Tests ---")
    val yaml =
      """d1: PT0S
        |d2: 21
        |d3: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse Duration from input '21'"){
      sj.read[SampleDuration](yaml)
    }
    val yaml2 =
      """d1: PT0S
        |d2: bogus
        |d3: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse Duration from input 'bogus'"){
      sj.read[SampleDuration](yaml2)
    }
  }

  test("Instant must break") {
    val yaml =
      """i1: 1970-01-01T00:00:00Z
        |i2: false
        |i3: -1000000000-01-01T00:00:00Z
        |i4: 2007-12-03T10:15:30Z
        |i5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse Instant from input 'false'"){
      sj.read[SampleInstant](yaml)
    }
    val yaml2 =
      """i1: 1970-01-01T00:00:00Z
        |i2: bogus
        |i3: -1000000000-01-01T00:00:00Z
        |i4: 2007-12-03T10:15:30Z
        |i5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse Instant from input 'bogus'"){
      sj.read[SampleInstant](yaml2)
    }
  }

  test("LocalDateTime must break") {
    val yaml =
      """d1: -1
        |d2: "-999999999-01-01T00:00:00"
        |d3: 2007-12-03T10:15:30
        |d4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Failed to parse LocalDateTime from input '-1'"){
      sj.read[SampleLocalDateTime](yaml)
    }
    val yaml2 =
      """d1: bogus
        |d2: "-999999999-01-01T00:00:00"
        |d3: 2007-12-03T10:15:30
        |d4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Failed to parse LocalDateTime from input 'bogus'"){
      sj.read[SampleLocalDateTime](yaml2)
    }
  }

  test("LocalDate must break") {
    val yaml =
      """d1: -1
        |d2: "-999999999-01-01"
        |d3: "2007-12-03"
        |d4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Failed to parse LocalDate from input '-1'"){
      sj.read[SampleLocalDate](yaml)
    }
    val yaml2 =
      """d1: bogus
        |d2: "-999999999-01-01"
        |d3: "2007-12-03"
        |d4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Failed to parse LocalDate from input 'bogus'"){
      sj.read[SampleLocalDate](yaml2)
    }
  }

  test("LocalTime must break") {
    val yaml =
      """d1: "23:59:59.999999999"
        |d2: "00:00:00"
        |d3: "00:00:00"
        |d4: "12:00:00"
        |d5: false
        |d6: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 4: Failed to parse LocalTime from input 'false'"){
      sj.read[SampleLocalTime](yaml)
    }
    val yaml2 =
      """d1: "23:59:59.999999999"
        |d2: "00:00:00"
        |d3: "00:00:00"
        |d4: "12:00:00"
        |d5: bogus
        |d6: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 4: Failed to parse LocalTime from input 'bogus'"){
      sj.read[SampleLocalTime](yaml2)
    }
  }

  test("OffsetDateTime must break") {
    val yaml =
      """o1: +999999999-12-31T23:59:59.999999999-18:00
        |o2: 2
        |o3: 2007-12-03T10:15:30+01:00
        |o4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse OffsetDateTime from input '2'"){
      sj.read[SampleOffsetDateTime](yaml)
    }
    val yaml2 =
      """o1: +999999999-12-31T23:59:59.999999999-18:00
        |o2: -999999999-01T00:00:00+18:00
        |o3: 2007-12-03T10:15:30+01:00
        |o4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse OffsetDateTime from input '-999999999-01T00:00:00+18:00'"){
      sj.read[SampleOffsetDateTime](yaml2)
    }
  }

  test("OffsetTime must break") {
    val yaml =
      """o1: "23:59:59.999999999-18:00"
        |o2: false
        |o3: "10:15:30+01:00"
        |o4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse OffsetTime from input 'false'"){
      sj.read[SampleOffsetTime](yaml)
    }
    val yaml2 =
      """o1: "23:59:59.999999999-18:00"
        |o2: 00:00:00:00+18:00
        |o3: "10:15:30+01:00"
        |o4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse OffsetTime from input '00:00:00:00+18:00'"){
      sj.read[SampleOffsetTime](yaml2)
    }
  }

  test("Period must break") {
    val yaml =
      """p1: P0D
        |p2: 5
        |p3: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse Period from input '5'"){
      sj.read[SamplePeriod](yaml)
    }
    val yaml2 =
      """p1: P0D
        |p2: bogus
        |p3: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to parse Period from input 'bogus'"){
      sj.read[SamplePeriod](yaml2)
    }
  }

  test("ZonedDateTime must break") {
    val yaml =
      """o1: true
        |o2: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Failed to parse ZonedDateTime from input 'true'"){
      sj.read[SampleZonedDateTime](yaml)
    }
    val yaml2 =
      """o1: "2007-12-03T10:15:30+01:00 Earth"
        |o2: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Failed to parse ZonedDateTime from input '2007-12-03T10:15:30+01:00 Earth'"){
      sj.read[SampleZonedDateTime](yaml2)
    }
  }