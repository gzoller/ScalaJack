package co.blocke.scalajack
package json.mapkeys

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import java.lang.{
  Boolean => JBoolean,
  Byte => JByte,
  Character => JChar,
  Double => JDouble,
  Float => JFloat,
  Integer => JInteger,
  Long => JLong,
  Short => JShort
}
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._

import co.blocke.scalajack.json.JsonMatcher

class JavaPrimKeys() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("With BigDecimal Key") {
    describe(
      "----------------------------------\n:  Java Primitive Map Key Tests  :\n----------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
    describe("Simple DelimSpec:")
    
    val inst = SampleJBigDecimal(
      Map(
        new JBigDecimal("123.456") -> new JBigDecimal("1"),
        new JBigDecimal("789.123") -> new JBigDecimal("2")
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"123.456":1,"789.123":2}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJBigDecimal](js))
  }

  test("With BigInteger Key") {
    val inst = SampleJBigInteger(
      Map(
        new JBigInteger("123") -> new JBigInteger("1"),
        new JBigInteger("789") -> new JBigInteger("2")
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"123":1,"789":2}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJBigInteger](js))
  }

  test("With Boolean Key") {
    val inst = SampleJBoolean(
      Map(
        true.asInstanceOf[JBoolean] -> false.asInstanceOf[JBoolean],
        false.asInstanceOf[JBoolean] -> true.asInstanceOf[JBoolean]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"true":false,"false":true}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJBoolean](js))
  }

  test("With Byte Key") {
    val inst = SampleJByte(
      Map(
        16.toByte.asInstanceOf[JByte] -> 2.toByte.asInstanceOf[JByte],
        48.toByte.asInstanceOf[JByte] -> 9.toByte.asInstanceOf[JByte]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"16":2,"48":9}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJByte](js))
  }

  test("With Char Key") {
    val inst = SampleJChar(
      Map(
        'a'.asInstanceOf[JChar] -> 'A'.asInstanceOf[JChar],
        'z'.asInstanceOf[JChar] -> 'Z'.asInstanceOf[JChar]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"a":"A","z":"Z"}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJChar](js))
  }

  test("With Double Key") {
    val inst = SampleJDouble(
      Map(
        12.34.asInstanceOf[JDouble] -> 56.78.asInstanceOf[JDouble],
        90.12.asInstanceOf[JDouble] -> 34.56.asInstanceOf[JDouble]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12.34":56.78,"90.12":34.56}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJDouble](js))
  }

  test("With Float Key") {
    val inst = SampleJFloat(
      Map(
        12.34F.asInstanceOf[JFloat] -> 56.78F.asInstanceOf[JFloat],
        90.12F.asInstanceOf[JFloat] -> 34.56F.asInstanceOf[JFloat]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12.34":56.78,"90.12":34.56}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJFloat](js))
  }

  test("With Integer Key") {
    val inst = SampleJInteger(
      Map(
        12.asInstanceOf[JInteger] -> 56.asInstanceOf[JInteger],
        90.asInstanceOf[JInteger] -> 34.asInstanceOf[JInteger]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56,"90":34}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJInteger](js))
  }

  test("With Long Key") {
    val inst = SampleJLong(
      Map(
        12L.asInstanceOf[JLong] -> 56L.asInstanceOf[JLong],
        90L.asInstanceOf[JLong] -> 34L.asInstanceOf[JLong]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56,"90":34}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJLong](js))
  }


  test("With Number Key") {
    val inst = SampleJNumber(
      Map(
        JByte.valueOf("-128") -> JByte.valueOf("127"),
        JShort.valueOf("-32768") -> JShort.valueOf("32767"),
        JInteger.valueOf("-2147483648") -> JInteger.valueOf("2147483647"),
        JLong.valueOf("-9223372036854775808") -> JLong
          .valueOf("9223372036854755807"),
        JByte.valueOf("0") -> new JBigInteger("9923372036854755810"),
        JFloat.valueOf("3.4e-038") -> JFloat.valueOf("3.4e+038"),
        JDouble.valueOf("1.7e-308") -> JDouble.valueOf("1.7e+308"),
        new JBigDecimal("1.8e+308") -> JFloat.valueOf("0.0")
      )
    )
    val result = SampleJNumber(
      Map(
        JByte.valueOf("0") -> new JBigDecimal("9923372036854755810"),
        JInteger.valueOf("-2147483648") -> JInteger.valueOf("2147483647"),
        JLong.valueOf("-9223372036854775808") -> JLong
          .valueOf("9223372036854755807"),
        JByte.valueOf("-128") -> JByte.valueOf("127"),
        JFloat.valueOf("3.4E-38") -> JFloat.valueOf("3.4E38"),
        JShort.valueOf("-32768") -> JShort.valueOf("32767"),
        new JBigDecimal("1.8E+308") -> JByte.valueOf("0"),
        JDouble.valueOf("1.7E-308") -> JDouble.valueOf("1.7E308")
      )
    )
    val js = sj.render(inst)
    assert( JsonMatcher.jsonMatches(js, 
      """{"m":{"0":9923372036854755810,"-2147483648":2147483647,"-9223372036854775808":9223372036854755807,"-128":127,"3.4E-38":3.4E38,"-32768":32767,"1.8E+308":0.0,"1.7E-308":1.7E308}}""".asInstanceOf[JSON]))
    val read = sj.read[SampleJNumber](js)
    assertEquals(result, read)
  }

  test("With Short Key") {
    val inst = SampleJShort(
      Map(
        12.toShort.asInstanceOf[JShort] -> 56.toShort
          .asInstanceOf[JShort],
        90.toShort.asInstanceOf[JShort] -> 34.toShort.asInstanceOf[JShort]
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56,"90":34}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleJShort](js))
  }


  test("With Duration Key") {
    describe("Time DelimSpec:")

    val inst =
      SampleDuration(Map(Duration.ZERO -> Duration.parse("P2DT3H4M")))
    val js = sj.render(inst)
    assertEquals("""{"m":{"PT0S":"PT51H4M"}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleDuration](js))
  }

  test("With Instant Key") {
    val inst = SampleInstant(
      Map(
        Instant.EPOCH -> Instant.MAX,
        Instant.MIN -> Instant.parse("2007-12-03T10:15:30.00Z")
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"1970-01-01T00:00:00Z":"+1000000000-12-31T23:59:59.999999999Z","-1000000000-01-01T00:00:00Z":"2007-12-03T10:15:30Z"}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleInstant](js))
  }

  test("With LocalDateTime Key") {
    val inst = SampleLocalDateTime(
      Map(
        LocalDateTime.MAX -> LocalDateTime.MIN,
        LocalDateTime.parse("2007-12-03T10:15:30") -> null
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"+999999999-12-31T23:59:59.999999999":"-999999999-01-01T00:00:00","2007-12-03T10:15:30":null}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleLocalDateTime](js))
  }

  test("With LocalDate Key") {
    val inst = SampleLocalDate(
      Map(
        LocalDate.MAX -> LocalDate.MIN,
        LocalDate.parse("2007-12-03") -> null
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"+999999999-12-31":"-999999999-01-01","2007-12-03":null}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleLocalDate](js))
  }

  test("With LocalTime Key") {
    val inst = SampleLocalTime(
      Map(
        LocalTime.MAX -> LocalTime.MIN,
        LocalTime.MIDNIGHT -> LocalTime.NOON,
        LocalTime.parse("10:15:30") -> null
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"23:59:59.999999999":"00:00:00","00:00:00":"12:00:00","10:15:30":null}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleLocalTime](js))
  }

  test("With OffsetDateTime Key") {
    val inst = SampleOffsetDateTime(
      Map(
        OffsetDateTime.MAX -> OffsetDateTime.MIN,
        OffsetDateTime.parse("2007-12-03T10:15:30+01:00") -> null
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"+999999999-12-31T23:59:59.999999999-18:00":"-999999999-01-01T00:00:00+18:00","2007-12-03T10:15:30+01:00":null}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleOffsetDateTime](js))
  }

  test("With OffsetTime Key") {
    val inst = SampleOffsetTime(
      Map(
        OffsetTime.MAX -> OffsetTime.MIN,
        OffsetTime.parse("10:15:30+01:00") -> null
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"23:59:59.999999999-18:00":"00:00:00+18:00","10:15:30+01:00":null}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleOffsetTime](js))
  }

  test("With Period Key") {
    val inst = SamplePeriod(Map(Period.ZERO -> Period.parse("P1Y2M3D")))
    val js = sj.render(inst)
    assertEquals("""{"m":{"P0D":"P1Y2M3D"}}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SamplePeriod](js))
  }

  test("With ZonedDateTime Key") {
    val inst = SampleZonedDateTime(
      Map(
        ZonedDateTime
          .parse("2007-12-03T10:15:30+01:00[Europe/Paris]") -> null
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"2007-12-03T10:15:30+01:00[Europe/Paris]":null}}"""
    .asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleZonedDateTime](js))
  }

  test("Bad BigDecimal Key") {
    describe("--- Negative Tests ---")
    describe("Simple DelimSpec:")

    val js = """{"m":{"fred":1,"789.123":2}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |fred
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJBigDecimal](js)
    }
  }

  test("Bad BigInt Key") {
    val js = """{"m":{"fred":1,"789":2}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |fred
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJBigInteger](js)
    }
  }

  test("Bad Boolean Key") {
    val js = """{"m":{"true":false,"123":true}}""".asInstanceOf[JSON]
    val msg = """Expected a Boolean here
              |123
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJBoolean](js)
    }
  }

  test("Bad Byte Key") {
    val js = """{"m":{"16":2,"4x8":9}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |4x8
              |-^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJByte](js)
    }
  }

  test("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
    val js = """{"m":{null:"A","z":"Z"}}""".asInstanceOf[JSON]
    val msg = """Map keys cannot be null
              |{"m":{null:"A","z":"Z"}}
              |----------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJChar](js)
    }
  }

  test("Bad Double Key") {
    val js = """{"m":{"12.34":56.78,"true":34.56}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |true
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJDouble](js)
    }
  }

  test("Bad Float Key") {
    val js = """{"m":{"12.34":56.78,"90.12.3":34.56}}""".asInstanceOf[JSON]
    interceptMessage[java.lang.NumberFormatException]("multiple points"){
      sj.read[SampleJFloat](js)
    }
  }

  test("Bad Int Key") {
    val js = """{"m":{"12.0":56,"90":34}}""".asInstanceOf[JSON]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"12.0\""){
      sj.read[SampleJInteger](js)
    }
  }

  test("Bad Long Key") {
    val js = """{"m":{"12":56,"hey":34}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |hey
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJLong](js)
    }
  }

  test("Bad Number Key") {
    val js =
      """{"m":{"flume":9923372036854755810,"-2147483648":2147483647,"-9223372036854775808":9223372036854755807,"-128":127,"3.4E-38":3.4E38,"-32768":32767,"1.8E+308":0.0,"1.7E-308":1.7E308}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |flume
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJNumber](js)
    }
  }

  test("Bad Duration Key") {
    describe("Time DelimSpec:") 

    val js = """{"m":{"PT0SXXX":"PT51H4M"}}""".asInstanceOf[JSON]
    val msg = """Failed to parse Duration from input 'PT0SXXX'
              |{"m":{"PT0SXXX":"PT51H4M"}}
              |--------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleDuration](js)
    }
  }

  test("Bad Instant Key") {
    val js =
      """{"m":{"1970-01-01T00:00:00Z":"+1000000000-12-31T23:59:59.999999999Z","bogus":"2007-12-03T10:15:30Z"}}""".asInstanceOf[JSON]
    val msg =
      """Failed to parse Instant from input 'bogus'
              |...Z":"+1000000000-12-31T23:59:59.999999999Z","bogus":"2007-12-03T10:15:30Z"}}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleInstant](js)
    }
  }

  test("Bad LocalDateTime Key") {
    val js =
      """{"m":{"+999999999-12-31T23:59:59.999999999":"-999999999-01-01T00:00:00","bogus":null}}""".asInstanceOf[JSON]
    val msg =
      """Failed to parse LocalDateTime from input 'bogus'
              |...:59.999999999":"-999999999-01-01T00:00:00","bogus":null}}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLocalDateTime](js)
    }
  }

  test("Bad LocalDate Key") {
    val js = """{"m":{"bogus":"-999999999-01-01","2007-12-03":null}}""".asInstanceOf[JSON]
    val msg = """Failed to parse LocalDate from input 'bogus'
              |{"m":{"bogus":"-999999999-01-01","2007-12-03":null}}
              |------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLocalDate](js)
    }
  }

  test("Bad LocalTime Key") {
    val js =
      """{"m":{"23:59:59.999999999":"00:00:00","nada":"12:00:00","10:15:30":null}}""".asInstanceOf[JSON]
    val msg =
      """Failed to parse LocalTime from input 'nada'
              |{"m":{"23:59:59.999999999":"00:00:00","nada":"12:00:00","10:15:30":null}}
              |-------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLocalTime](js)
    }
  }

  test("Bad OffsetDateTime Key") {
    val js =
      """{"m":{"false":"-999999999-01-01T00:00:00+18:00","2007-12-03T10:15:30+01:00":null}}""".asInstanceOf[JSON]
    val msg =
      """Failed to parse OffsetDateTime from input 'false'
              |{"m":{"false":"-999999999-01-01T00:00:00+18:00","2007-12-03T10:15:30+01:00":n...
              |------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleOffsetDateTime](js)
    }
  }

  test("Bad OffsetTime Key") {
    val js = """{"m":{"2007-12-03T10:15:30+01:00[Europe\/Bogus]":null}}""".asInstanceOf[JSON]
    val msg =
      """Failed to parse OffsetTime from input '2007-12-03T10:15:30+01:00[Europe/Bogus]'
              |{"m":{"2007-12-03T10:15:30+01:00[Europe\/Bogus]":null}}
              |-----------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleOffsetTime](js)
    }
  }

  test("Bad Period Key") {
    val js = """{"m":{"P0D???":"P1Y2M3D"}}""".asInstanceOf[JSON]
    val msg = """Failed to parse Period from input 'P0D???'
              |{"m":{"P0D???":"P1Y2M3D"}}
              |-------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SamplePeriod](js)
    }
  }

  test("Bad ZonedDateTime Key") {
    val js =
      """{"m":{"FRED23:59:59.999999999-18:00":"00:00:00+18:00","10:15:30+01:00":null}}""".asInstanceOf[JSON]
    val msg =
      """Failed to parse ZonedDateTime from input 'FRED23:59:59.999999999-18:00'
              |{"m":{"FRED23:59:59.999999999-18:00":"00:00:00+18:00","10:15:30+01:00":null}}
              |-----------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleZonedDateTime](js)
    }
  }