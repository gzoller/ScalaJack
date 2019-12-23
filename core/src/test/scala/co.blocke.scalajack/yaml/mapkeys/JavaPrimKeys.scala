package co.blocke.scalajack
package yaml
package mapkeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import java.lang.{Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal, BigInteger => JBigInteger}
import java.time._

class JavaPrimKeys() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "-----------------------------------------\n:  Java Primitive Map Key Tests (YAML)  :\n-----------------------------------------"
  ) {
    describe("+++ Positive Tests +++") {
      describe("Simple DelimSpec:") {
        it("With BigDecimal Key") {
          val inst = SampleJBigDecimal(
            Map(
              new JBigDecimal("123.456") -> new JBigDecimal("1"),
              new JBigDecimal("789.123") -> new JBigDecimal("2")
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  123.456: !!float '1'
                             |  789.123: !!float '2'
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJBigDecimal](yaml)
          }
        }
        it("With BigInteger Key") {
          val inst = SampleJBigInteger(
            Map(
              new JBigInteger("123") -> new JBigInteger("1"),
              new JBigInteger("789") -> new JBigInteger("2")
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  123: 1
                             |  789: 2
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJBigInteger](yaml)
          }
        }
        it("With Boolean Key") {
          val inst = SampleJBoolean(
            Map(
              true.asInstanceOf[JBoolean]  -> false.asInstanceOf[JBoolean],
              false.asInstanceOf[JBoolean] -> true.asInstanceOf[JBoolean]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  true: false
                             |  false: true
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJBoolean](yaml)
          }
        }
        it("With Byte Key") {
          val inst = SampleJByte(
            Map(
              16.toByte.asInstanceOf[JByte] -> 2.toByte.asInstanceOf[JByte],
              48.toByte.asInstanceOf[JByte] -> 9.toByte.asInstanceOf[JByte]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  16: 2
                             |  48: 9
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJByte](yaml)
          }
        }
        it("With Char Key") {
          val inst = SampleJChar(
            Map(
              'a'.asInstanceOf[JChar] -> 'A'.asInstanceOf[JChar],
              'z'.asInstanceOf[JChar] -> 'Z'.asInstanceOf[JChar]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  a: A
                             |  z: Z
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJChar](yaml)
          }
        }
        it("With Double Key") {
          val inst = SampleJDouble(
            Map(
              12.34.asInstanceOf[JDouble] -> 56.78.asInstanceOf[JDouble],
              90.12.asInstanceOf[JDouble] -> 34.56.asInstanceOf[JDouble]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  12.34: 56.78
                             |  90.12: 34.56
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJDouble](yaml)
          }
        }
        it("With Float Key") {
          val inst = SampleJFloat(
            Map(
              12.34F.asInstanceOf[JFloat] -> 56.78F.asInstanceOf[JFloat],
              90.12F.asInstanceOf[JFloat] -> 34.56F.asInstanceOf[JFloat]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  12.34: 56.78
                             |  90.12: 34.56
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJFloat](yaml)
          }
        }
        it("With Integer Key") {
          val inst = SampleJInteger(
            Map(
              12.asInstanceOf[JInteger] -> 56.asInstanceOf[JInteger],
              90.asInstanceOf[JInteger] -> 34.asInstanceOf[JInteger]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  12: 56
                             |  90: 34
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJInteger](yaml)
          }
        }
        it("With Long Key") {
          val inst = SampleJLong(
            Map(
              12L.asInstanceOf[JLong] -> 56L.asInstanceOf[JLong],
              90L.asInstanceOf[JLong] -> 34L.asInstanceOf[JLong]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  12: 56
                             |  90: 34
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJLong](yaml)
          }
        }

        it("With Number Key") {
          val inst = SampleJNumber(
            Map(
              JByte.valueOf("-128")           -> JByte.valueOf("127"),
              JShort.valueOf("-32768")        -> JShort.valueOf("32767"),
              JInteger.valueOf("-2147483648") -> JInteger.valueOf("2147483647"),
              JLong.valueOf("-9223372036854775808") -> JLong
                .valueOf("9223372036854755807"),
              JByte.valueOf("0")          -> new JBigInteger("9923372036854755810"),
              JFloat.valueOf("3.4e-038")  -> JFloat.valueOf("3.4e+038"),
              JDouble.valueOf("1.7e-308") -> JDouble.valueOf("1.7e+308"),
              new JBigDecimal("1.8e+308") -> JFloat.valueOf("0.0")
            )
          )
          val result = SampleJNumber(
            Map(
              JByte.valueOf("0")              -> new JBigDecimal("9923372036854755810"),
              JInteger.valueOf("-2147483648") -> JInteger.valueOf("2147483647"),
              JLong.valueOf("-9223372036854775808") -> JLong
                .valueOf("9223372036854755807"),
              JByte.valueOf("-128")       -> JByte.valueOf("127"),
              JFloat.valueOf("3.4E-38")   -> JFloat.valueOf("3.4E38"),
              JShort.valueOf("-32768")    -> JShort.valueOf("32767"),
              new JBigDecimal("1.8E+308") -> JByte.valueOf("0"),
              JDouble.valueOf("1.7E-308") -> JDouble.valueOf("1.7E308")
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  -128: 127
                             |  3.4E-38: 3.4E38
                             |  -32768: 32767
                             |  1.8E+308: 0.0
                             |  1.7E-308: 1.7E308
                             |  0: 9923372036854755810
                             |  -2147483648: 2147483647
                             |  -9223372036854775808: 9223372036854755807
                             |""".stripMargin
          assertResult(comparison) { yaml }
          val read = sj.read[SampleJNumber](yaml)
          assertResult(result) { read }
        }
        it("With Short Key") {
          val inst = SampleJShort(
            Map(
              12.toShort.asInstanceOf[JShort] -> 56.toShort
                .asInstanceOf[JShort],
              90.toShort.asInstanceOf[JShort] -> 34.toShort.asInstanceOf[JShort]
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  12: 56
                             |  90: 34
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleJShort](yaml)
          }
        }
      }
      describe("Time DelimSpec:") {
        it("With Duration Key") {
          val inst =
            SampleDuration(Map(Duration.ZERO -> Duration.parse("P2DT3H4M")))
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  PT0S: PT51H4M
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleDuration](yaml)
          }
        }
        it("With Instant Key") {
          val inst = SampleInstant(
            Map(
              Instant.EPOCH -> Instant.MAX,
              Instant.MIN   -> Instant.parse("2007-12-03T10:15:30.00Z")
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  1970-01-01T00:00:00Z: +1000000000-12-31T23:59:59.999999999Z
                             |  -1000000000-01-01T00:00:00Z: 2007-12-03T10:15:30Z
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleInstant](yaml)
          }
        }
        it("With LocalDateTime Key") {
          val inst = SampleLocalDateTime(
            Map(
              LocalDateTime.MAX                          -> LocalDateTime.MIN,
              LocalDateTime.parse("2007-12-03T10:15:30") -> null
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  +999999999-12-31T23:59:59.999999999: -999999999-01-01T00:00:00
                             |  2007-12-03T10:15:30: null
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleLocalDateTime](yaml)
          }
        }
        it("With LocalDate Key") {
          val inst = SampleLocalDate(
            Map(
              LocalDate.MAX                 -> LocalDate.MIN,
              LocalDate.parse("2007-12-03") -> null
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  +999999999-12-31: -999999999-01-01
                             |  2007-12-03: null
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleLocalDate](yaml)
          }
        }
        it("With LocalTime Key") {
          val inst = SampleLocalTime(
            Map(
              LocalTime.MAX               -> LocalTime.MIN,
              LocalTime.MIDNIGHT          -> LocalTime.NOON,
              LocalTime.parse("10:15:30") -> null
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  23:59:59.999999999: 00:00:00
                             |  00:00:00: 12:00:00
                             |  10:15:30: null
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleLocalTime](yaml)
          }
        }
        it("With OffsetDateTime Key") {
          val inst = SampleOffsetDateTime(
            Map(
              OffsetDateTime.MAX                                -> OffsetDateTime.MIN,
              OffsetDateTime.parse("2007-12-03T10:15:30+01:00") -> null
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  +999999999-12-31T23:59:59.999999999-18:00: -999999999-01-01T00:00:00+18:00
                             |  2007-12-03T10:15:30+01:00: null
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleOffsetDateTime](yaml)
          }
        }
        it("With OffsetTime Key") {
          val inst = SampleOffsetTime(
            Map(
              OffsetTime.MAX                     -> OffsetTime.MIN,
              OffsetTime.parse("10:15:30+01:00") -> null
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  23:59:59.999999999-18:00: 00:00:00+18:00
                             |  10:15:30+01:00: null
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleOffsetTime](yaml)
          }
        }
        it("With Period Key") {
          val inst       = SamplePeriod(Map(Period.ZERO -> Period.parse("P1Y2M3D")))
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  P0D: P1Y2M3D
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SamplePeriod](yaml)
          }
        }
        it("With ZonedDateTime Key") {
          val inst = SampleZonedDateTime(
            Map(
              ZonedDateTime
                .parse("2007-12-03T10:15:30+01:00[Europe/Paris]") -> null
            )
          )
          val yaml       = sj.render(inst)
          val comparison = """m:
                             |  2007-12-03T10:15:30+01:00[Europe/Paris]: null
                             |""".stripMargin
          assertResult(comparison) { yaml }
          assertResult(inst) {
            sj.read[SampleZonedDateTime](yaml)
          }
        }
      }
    }
  }
}
