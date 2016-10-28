package co.blocke.scalajack
package msgpack
package test.primitives

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import java.lang.{ Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Number => JNumber, Short => byteshort }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._

class JavaPrim() extends FunSpec with Matchers {

  val sj = ScalaJack(MsgPackFlavor())

  describe("-------------------------------------\n:  Java Primitives Tests (msgpack)  :\n-------------------------------------") {
    describe("+++ Positive Tests +++") {
      describe("Simple Primitives:") {
        it("Boolean must work") {
          val inst = SampleJBoolean(JBoolean.TRUE, JBoolean.FALSE, true, false, null)
          val bytes = sj.render(inst)
          assertResult("""85 A5 62 6F 6F 6C 31 C3 A5 62 6F 6F 6C 32 C2 A5 62 6F 6F 6C 33 C3 A5 62 6F 6F 6C 34 C2 A5 62 6F 6F 6C 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJBoolean](bytes)
          }
        }
        it("Byte must work") {
          val inst = SampleJByte(JByte.MAX_VALUE, JByte.MIN_VALUE, 0.asInstanceOf[Byte], 64.asInstanceOf[Byte], null)
          val bytes = sj.render(inst)
          assertResult("""85 A2 62 31 7F A2 62 32 D0 80 A2 62 33 00 A2 62 34 40 A2 62 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJByte](bytes)
          }
        }
        it("Char must work") {
          val inst = SampleJChar('Z', '\u20A0', null)
          val bytes = sj.render(inst)
          assertResult("""83 A2 63 31 A1 5A A2 63 32 A3 E2 82 A0 A2 63 33 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJChar](bytes)
          }
        }
        it("Double must work") {
          val inst = SampleJDouble(JDouble.MAX_VALUE, JDouble.MIN_VALUE, 0.0, -123.4567, null)
          val bytes = sj.render(inst)
          assertResult("""85 A2 64 31 CB 7F EF FF FF FF FF FF FF A2 64 32 CB 00 00 00 00 00 00 00 01 A2 64 33 CB 00 00 00 00 00 00 00 00 A2 64 34 CB C0 5E DD 3A 92 A3 05 53 A2 64 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJDouble](bytes)
          }
        }
        it("Float must work") {
          val inst = SampleJFloat(JFloat.MAX_VALUE, JFloat.MIN_VALUE, 0.0F, -123.4567F, null)
          val bytes = sj.render(inst)
          assertResult("""85 A2 66 31 CA 7F 7F FF FF A2 66 32 CA 00 00 00 01 A2 66 33 CA 00 00 00 00 A2 66 34 CA C2 F6 E9 D5 A2 66 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJFloat](bytes)
          }
        }
        it("Int must work") {
          val inst = SampleJInt(JInt.MAX_VALUE, JInt.MIN_VALUE, 0, 123, null)
          val bytes = sj.render(inst)
          assertResult("""85 A2 69 31 CE 7F FF FF FF A2 69 32 D2 80 00 00 00 A2 69 33 00 A2 69 34 7B A2 69 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJInt](bytes)
          }
        }
        it("Long must work") {
          val inst = SampleJLong(JLong.MAX_VALUE, JLong.MIN_VALUE, 0L, 123L, null)
          val bytes = sj.render(inst)
          assertResult("""85 A2 6C 31 CF 7F FF FF FF FF FF FF FF A2 6C 32 D3 80 00 00 00 00 00 00 00 A2 6C 33 00 A2 6C 34 7B A2 6C 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJLong](bytes)
          }
        }
        it("Number must work") {
          val inst = SampleJNumber(
            new JByte("-128"), new JByte("127"),
            -32768.toShort, 32767.toShort,
            // new byteshort("-32768"), new byteshort("32767"),
            new JInt("-2147483648"), new JInt("2147483647"),
            new JLong("-9223372036854775808"), new JLong("9223372036854755807"),
            new JByte("0"),
            new JFloat("3.4e-038"), new JFloat("3.4e+038"),
            new JDouble("1.7e-308"), new JDouble("1.7e+308"),
            new JFloat("0.0"),
            null,
            null
          )
          val bytes = sj.render(inst)
          assertResult("""DE 00 10 A2 6E 31 D0 80 A2 6E 32 7F A2 6E 33 D1 80 00 A2 6E 34 CD 7F FF A2 6E 35 D2 80 00 00 00 A2 6E 36 CE 7F FF FF FF A2 6E 37 D3 80 00 00 00 00 00 00 00 A2 6E 38 CF 7F FF FF FF FF FF B1 DF A2 6E 39 00 A3 6E 31 30 CA 01 39 1D 15 A3 6E 31 31 CA 7F 7F C9 9E A3 6E 31 32 CB 00 0C 39 6C 98 F8 D8 99 A3 6E 31 33 CB 7F EE 42 D1 30 77 3B 76 A3 6E 31 34 CA 00 00 00 00 A3 6E 31 35 C0 A3 6E 31 36 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJNumber](bytes)
          }
        }
        it("Short must work") {
          val inst = SampleJShort(byteshort.MAX_VALUE, byteshort.MIN_VALUE, 0.asInstanceOf[Short], 123.asInstanceOf[Short], null)
          val bytes = sj.render(inst)
          assertResult("""85 A2 73 31 CD 7F FF A2 73 32 D1 80 00 A2 73 33 00 A2 73 34 7B A2 73 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleJShort](bytes)
          }
        }
        it("UUID must work") {
          val inst = SampleUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"), null)
          val bytes = sj.render(inst)
          assertResult("""82 A2 75 31 DA 00 24 35 34 63 61 62 37 37 38 2D 37 62 39 65 2D 34 62 30 37 2D 39 64 33 37 2D 38 37 62 39 37 61 30 31 31 65 35 35 A2 75 32 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleUUID](bytes)
          }
        }
      }

      describe("Time Primitives:") {
        it("Duration must work") {
          val inst = SampleDuration(Duration.ZERO, Duration.parse("P2DT3H4M"), null)
          val bytes = sj.render(inst)
          assertResult("""83 A2 64 31 A4 50 54 30 53 A2 64 32 A7 50 54 35 31 48 34 4D A2 64 33 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleDuration](bytes)
          }
        }
        it("Instant must work") {
          val inst = SampleInstant(Instant.EPOCH, Instant.MAX, Instant.MIN, Instant.parse("2007-12-03T10:15:30.00Z"), null)
          val bytes = sj.render(inst)
          assertResult("""85 A2 69 31 B4 31 39 37 30 2D 30 31 2D 30 31 54 30 30 3A 30 30 3A 30 30 5A A2 69 32 DA 00 25 2B 31 30 30 30 30 30 30 30 30 30 2D 31 32 2D 33 31 54 32 33 3A 35 39 3A 35 39 2E 39 39 39 39 39 39 39 39 39 5A A2 69 33 BB 2D 31 30 30 30 30 30 30 30 30 30 2D 30 31 2D 30 31 54 30 30 3A 30 30 3A 30 30 5A A2 69 34 B4 32 30 30 37 2D 31 32 2D 30 33 54 31 30 3A 31 35 3A 33 30 5A A2 69 35 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleInstant](bytes)
          }
        }
        it("LocalDateTime must work") {
          val inst = SampleLocalDateTime(LocalDateTime.MAX, LocalDateTime.MIN, LocalDateTime.parse("2007-12-03T10:15:30"), null)
          val bytes = sj.render(inst)
          assertResult("""84 A2 64 31 DA 00 23 2B 39 39 39 39 39 39 39 39 39 2D 31 32 2D 33 31 54 32 33 3A 35 39 3A 35 39 2E 39 39 39 39 39 39 39 39 39 A2 64 32 B9 2D 39 39 39 39 39 39 39 39 39 2D 30 31 2D 30 31 54 30 30 3A 30 30 3A 30 30 A2 64 33 B3 32 30 30 37 2D 31 32 2D 30 33 54 31 30 3A 31 35 3A 33 30 A2 64 34 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleLocalDateTime](bytes)
          }
        }
        it("LocalDate must work") {
          val inst = SampleLocalDate(LocalDate.MAX, LocalDate.MIN, LocalDate.parse("2007-12-03"), null)
          val bytes = sj.render(inst)
          assertResult("""84 A2 64 31 B0 2B 39 39 39 39 39 39 39 39 39 2D 31 32 2D 33 31 A2 64 32 B0 2D 39 39 39 39 39 39 39 39 39 2D 30 31 2D 30 31 A2 64 33 AA 32 30 30 37 2D 31 32 2D 30 33 A2 64 34 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleLocalDate](bytes)
          }
        }
        it("LocalTime must work") {
          val inst = SampleLocalTime(LocalTime.MAX, LocalTime.MIN, LocalTime.MIDNIGHT, LocalTime.NOON, LocalTime.parse("10:15:30"), null)
          val bytes = sj.render(inst)
          assertResult("""86 A2 64 31 B2 32 33 3A 35 39 3A 35 39 2E 39 39 39 39 39 39 39 39 39 A2 64 32 A8 30 30 3A 30 30 3A 30 30 A2 64 33 A8 30 30 3A 30 30 3A 30 30 A2 64 34 A8 31 32 3A 30 30 3A 30 30 A2 64 35 A8 31 30 3A 31 35 3A 33 30 A2 64 36 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleLocalTime](bytes)
          }
        }
        it("OffsetDateTime must work") {
          val inst = SampleOffsetDateTime(OffsetDateTime.MAX, OffsetDateTime.MIN, OffsetDateTime.parse("2007-12-03T10:15:30+01:00"), null)
          val bytes = sj.render(inst)
          assertResult("""84 A2 6F 31 DA 00 29 2B 39 39 39 39 39 39 39 39 39 2D 31 32 2D 33 31 54 32 33 3A 35 39 3A 35 39 2E 39 39 39 39 39 39 39 39 39 2D 31 38 3A 30 30 A2 6F 32 BF 2D 39 39 39 39 39 39 39 39 39 2D 30 31 2D 30 31 54 30 30 3A 30 30 3A 30 30 2B 31 38 3A 30 30 A2 6F 33 B9 32 30 30 37 2D 31 32 2D 30 33 54 31 30 3A 31 35 3A 33 30 2B 30 31 3A 30 30 A2 6F 34 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleOffsetDateTime](bytes)
          }
        }
        it("OffsetTime must work") {
          val inst = SampleOffsetTime(OffsetTime.MAX, OffsetTime.MIN, OffsetTime.parse("10:15:30+01:00"), null)
          val bytes = sj.render(inst)
          assertResult("""84 A2 6F 31 B8 32 33 3A 35 39 3A 35 39 2E 39 39 39 39 39 39 39 39 39 2D 31 38 3A 30 30 A2 6F 32 AE 30 30 3A 30 30 3A 30 30 2B 31 38 3A 30 30 A2 6F 33 AE 31 30 3A 31 35 3A 33 30 2B 30 31 3A 30 30 A2 6F 34 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleOffsetTime](bytes)
          }
        }
        it("Period must work") {
          val inst = SamplePeriod(Period.ZERO, Period.parse("P1Y2M3D"), null)
          val bytes = sj.render(inst)
          assertResult("""83 A2 70 31 A3 50 30 44 A2 70 32 A7 50 31 59 32 4D 33 44 A2 70 33 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SamplePeriod](bytes)
          }
        }
        it("ZonedDateTime must work") {
          val inst = SampleZonedDateTime(ZonedDateTime.parse("2007-12-03T10:15:30+01:00[Europe/Paris]"), null)
          val bytes = sj.render(inst)
          assertResult("""82 A2 6F 31 DA 00 27 32 30 30 37 2D 31 32 2D 30 33 54 31 30 3A 31 35 3A 33 30 2B 30 31 3A 30 30 5B 45 75 72 6F 70 65 2F 50 61 72 69 73 5D A2 6F 32 C0""") { bytes.map("%02X" format _).mkString(" ") }
          assertResult(inst) {
            sj.read[SampleZonedDateTime](bytes)
          }
        }
      }
    }
  }
}
