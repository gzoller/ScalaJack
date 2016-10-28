package co.blocke.scalajack
package msgpack
package test.primitives

import org.scalatest.{ FunSpec, Matchers }
import scala.math.BigDecimal

class ScalaPrim() extends FunSpec with Matchers {

  val sj = ScalaJack(MsgPackFlavor())

  describe("-------------------------------------\n:  Scala Primitives Tests (MsgPack)  :\n-------------------------------------") {
    describe("+++ Positive Tests +++") {
      it("BigDecimal must work") {
        val inst = SampleBigDecimal(BigDecimal(123L), BigDecimal(1.23), BigDecimal(0), BigDecimal("123.456"), BigDecimal("0.1499999999999999944488848768742172978818416595458984375"), null)
        val bytes = sj.render(inst)
        assertResult("""86 A3 62 64 31 A3 31 32 33 A3 62 64 32 A4 31 2E 32 33 A3 62 64 33 A1 30 A3 62 64 34 A7 31 32 33 2E 34 35 36 A3 62 64 35 DA 00 39 30 2E 31 34 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 34 34 34 38 38 38 34 38 37 36 38 37 34 32 31 37 32 39 37 38 38 31 38 34 31 36 35 39 35 34 35 38 39 38 34 33 37 35 A3 62 64 36 C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleBigDecimal](bytes)
        }
      }
      it("BigInt must work") {
        val inst = SampleBigInt(BigInt("-90182736451928374653345"), BigInt("90182736451928374653345"), BigInt(0), null)
        val bytes = sj.render(inst)
        assertResult("""84 A3 62 69 31 B8 2D 39 30 31 38 32 37 33 36 34 35 31 39 32 38 33 37 34 36 35 33 33 34 35 A3 62 69 32 B7 39 30 31 38 32 37 33 36 34 35 31 39 32 38 33 37 34 36 35 33 33 34 35 A3 62 69 33 A1 30 A3 62 69 34 C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleBigInt](bytes)
        }
      }
      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(true, false)
        val bytes = sj.render(inst)
        assertResult("""82 A5 62 6F 6F 6C 31 C3 A5 62 6F 6F 6C 32 C2""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleBoolean](bytes)
        }
      }
      it("Byte must work (not nullable)") {
        val inst = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
        val bytes = sj.render(inst)
        assertResult("""84 A2 62 31 7F A2 62 32 D0 80 A2 62 33 00 A2 62 34 40""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleByte](bytes)
        }
      }
      it("Char must work (not nullable)") {
        val inst = SampleChar(Char.MaxValue, 'Z', '\u20A0')
        val bytes = sj.render(inst)
        assertResult("""83 A2 63 31 A3 EF BF BF A2 63 32 A1 5A A2 63 33 A3 E2 82 A0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleChar](bytes)
        }
      }
      it("Double must work (not nullable)") {
        val inst = SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val bytes = sj.render(inst)
        assertResult("""84 A2 64 31 CB 7F EF FF FF FF FF FF FF A2 64 32 CB FF EF FF FF FF FF FF FF A2 64 33 CB 00 00 00 00 00 00 00 00 A2 64 34 CB C0 5E DD 3A 92 A3 05 53""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleDouble](bytes)
        }
      }
      it("Enumeration must work (not nullable)") {
        val inst = SampleEnum(Size.Small, Size.Medium, Size.Large, null)
        val bytes = sj.render(inst)
        assertResult("""84 A2 65 31 A5 53 6D 61 6C 6C A2 65 32 A6 4D 65 64 69 75 6D A2 65 33 A5 4C 61 72 67 65 A2 65 34 C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleEnum](bytes)
        }
      }
      it("Float must work") {
        val inst = SampleFloat(Float.MaxValue, Float.MinValue, 0.0F, -123.4567F)
        val bytes = sj.render(inst)
        assertResult("""84 A2 66 31 CA 7F 7F FF FF A2 66 32 CA FF 7F FF FF A2 66 33 CA 00 00 00 00 A2 66 34 CA C2 F6 E9 D5""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleFloat](bytes)
        }
      }
      it("Int must work (not nullable)") {
        val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val bytes = sj.render(inst)
        assertResult("""84 A2 69 31 CE 7F FF FF FF A2 69 32 D2 80 00 00 00 A2 69 33 00 A2 69 34 7B""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleInt](bytes)
        }
      }
      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val bytes = sj.render(inst)
        assertResult("""84 A2 6C 31 CF 7F FF FF FF FF FF FF FF A2 6C 32 D3 80 00 00 00 00 00 00 00 A2 6C 33 00 A2 6C 34 7B""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleLong](bytes)
        }
      }
      it("Short must work (not nullable)") {
        val inst = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
        val bytes = sj.render(inst)
        assertResult("""84 A2 73 31 CD 7F FF A2 73 32 D1 80 00 A2 73 33 00 A2 73 34 7B""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleShort](bytes)
        }
      }
      it("String must work") {
        val inst = SampleString("something\b\n\f\r\t☆", "", null)
        val bytes = sj.render(inst)
        // The weird '+' here is to break up the unicode so it won't be interpreted and wreck the test.
        assertResult("""83 A2 73 31 B1 73 6F 6D 65 74 68 69 6E 67 08 0A 0C 0D 09 E2 98 86 A2 73 32 A0 A2 73 33 C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[SampleString](bytes)
        }
      }
    }
  }
}
