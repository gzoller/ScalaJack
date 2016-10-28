package co.blocke.scalajack
package msgpack
package test.primitives

import org.scalatest.{ FunSpec, Matchers }

class AnyPrim() extends FunSpec with Matchers {

  val sj = ScalaJack(MsgPackFlavor())

  describe("------------------------------------\n:  Any Primitives Tests (msgpack)  :\n------------------------------------") {
    describe("+++ Positive Tests +++") {
      it("null works") {
        val shell = AnyShell(null)
        val bytes = sj.render(shell)
        assertResult("""81 A1 61 C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(null) {
          sj.read[AnyShell](bytes).a
        }
      }
      //
      // BigDecimal and BigInt are not inferred for Any with msgpack
      //
      it("Boolean works") {
        val payload = true
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 C3""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && parsed.isInstanceOf[Boolean] // boolean becomes Boolean
        }
      }
      it("Byte works") {
        val payload: Byte = 16
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 10""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && parsed.isInstanceOf[Int] // byte becomes Byte
        }
      }
      it("Char works") {
        val payload: Char = 'Z'
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 A1 5A""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload.toString) && parsed.isInstanceOf[String] // Char becomes String
        }
      }
      it("Double works") {
        val payload: Double = 1234.5678
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 CB 40 93 4A 45 6D 5C FA AD""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && parsed.isInstanceOf[Double] // double becomes Double
        }
      }
      it("Enumeration works") {
        val payload: Size.Value = Size.Small
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 A5 53 6D 61 6C 6C""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload.toString) && parsed.isInstanceOf[String] // enum value becomes String
        }
      }
      it("Float works") {
        val payload: Float = 1234.5678F
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 CA 44 9A 52 2B""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && parsed.isInstanceOf[Float] // float becomes Float
        }
      }
      it("Int works") {
        val payload: Int = 1234
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 CD 04 D2""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && parsed.isInstanceOf[Int] // int becomes Short
        }
      }
      it("Long works") {
        val payload: Long = 123456789012345L
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 CF 00 00 70 48 86 0D DF 79""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && parsed.isInstanceOf[BigInt] // long becomes Long (Note this could become Integer if smaller number parsed)
        }
        val payload2: Long = 123L
        val bytes2 = sj.render(AnyShell(payload2))
        assertResult("""81 A1 61 7B""") { bytes2.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes2).a
          (parsed == payload2) && parsed.isInstanceOf[Int] // long becomes Byte due to small number size
        }
      }
      it("Short works") {
        val payload: Short = 16234
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 CD 3F 6A""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && parsed.isInstanceOf[Int] // short becomes Short
        }
      }
      it("String works") {
        val payload = "something"
        val bytes = sj.render(AnyShell(payload))
        assertResult("""81 A1 61 A9 73 6F 6D 65 74 68 69 6E 67""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(true) {
          val parsed = sj.read[AnyShell](bytes).a
          (parsed == payload) && (parsed.getClass == payload.getClass)
        }
      }
    }
    // describe("--- Negative Tests ---") {
    //   No real negative tests yet... can't think of how to break Any primitives, given well-formed bytesON input.
    //   It may not infer what you want/expect, but it should always infer something.
    // }
  }
}
