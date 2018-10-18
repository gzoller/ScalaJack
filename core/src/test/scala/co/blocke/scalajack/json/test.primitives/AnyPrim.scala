package co.blocke.scalajack
package json.test.primitives

import org.scalatest.{ FunSpec, Matchers }

class AnyPrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("--------------------------\n:  Any Primitives Tests  :\n--------------------------") {
    describe("+++ Positive Tests +++") {
      it("null works") {
        val shell = AnyShell(null)
        val js = sj.render(shell)
        assertResult("""{"a":null}""") { js }
        assertResult(null) {
          sj.read[AnyShell](js).a
        }
      }
      it("BigDecimal works") {
        val payload = BigDecimal("12345678901234567890.12345678901234567890")
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":12345678901234567890.12345678901234567890}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload) && (parsed.getClass == payload.getClass)
        }
      }
      it("BigInt works") {
        val payload = BigInt("12345678901234567890")
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":12345678901234567890}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload) && (parsed.getClass == payload.getClass)
        }
      }
      it("Boolean works") {
        val payload = true
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":true}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload) && parsed.isInstanceOf[Boolean] // boolean becomes Boolean
        }
      }
      it("Byte works") {
        val payload: Byte = 16
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":16}""") { js }
        val parsed = sj.read[AnyShell](js).a
        assertResult(payload) {
          16L
        }
        assertResult(true) {
          parsed.isInstanceOf[Long] // byte becomes Integer
        }
      }
      it("Char works") {
        val payload: Char = 'Z'
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":"Z"}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload.toString) && parsed.isInstanceOf[String] // Char becomes String
        }
      }
      it("Double works") {
        val payload: Double = 1234.5678
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":1234.5678}""") { js }
        val parsed = sj.read[AnyShell](js).a
        assertResult(payload) {
          parsed
        }
        assertResult(true) {
          parsed.isInstanceOf[Double] // double becomes Double
        }
      }
      it("Enumeration works") {
        val payload: Size.Value = Size.Small
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":"Small"}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload.toString) && parsed.isInstanceOf[String] // enum value becomes String
        }
      }
      it("Float works") {
        // We lose 0.0001 precision here.  Don't know why.  Because Float :-(
        val payload: Float = 1234.5678F
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":1234.5677}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == 1234.5677) && parsed.isInstanceOf[Double] // float becomes Double
        }
      }
      it("Int works") {
        val payload: Int = 1234567
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":1234567}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload) && parsed.isInstanceOf[Long] // int becomes Long
        }
      }
      it("Long works") {
        val payload: Long = 123456789012345L
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":123456789012345}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload) && parsed.isInstanceOf[java.lang.Long] // long becomes Long
        }
      }
      it("Short works") {
        val payload: Short = 16234
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":16234}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload) && parsed.isInstanceOf[Long] // short becomes Int
        }
      }
      it("String works") {
        val payload = "something"
        val js = sj.render(AnyShell(payload))
        assertResult("""{"a":"something"}""") { js }
        assertResult(true) {
          val parsed = sj.read[AnyShell](js).a
          (parsed == payload) && (parsed.getClass == payload.getClass)
        }
      }
    }
    // describe("--- Negative Tests ---") {
    //   No real negative tests yet... can't think of how to break Any primitives, given well-formed JSON input.
    //   It may not infer what you want/expect, but it should always infer something.
    // }
  }
}
