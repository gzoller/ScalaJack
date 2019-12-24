package co.blocke.scalajack
package yaml
package primitives

import co.blocke.scalajack.model.JackFlavor
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class AnyPrim() extends AnyFunSpec with Matchers {

  val sj: JackFlavor[YAML] = ScalaJack(YamlFlavor())

  describe(
    "--------------------------------\n:  Any Primitive Tests (YAML)  :\n--------------------------------"
  ) {
    describe("+++ Positive Tests +++") {
      it("null works") {
        val shell = AnyShell(null)
        val yaml  = sj.render(shell)
        assertResult("""a: null""") { yaml.trim }
        assertResult(null) {
          sj.read[AnyShell](yaml).a
        }
      }
      it("BigDecimal works") {
        val payload = BigDecimal("12345678901234567890.12345678901234567890")
        val yaml    = sj.render(AnyShell(payload))
        assertResult("""a: 12345678901234567890.12345678901234567890""") {
          yaml.trim
        }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && (parsed.getClass == payload.getClass)
        }
      }
      it("BigInt works") {
        val payload = BigInt("12345678901234567890")
        val yaml    = sj.render(AnyShell(payload))
        assertResult("""a: 12345678901234567890""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && parsed.isInstanceOf[BigDecimal]
        }
      }
      it("Boolean works") {
        val payload = true
        val yaml    = sj.render(AnyShell(payload))
        assertResult("""a: true""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && parsed
            .isInstanceOf[Boolean] // boolean becomes Boolean
        }
      }
      it("Byte works") {
        val payload: Byte = 16
        val yaml          = sj.render(AnyShell(payload))
        assertResult("""a: 16""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && parsed
            .isInstanceOf[Integer] // byte becomes Integer
        }
      }
      it("Char works") {
        val payload: Char = 'Z'
        val yaml          = sj.render(AnyShell(payload))
        assertResult("""a: Z""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload.toString) && parsed
            .isInstanceOf[String] // Char becomes String
        }
      }
      it("Double works") {
        val payload: Double = 1234.5678
        val yaml            = sj.render(AnyShell(payload))
        assertResult("""a: 1234.5678""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && parsed
            .isInstanceOf[Double] // double becomes Double
        }
      }
      it("Enumeration works") {
        val payload: Size.Value = Size.Small
        val yaml                = sj.render(AnyShell(payload))
        assertResult("""a: Small""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload.toString) && parsed
            .isInstanceOf[String] // enum value becomes String
        }
      }
      it("Float works") {
        val payload: Float = 1234.5678F
        val yaml           = sj.render(AnyShell(payload))
        assertResult("""a: 1234.5677""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed.toString == payload.toString) && parsed
            .isInstanceOf[Double] // float becomes Double
        }
      }
      it("Int works") {
        val payload: Int = 1234567
        val yaml         = sj.render(AnyShell(payload))
        assertResult("""a: 1234567""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && parsed.isInstanceOf[Int] // int becomes Int
        }
      }
      it("Long works") {
        val payload: Long = 123456789012345L
        val yaml          = sj.render(AnyShell(payload))
        assertResult("""a: 123456789012345""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && parsed
            .isInstanceOf[java.lang.Long] // long becomes Long (Note this could become Integer if smaller number parsed)
        }
        val payload2: Long = 123L
        val yaml2          = sj.render(AnyShell(payload2))
        assertResult("""a: 123""") { yaml2.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml2).a
          (parsed == payload2) && parsed
            .isInstanceOf[Int] // long becomes Byte due to small number size
        }
      }
      it("Short works") {
        val payload: Short = 16234
        val yaml           = sj.render(AnyShell(payload))
        assertResult("""a: 16234""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
          (parsed == payload) && parsed.isInstanceOf[Int] // short becomes Int
        }
      }
      it("String works") {
        val payload = "something"
        val yaml    = sj.render(AnyShell(payload))
        assertResult("""a: something""") { yaml.trim }
        assertResult(true) {
          val parsed = sj.read[AnyShell](yaml).a
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
