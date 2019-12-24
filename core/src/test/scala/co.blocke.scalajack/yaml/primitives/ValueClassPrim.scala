package co.blocke.scalajack
package yaml
package primitives

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import java.util.UUID

class ValueClassPrim() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "---------------------------------------\n:  ValueClass Primitive Tests (YAML)  :\n---------------------------------------"
  ) {
    describe("+++ Positive Tests +++") {
      it("Value class of BigDecimal") {
        val inst = VCBigDecimal(BigDecimal(12.34))
        val yaml = sj.render(inst)
        assertResult("""12.34""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCBigDecimal](yaml)
        }
      }
      it("Value class of BigDecimal with null") {
        val inst = VCBigDecimal(null)
        val yaml = sj.render(inst)
        assertResult("""null""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCBigDecimal](yaml)
        }
      }
      it("Value class of BigInt") {
        val inst = VCBigInt(BigInt(1))
        val yaml = sj.render(inst)
        assertResult("""1""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCBigInt](yaml)
        }
      }
      it("Value class of BigInt with null") {
        val inst = VCBigInt(null)
        val yaml = sj.render(inst)
        assertResult("""null""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCBigInt](yaml)
        }
      }
      it("Value class of Byte") {
        val inst = VCByte(100.asInstanceOf[Byte])
        val yaml = sj.render(inst)
        assertResult("""100""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCByte](yaml)
        }
      }
      it("Value class of Boolean") {
        val inst = VCBoolean(false)
        val yaml = sj.render(inst)
        assertResult("""false""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCBoolean](yaml)
        }
      }
      it("Value class of Char") {
        val inst = VCChar('Z')
        val yaml = sj.render(inst)
        assertResult("""Z""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCChar](yaml)
        }
      }
      it("Value class of Double") {
        val inst = VCDouble(100.5)
        val yaml = sj.render(inst)
        assertResult("""100.5""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCDouble](yaml)
        }
      }
      it("Value class of Enumeration") {
        val inst = VCEnumeration(Size.Medium)
        val yaml = sj.render(inst)
        assertResult("""Medium""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCEnumeration](yaml)
        }
      }
      it("Value class of Enumeration with null") {
        val inst = VCEnumeration(null)
        val yaml = sj.render(inst)
        assertResult("""null""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCEnumeration](yaml)
        }
      }
      it("Value class of Float") {
        val inst = VCFloat(100.5F)
        val yaml = sj.render(inst)
        assertResult("""100.5""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCFloat](yaml)
        }
      }
      it("Value class of Int") {
        val inst = VCInt(100)
        val yaml = sj.render(inst)
        assertResult("""100""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCInt](yaml)
        }
      }
      it("Value class of Long") {
        val inst = VCLong(100L)
        val yaml = sj.render(inst)
        assertResult("""100""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCLong](yaml)
        }
      }
      it("Value class of Short") {
        val inst = VCShort(100.asInstanceOf[Short])
        val yaml = sj.render(inst)
        assertResult("""100""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCShort](yaml)
        }
      }
      it("Value class of String") {
        val inst = VCString("foo")
        val yaml = sj.render(inst)
        assertResult("""foo""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCString](yaml)
        }
      }
      it("Value class of String with null") {
        val inst = VCString(null)
        val yaml = sj.render(inst)
        assertResult("""null""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCString](yaml)
        }
      }
      it("Value class of UUID") {
        val inst =
          VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))
        val yaml = sj.render(inst)
        assertResult("""54cab778-7b9e-4b07-9d37-87b97a011e55""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCUUID](yaml)
        }
      }
      it("Value class of UUID with null") {
        val inst = VCUUID(null)
        val yaml = sj.render(inst)
        assertResult("""null""") { yaml.trim }
        assertResult(inst) {
          sj.read[VCUUID](yaml)
        }
      }
      it("Value class of Number") {
        val inst = VCNumber(25)
        val yaml = sj.render(inst)
        assertResult("""25""") { yaml.trim }
        assertResult((inst, true)) {
          val r = sj.read[VCNumber](yaml)
          (r, r.vc.isInstanceOf[Byte])
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Wrong YAML for wrapped type") {
        val yaml = """100.25"""
        the[ScalaJackError] thrownBy sj.read[VCShort](yaml) should have message "Line 0: Cannot parse an Short from value"
      }
    }
  }
}
