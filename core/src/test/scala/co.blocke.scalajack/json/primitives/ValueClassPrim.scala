package co.blocke.scalajack
package json.primitives

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID

class ValueClassPrim() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("--------------------------------\n:  ValueClass DelimSpec Tests  :\n--------------------------------") {
    describe("+++ Positive Tests +++") {
      it("Value class of BigDecimal") {
        val inst = VCBigDecimal(BigDecimal(12.34))
        val js = sj.render(inst)
        assertResult("""12.34""") { js }
        assertResult(inst) {
          sj.read[VCBigDecimal](js)
        }
      }
      it("Value class of BigDecimal with null") {
        val inst = VCBigDecimal(null)
        val js = sj.render(inst)
        assertResult("""null""") { js }
        assertResult(inst) {
          sj.read[VCBigDecimal](js)
        }
      }
      it("Value class of BigInt") {
        val inst = VCBigInt(BigInt(1))
        val js = sj.render(inst)
        assertResult("""1""") { js }
        assertResult(inst) {
          sj.read[VCBigInt](js)
        }
      }
      it("Value class of BigInt with null") {
        val inst = VCBigInt(null)
        val js = sj.render(inst)
        assertResult("""null""") { js }
        assertResult(inst) {
          sj.read[VCBigInt](js)
        }
      }
      it("Value class of Byte") {
        val inst = VCByte(100.asInstanceOf[Byte])
        val js = sj.render(inst)
        assertResult("""100""") { js }
        assertResult(inst) {
          sj.read[VCByte](js)
        }
      }
      it("Value class of Boolean") {
        val inst = VCBoolean(false)
        val js = sj.render(inst)
        assertResult("""false""") { js }
        assertResult(inst) {
          sj.read[VCBoolean](js)
        }
      }
      it("Value class of Char") {
        val inst = VCChar('Z')
        val js = sj.render(inst)
        assertResult(""""Z"""") { js }
        assertResult(inst) {
          sj.read[VCChar](js)
        }
      }
      it("Value class of Double") {
        val inst = VCDouble(100.5)
        val js = sj.render(inst)
        assertResult("""100.5""") { js }
        assertResult(inst) {
          sj.read[VCDouble](js)
        }
      }
      it("Value class of Enumeration") {
        val inst = VCEnumeration(Size.Medium)
        val js = sj.render(inst)
        assertResult(""""Medium"""") { js }
        assertResult(inst) {
          sj.read[VCEnumeration](js)
        }
      }
      it("Value class of Enumeration with null") {
        val inst = VCEnumeration(null)
        val js = sj.render(inst)
        assertResult("""null""") { js }
        assertResult(inst) {
          sj.read[VCEnumeration](js)
        }
      }
      it("Value class of Float") {
        val inst = VCFloat(100.5F)
        val js = sj.render(inst)
        assertResult("""100.5""") { js }
        assertResult(inst) {
          sj.read[VCFloat](js)
        }
      }
      it("Value class of Int") {
        val inst = VCInt(100)
        val js = sj.render(inst)
        assertResult("""100""") { js }
        assertResult(inst) {
          sj.read[VCInt](js)
        }
      }
      it("Value class of Long") {
        val inst = VCLong(100L)
        val js = sj.render(inst)
        assertResult("""100""") { js }
        assertResult(inst) {
          sj.read[VCLong](js)
        }
      }
      it("Value class of Short") {
        val inst = VCShort(100.asInstanceOf[Short])
        val js = sj.render(inst)
        assertResult("""100""") { js }
        assertResult(inst) {
          sj.read[VCShort](js)
        }
      }
      it("Value class of String") {
        val inst = VCString("foo")
        val js = sj.render(inst)
        assertResult(""""foo"""") { js }
        assertResult(inst) {
          sj.read[VCString](js)
        }
      }
      it("Value class of String with null") {
        val inst = VCString(null)
        val js = sj.render(inst)
        assertResult("""null""") { js }
        assertResult(inst) {
          sj.read[VCString](js)
        }
      }
      it("Value class of UUID") {
        val inst = VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))
        val js = sj.render(inst)
        assertResult(""""54cab778-7b9e-4b07-9d37-87b97a011e55"""") { js }
        assertResult(inst) {
          sj.read[VCUUID](js)
        }
      }
      it("Value class of UUID with null") {
        val inst = VCUUID(null)
        val js = sj.render(inst)
        assertResult("""null""") { js }
        assertResult(inst) {
          sj.read[VCUUID](js)
        }
      }
      it("Value class of Number") {
        val inst = VCNumber(25)
        val js = sj.render(inst)
        assertResult("""25""") { js }
        assertResult((inst, true)) {
          val r = sj.read[VCNumber](js)
          (r, r.vc.isInstanceOf[Byte])
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Wrong JSON for wrapped type") {
        val js = """100.25"""
        val msg = """[$]: Unable to read value (e.g. bad number format)
                    |100.25
                    |-----^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[VCShort](js) should have message msg
      }
    }
  }
}
