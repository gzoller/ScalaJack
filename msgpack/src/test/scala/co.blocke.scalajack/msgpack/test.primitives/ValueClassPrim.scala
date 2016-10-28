package co.blocke.scalajack
package msgpack
package test.primitives

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID

class ValueClassPrim() extends FunSpec with Matchers {

  val sj = ScalaJack(MsgPackFlavor())

  describe("-------------------------------------------\n:  ValueClass Primitives Tests (msgpack)  :\n-------------------------------------------") {
    describe("+++ Positive Tests +++") {
      it("Value class of BigDecimal") {
        val inst = VCBigDecimal(BigDecimal(12.34))
        val bytes = sj.render(inst)
        assertResult("""A5 31 32 2E 33 34""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCBigDecimal](bytes)
        }
      }
      it("Value class of BigDecimal with null") {
        val inst = VCBigDecimal(null)
        val bytes = sj.render(inst)
        assertResult("""C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCBigDecimal](bytes)
        }
      }
      it("Value class of BigInt") {
        val inst = VCBigInt(BigInt(1))
        val bytes = sj.render(inst)
        assertResult("""A1 31""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCBigInt](bytes)
        }
      }
      it("Value class of BigInt with null") {
        val inst = VCBigInt(null)
        val bytes = sj.render(inst)
        assertResult("""C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCBigInt](bytes)
        }
      }
      it("Value class of Byte") {
        val inst = VCByte(100.asInstanceOf[Byte])
        val bytes = sj.render(inst)
        assertResult("""64""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCByte](bytes)
        }
      }
      it("Value class of Boolean") {
        val inst = VCBoolean(false)
        val bytes = sj.render(inst)
        assertResult("""C2""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCBoolean](bytes)
        }
      }
      it("Value class of Char") {
        val inst = VCChar('Z')
        val bytes = sj.render(inst)
        assertResult("""A1 5A""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCChar](bytes)
        }
      }
      it("Value class of Double") {
        val inst = VCDouble(100.5)
        val bytes = sj.render(inst)
        assertResult("""CB 40 59 20 00 00 00 00 00""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCDouble](bytes)
        }
      }
      it("Value class of Enumeration") {
        val inst = VCEnumeration(Size.Medium)
        val bytes = sj.render(inst)
        assertResult("""A6 4D 65 64 69 75 6D""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCEnumeration](bytes)
        }
      }
      it("Value class of Enumeration with null") {
        val inst = VCEnumeration(null)
        val bytes = sj.render(inst)
        assertResult("""C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCEnumeration](bytes)
        }
      }
      it("Value class of Float") {
        val inst = VCFloat(100.5F)
        val bytes = sj.render(inst)
        assertResult("""CA 42 C9 00 00""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCFloat](bytes)
        }
      }
      it("Value class of Int") {
        val inst = VCInt(100)
        val bytes = sj.render(inst)
        assertResult("""64""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCInt](bytes)
        }
      }
      it("Value class of Long") {
        val inst = VCLong(100L)
        val bytes = sj.render(inst)
        assertResult("""64""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCLong](bytes)
        }
      }
      it("Value class of Short") {
        val inst = VCShort(100.asInstanceOf[Short])
        val bytes = sj.render(inst)
        assertResult("""64""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCShort](bytes)
        }
      }
      it("Value class of String") {
        val inst = VCString("foo")
        val bytes = sj.render(inst)
        assertResult("""A3 66 6F 6F""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCString](bytes)
        }
      }
      it("Value class of String with null") {
        val inst = VCString(null)
        val bytes = sj.render(inst)
        assertResult("""C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCString](bytes)
        }
      }
      it("Value class of UUID") {
        val inst = VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))
        val bytes = sj.render(inst)
        assertResult("""DA 00 24 35 34 63 61 62 37 37 38 2D 37 62 39 65 2D 34 62 30 37 2D 39 64 33 37 2D 38 37 62 39 37 61 30 31 31 65 35 35""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCUUID](bytes)
        }
      }
      it("Value class of UUID with null") {
        val inst = VCUUID(null)
        val bytes = sj.render(inst)
        assertResult("""C0""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult(inst) {
          sj.read[VCUUID](bytes)
        }
      }
      it("Value class of Number") {
        val inst = VCNumber(25)
        val bytes = sj.render(inst)
        assertResult("""19""") { bytes.map("%02X" format _).mkString(" ") }
        assertResult((inst, true)) {
          val r = sj.read[VCNumber](bytes)
          (r, r.vc.isInstanceOf[Int])
        }
      }
    }
  }
}
