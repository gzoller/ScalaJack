package co.blocke.scalajack
package json.primitives

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import co.blocke.scala_reflection._
import java.util.UUID


class ValueClassPrim() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("Value class of BigDecimal") {
    describe("--------------------------------\n:  ValueClass Primitive Tests  :\n--------------------------------", Console.BLUE)
    describe("+++ Positive Tests +++")

    val inst = VCBigDecimal(BigDecimal(12.34))
    val js = sj.render(inst)
    assertEquals("""12.34""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCBigDecimal](js))
  }

  test("Value class of BigDecimal with null") {
    val inst = VCBigDecimal(null)
    val js = sj.render(inst)
    assertEquals("""null""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCBigDecimal](js))
  }

  test("Value class of BigInt") {
    val inst = VCBigInt(BigInt(1))
    val js = sj.render(inst)
    assertEquals("""1""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCBigInt](js))
  }

  test("Value class of BigInt with null") {
    val inst = VCBigInt(null)
    val js = sj.render(inst)
    assertEquals("""null""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCBigInt](js))
  }

  test("Value class of Byte") {
    val inst = VCByte(100.asInstanceOf[Byte])
    val js = sj.render(inst)
    assertEquals("""100""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCByte](js))
  }

  test("Value class of Boolean") {
    val inst = VCBoolean(false)
    val js = sj.render(inst)
    assertEquals("""false""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCBoolean](js))
  }

  test("Value class of Char") {
    val inst = VCChar('Z')
    val js = sj.render(inst)
    assertEquals(""""Z"""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCChar](js))
  }

  test("Value class of Double") {
    val inst = VCDouble(100.5)
    val js = sj.render(inst)
    assertEquals("""100.5""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCDouble](js))
  }

  test("Value class of Enumeration") {
    val inst = VCEnumeration(Size.Medium)
    val js = sj.render(inst)
    assertEquals(""""Medium"""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCEnumeration](js))
  }

  test("Value class of Enumeration with null") {
    val inst = VCEnumeration(null)
    val js = sj.render(inst)
    assertEquals("""null""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCEnumeration](js))
  }

  test("Value class of Enum") {
    val inst = VCEnum(Color.Green)
    val js = sj.render(inst)
    assertEquals(""""Green"""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCEnum](js))
  }

  test("Value class of Enum with null") {
    val inst = VCEnum(null)
    val js = sj.render(inst)
    assertEquals("""null""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCEnum](js))
  }

  test("Value class of Float") {
    val inst = VCFloat(100.5F)
    val js = sj.render(inst)
    assertEquals("""100.5""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCFloat](js))
  }

  test("Value class of Int") {
    val inst = VCInt(100)
    val js = sj.render(inst)
    assertEquals("""100""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCInt](js))
  }

  test("Value class of Long") {
    val inst = VCLong(100L)
    val js = sj.render(inst)
    assertEquals("""100""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCLong](js))
  }

  test("Value class of Short") {
    val inst = VCShort(100.asInstanceOf[Short])
    val js = sj.render(inst)
    assertEquals("""100""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCShort](js))
  }

  test("Value class of String") {
    val inst = VCString("foo")
    val js = sj.render(inst)
    assertEquals(""""foo"""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCString](js))
  }

  test("Value class of String with null") {
    val inst = VCString(null)
    val js = sj.render(inst)
    assertEquals("""null""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCString](js))
  }

  test("Value class of UUID") {
    val inst = VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))
    val js = sj.render(inst)
    assertEquals(""""54cab778-7b9e-4b07-9d37-87b97a011e55"""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCUUID](js))
  }

  test("Value class of UUID with null") {
    val inst = VCUUID(null)
    val js = sj.render(inst)
    assertEquals("""null""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[VCUUID](js))
  }

  test("Value class of Number") {
    val inst = VCNumber(25)
    val js = sj.render(inst)
    assertEquals("""25""".asInstanceOf[JSON], js)
    assertEquals((inst,true), {
      val r = sj.read[VCNumber](js)
      (r, r.vc.isInstanceOf[Byte])
    })
  }

  test("Wrong JSON for wrapped type") {
    describe("--- Negative Tests ---")

    val js = """100.25""".asInstanceOf[JSON]
    val msg = """Cannot parse an Short from value
                      |100.25
                      |-----^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[VCShort](js) 
    }
  }
