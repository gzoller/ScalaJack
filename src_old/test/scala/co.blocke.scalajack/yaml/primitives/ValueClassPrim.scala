package co.blocke.scalajack
package yaml
package primitives

import TestUtil._
import munit._
import munit.internal.console
import java.util.UUID

class ValueClassPrim() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Value class of BigDecimal") {
    describe(
      "---------------------------------------\n:  ValueClass Primitive Tests (YAML)  :\n---------------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
    val inst = VCBigDecimal(BigDecimal(12.34))
    val yaml = sj.render(inst)
    assertEquals("""12.34""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCBigDecimal](yaml))
  }

  test("Value class of BigDecimal with null") {
    val inst = VCBigDecimal(null)
    val yaml = sj.render(inst)
    assertEquals("""null""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCBigDecimal](yaml))
  }

  test("Value class of BigInt") {
    val inst = VCBigInt(BigInt(1))
    val yaml = sj.render(inst)
    assertEquals("""1""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCBigInt](yaml))
  }

  test("Value class of BigInt with null") {
    val inst = VCBigInt(null)
    val yaml = sj.render(inst)
    assertEquals("""null""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCBigInt](yaml))
  }

  test("Value class of Byte") {
    val inst = VCByte(100.asInstanceOf[Byte])
    val yaml = sj.render(inst)
    assertEquals("""100""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCByte](yaml))
  }

  test("Value class of Boolean") {
    val inst = VCBoolean(false)
    val yaml = sj.render(inst)
    assertEquals("""false""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCBoolean](yaml))
  }

  test("Value class of Char") {
    val inst = VCChar('Z')
    val yaml = sj.render(inst)
    assertEquals("""Z""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCChar](yaml))
  }

  test("Value class of Double") {
    val inst = VCDouble(100.5)
    val yaml = sj.render(inst)
    assertEquals("""100.5""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCDouble](yaml))
  }

  test("Value class of Enumeration") {
    val inst = VCEnumeration(Size.Medium)
    val yaml = sj.render(inst)
    assertEquals("""Medium""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCEnumeration](yaml))
  }

  test("Value class of Enumeration with null") {
    val inst = VCEnumeration(null)
    val yaml = sj.render(inst)
    assertEquals("""null""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCEnumeration](yaml))
  }

  test("Value class of Float") {
    val inst = VCFloat(100.5F)
    val yaml = sj.render(inst)
    assertEquals("""100.5""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCFloat](yaml))
  }

  test("Value class of Int") {
    val inst = VCInt(100)
    val yaml = sj.render(inst)
    assertEquals("""100""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCInt](yaml))
  }

  test("Value class of Long") {
    val inst = VCLong(100L)
    val yaml = sj.render(inst)
    assertEquals("""100""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCLong](yaml))
  }

  test("Value class of Short") {
    val inst = VCShort(100.asInstanceOf[Short])
    val yaml = sj.render(inst)
    assertEquals("""100""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCShort](yaml))
  }

  test("Value class of String") {
    val inst = VCString("foo")
    val yaml = sj.render(inst)
    assertEquals("""foo""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCString](yaml))
  }

  test("Value class of String with null") {
    val inst = VCString(null)
    val yaml = sj.render(inst)
    assertEquals("""null""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCString](yaml))
  }

  test("Value class of UUID") {
    val inst =
      VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))
    val yaml = sj.render(inst)
    assertEquals("""54cab778-7b9e-4b07-9d37-87b97a011e55""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCUUID](yaml))
  }

  test("Value class of UUID with null") {
    val inst = VCUUID(null)
    val yaml = sj.render(inst)
    assertEquals("""null""",  yaml.asInstanceOf[String].trim )
    assertEquals(inst, sj.read[VCUUID](yaml))
  }

  test("Value class of Number") {
    val inst = VCNumber(25)
    val yaml = sj.render(inst)
    assertEquals("""25""",  yaml.asInstanceOf[String].trim )
    assertEquals((inst, true), {
      val r = sj.read[VCNumber](yaml)
      (r, r.vc.isInstanceOf[Byte])
    })
  }

  test("Wrong YAML for wrapped type") {
    describe("--- Negative Tests ---") 
    val yaml = """100.25""".asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Cannot parse an Short from value"){
      sj.read[VCShort](yaml)
    }
  }
