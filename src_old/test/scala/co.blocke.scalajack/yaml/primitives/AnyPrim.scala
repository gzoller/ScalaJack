package co.blocke.scalajack
package yaml
package primitives

import co.blocke.scalajack.model.JackFlavor
import TestUtil._
import munit._
import munit.internal.console

class AnyPrim() extends FunSuite:

  val sj: JackFlavor[YAML] = ScalaJack(YamlFlavor())

  test("null works") {
    describe(
      "--------------------------------\n:  Any Primitive Tests (YAML)  :\n--------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
    val shell = AnyShell(null)
    val yaml  = sj.render(shell)
    assertEquals("""a: null""",  yaml.asInstanceOf[String].trim )
    assert(null == sj.read[AnyShell](yaml).a)
  }

  test("BigDecimal works") {
    val payload = BigDecimal("12345678901234567890.12345678901234567890")
    val yaml    = sj.render(AnyShell(payload))
    assertEquals("""a: 12345678901234567890.12345678901234567890""", yaml.asInstanceOf[String].trim)
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && (parsed.getClass == payload.getClass)
    })
  }

  test("BigInt works") {
    val payload = BigInt("12345678901234567890")
    val yaml    = sj.render(AnyShell(payload))
    assertEquals("""a: 12345678901234567890""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && parsed.isInstanceOf[BigInt]
    })
  }

  test("Boolean works") {
    val payload = true
    val yaml    = sj.render(AnyShell(payload))
    assertEquals("""a: true""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && parsed
        .isInstanceOf[Boolean] // boolean becomes Boolean
    })
  }

  test("Byte works") {
    val payload: Byte = 16
    val yaml          = sj.render(AnyShell(payload))
    assertEquals("""a: 16""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && parsed
        .isInstanceOf[Integer] // byte becomes Integer
    })
  }

  test("Char works") {
    val payload: Char = 'Z'
    val yaml          = sj.render(AnyShell(payload))
    assertEquals("""a: Z""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload.toString) && parsed
        .isInstanceOf[String] // Char becomes String
    })
  }

  test("Double works") {
    val payload: Double = 1234.5678
    val yaml            = sj.render(AnyShell(payload))
    assertEquals("""a: 1234.5678""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && parsed
        .isInstanceOf[Double] // double becomes Double
    })
  }

  test("Enumeration works") {
    val payload: Size.Value = Size.Small
    val yaml                = sj.render(AnyShell(payload))
    assertEquals("""a: Small""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload.toString) && parsed
        .isInstanceOf[String] // enum value becomes String
    })
  }

  test("Float works") {
    val payload: Float = 1234.5678F
    val yaml           = sj.render(AnyShell(payload))
    assertEquals("""a: 1234.5677""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed.toString == payload.toString) && parsed
        .isInstanceOf[Double] // float becomes Double
    })
  }

  test("Int works") {
    val payload: Int = 1234567
    val yaml         = sj.render(AnyShell(payload))
    assertEquals("""a: 1234567""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && parsed.isInstanceOf[Int] // int becomes Int
    })
  }

  test("Long works") {
    val payload: Long = 123456789012345L
    val yaml          = sj.render(AnyShell(payload))
    assertEquals("""a: 123456789012345""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && parsed
        .isInstanceOf[java.lang.Long] // long becomes Long (Note this could become Integer if smaller number parsed)
    })
    val payload2: Long = 123L
    val yaml2          = sj.render(AnyShell(payload2))
    assertEquals("""a: 123""",  yaml2.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml2).a
      (parsed == payload2) && parsed
        .isInstanceOf[Int] // long becomes Byte due to small number size
    })
  }

  test("Short works") {
    val payload: Short = 16234
    val yaml           = sj.render(AnyShell(payload))
    assertEquals("""a: 16234""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && parsed.isInstanceOf[Int] // short becomes Int
    })
  }

  test("String works") {
    val payload = "something"
    val yaml    = sj.render(AnyShell(payload))
    assertEquals("""a: something""",  yaml.asInstanceOf[String].trim )
    assertEquals(true, {
      val parsed = sj.read[AnyShell](yaml).a
      (parsed == payload) && (parsed.getClass == payload.getClass)
    })
  }

  // describe("--- Negative Tests ---") {
  //   No real negative tests yet... can't think of how to break Any primitives, given well-formed JSON input.
  //   It may not infer what you want/expect, but it should always infer something.
  // }
