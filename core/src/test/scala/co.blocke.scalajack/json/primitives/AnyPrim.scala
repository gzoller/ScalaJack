package co.blocke.scalajack
package json.primitives

import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import co.blocke.scala_reflection._

class AnyPrim() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("null works") {
    describe("-------------------------\n:  Any Primitive Tests  :\n-------------------------", Console.BLUE)

    val shell = AnyShell(null)
    val js = sj.render(shell)
    assertEquals("""{"a":null}""".asInstanceOf[JSON],js)
    assert(sj.read[AnyShell](js).a == null)
  }

  test("BigDecimal works") {
    val payload = BigDecimal("12345678901234567890.12345678901234567890")
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":12345678901234567890.12345678901234567890}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && (parsed.getClass == payload.getClass)
    })
  }

  test("BigInt works") {
    val payload = BigInt("12345678901234567890")
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":12345678901234567890}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && parsed.isInstanceOf[BigInt]
    })
  }

  test("Boolean works") {
    val payload = true
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":true}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && parsed.isInstanceOf[Boolean]
    })
  }

  test("Byte works") {
    val payload: Byte = 16
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":16}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && parsed.isInstanceOf[Integer] // byte becomes Integer
    })
  }

  test("Char works") {
    val payload: Char = 'Z'
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":"Z"}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload.toString) && parsed.isInstanceOf[String] // Char becomes String
    })
  }

  test("Double works") {
    val payload: Double = 1234.5678
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":1234.5678}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && parsed.isInstanceOf[Double]
    })
  }

  test("Enumeration works") {
    val payload: Size.Value = Size.Small
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":"Small"}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload.toString) && parsed.isInstanceOf[String] // enum value becomes String
    })
  }

  test("Enum works") {
    val payload = Color.Blue
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":"Blue"}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload.toString) && parsed.isInstanceOf[String] // enum value becomes String
    })
  }

  test("Float works") {
    val payload: Float = 1234.5678F
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":1234.5677}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed.toString == payload.toString) && parsed.isInstanceOf[Double] // float becomes Double
    })
  }

  test("Int works") {
    val payload: Int = 1234567
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":1234567}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && parsed.isInstanceOf[Int]
    })
  }    

  test("Long works") {
    val payload: Long = 123456789012345L
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":123456789012345}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && parsed.isInstanceOf[Long] // (Note this could become Integer if smaller number parsed)
    })

    val payload2: Long = 123L
    val js2 = sj.render(AnyShell(payload2))
    assertEquals("""{"a":123}""".asInstanceOf[JSON], js2)
    assert({
      val parsed = sj.read[AnyShell](js2).a
      (parsed == payload2) && parsed.isInstanceOf[Int] // Long became Int due to smaller size
    })
  }

  test("Short works") {
    val payload: Short = 16234
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":16234}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && parsed.isInstanceOf[Int] // short becomes Int
    })
  }    

  test("String works") {
    val payload = "something"
    val js = sj.render(AnyShell(payload))
    assertEquals("""{"a":"something"}""".asInstanceOf[JSON], js)
    assert({
      val parsed = sj.read[AnyShell](js).a
      (parsed == payload) && (parsed.getClass == payload.getClass)
    })
  }