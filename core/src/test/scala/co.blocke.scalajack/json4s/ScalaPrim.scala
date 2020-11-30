package co.blocke.scalajack
package json4s

import TestUtil._
import munit._
import munit.internal.console
import scala.math.BigDecimal
import org.json4s.JsonDSL._
import org.json4s._
import co.blocke.scalajack.json4s.Json4sFlavor

class ScalaPrim() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack(Json4sFlavor())

  test("BigDecimal must work") {
    describe(
      "------------------------------------\n:  Scala Primitive Tests (Json4s)  :\n------------------------------------", Console.BLUE
    )
    val inst = SampleBigDecimal(
      BigDecimal(123L),
      BigDecimal(1.23),
      BigDecimal(0),
      BigDecimal("123.456"),
      BigDecimal(
        "0.1499999999999999944488848768742172978818416595458984375"
      ),
      null
    )
    val js4s = sj.render(inst)
    val expected = JObject() ~ ("bd1" -> JDecimal(123L)) ~ ("bd2" -> JDecimal(
      1.23
    )) ~ ("bd3" -> JDecimal(0)) ~ ("bd4" -> JDecimal(123.456)) ~ ("bd5" -> JDecimal(
        BigDecimal(
          "0.1499999999999999944488848768742172978818416595458984375"
        )
      )) ~ ("bd6" -> JNull)
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[SampleBigDecimal](js4s))
  }

  test("BigInt must work") {
    val inst = SampleBigInt(
      BigInt("-90182736451928374653345"),
      BigInt("90182736451928374653345"),
      BigInt(0),
      null
    )
    val js4s = sj.render(inst)
    val expected = JObject() ~ ("bi1" -> JInt(
      BigInt("-90182736451928374653345")
    )) ~ ("bi2" -> JInt(BigInt("90182736451928374653345"))) ~ ("bi3" -> JInt(0)) ~ ("bi4" -> JNull)
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[SampleBigInt](js4s))
  }

  test("Boolean must work (not nullable)") {
    val inst = SampleBoolean(true, false)
    val js4s = sj.render(inst)
    val expected = JObject() ~ ("bool1" -> JBool(true)) ~ ("bool2" -> JBool(
      false
    ))
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[SampleBoolean](js4s))
  }

  test("Double must work (not nullable)") {
    val inst =
      SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
    val js4s = sj.render(inst)
    val expected = JObject() ~ ("d1" -> JDouble(Double.MaxValue)) ~ ("d2" -> JDouble(
      Double.MinValue
    )) ~ ("d3" -> JDouble(0.0)) ~ ("d4" -> JDouble(-123.4567))
    assert(Diff(JNothing, JNothing, JNothing) ==js4s.diff(expected) )
    assertEquals(inst, sj.read[SampleDouble](js4s))
  }

  test("Enumeration must work (not nullable)") {
    val inst =
      SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
    val js4s = sj.render(inst)
    val expected = JObject() ~ ("e1" -> JString("Small")) ~ ("e2" -> JString("Medium"))
      ~ ("e3" -> JString("Large")) ~ ("e4" -> JNull) ~ ("e5" -> JString("Medium"))
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    // mutate e5 into an ordinal...
    val js2 = js4s.asInstanceOf[JObject] ~ ("e5" -> JInt(1))
    assertEquals(inst, sj.read[SampleEnum](js2))
  }

  test("Enumerations as Ints must work") {
    val sj2 = sj.enumsAsInts()
    val inst =
      SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
    val js4s = sj2.render(inst)
    val expected = JObject() ~ ("e1" -> JInt(0)) ~ ("e2" -> JInt(1)) ~ ("e3" -> JInt(
      2
    )) ~ ("e4" -> JNull) ~ ("e5" -> JInt(1))
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj2.read[SampleEnum](js4s))
  }

  test("Int must work (not nullable)") {
    val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
    val js4s = sj.render(inst)
    val expected = JObject() ~ ("i1" -> JInt(Int.MaxValue)) ~ ("i2" -> JInt(
      Int.MinValue
    )) ~ ("i3" -> JInt(0)) ~ ("i4" -> JInt(123))
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[SampleInt](js4s))
  }

  test("Long must work (not nullable)") {
    val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
    val js4s = sj.render(inst)
    val expected = JObject() ~ ("l1" -> JLong(Long.MaxValue)) ~ ("l2" -> JLong(
      Long.MinValue
    )) ~ ("l3" -> JLong(0L)) ~ ("l4" -> JLong(123L))
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[SampleLong](js4s))
  }

  test("Map of string-wrapped primitives work") {
    val inst = WrappedMaps(
      Map(3.toByte -> 2),
      Map(1 -> 2),
      Map(5L -> 7),
      Map(1.2 -> 3),
      Map(1.2F -> 3),
      Map(2.toShort -> 9),
      Map(BigInt(5) -> 6),
      Map(BigDecimal(4.9) -> 8),
      Map(true -> 1),
      Map('c' -> 1),
      Map("stuff" -> 99)
    )
    val js4s = sj.render(inst)
    val expected = JObject(
      List(
        "a" -> JObject(List("3" -> JInt(2))),
        "b" -> JObject(List("1" -> JInt(2))),
        "c" -> JObject(List("5" -> JInt(7))),
        "d" -> JObject(List("1.2" -> JInt(3))),
        "e" -> JObject(List("1.2" -> JInt(3))),
        "f" -> JObject(List("2" -> JInt(9))),
        "g" -> JObject(List("5" -> JInt(6))),
        "h" -> JObject(List("4.9" -> JInt(8))),
        "i" -> JObject(List("true" -> JInt(1))),
        "j" -> JObject(List("c" -> JInt(1))),
        "k" -> JObject(List("stuff" -> JInt(99)))
      )
    )
    assert(Diff(JNothing, JNothing, JNothing) == js4s.diff(expected) )
    assertEquals(inst, sj.read[WrappedMaps](js4s))
  }
