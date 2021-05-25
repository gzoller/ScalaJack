package co.blocke.scalajack
package yaml
package primitives

import scala.math.BigDecimal
import java.util.UUID
import TestUtil._
import munit._
import munit.internal.console

class ScalaPrim() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("BigDecimal must work") {
    describe(
      "----------------------------------\n:  Scala Primitive Tests (YAML)  :\n----------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
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
    val yaml       = sj.render(inst)
    val comparison = """bd1: !!float '123'
                        |bd2: 1.23
                        |bd3: !!float '0'
                        |bd4: 123.456
                        |bd5: 0.1499999999999999944488848768742172978818416595458984375
                        |bd6: null""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet))
    assertEquals(inst, sj.read[SampleBigDecimal](yaml))
  }

  test("BigInt must work") {
    val inst = SampleBigInt(
      BigInt("-90182736451928374653345"),
      BigInt("90182736451928374653345"),
      BigInt(0),
      null
    )
    val yaml = sj.render(inst)
    val comparison =
      """bi1: -90182736451928374653345
        |bi2: 90182736451928374653345
        |bi3: 0
        |bi4: null""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleBigInt](yaml))
  }

  test("Binary must work") {
    val inst = SampleBinary(
      null,
      hexStringToByteArray("e04fd020ea3a6910a2d808002b30309d")
    )
    val yaml = sj.render(inst)
    val comparison =
      """b1: null
        |b2: 4E/QIOo6aRCi2AgAKzAwnQ==""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    val inst2 = sj.read[SampleBinary](yaml)
    assert(null == inst2.b1 )
    assertEquals(true, inst.b2.toList == inst2.b2.toList)
  }

  test("Boolean must work (not nullable)") {
    val inst = SampleBoolean(true, false)
    val yaml = sj.render(inst)
    val comparison =
      """bool1: true
        |bool2: false""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleBoolean](yaml))
  }
  
  test("Byte must work (not nullable)") {
    val inst       = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
    val yaml       = sj.render(inst)
    val comparison = """b1: 127
                        |b2: -128
                        |b3: 0
                        |b4: 64""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleByte](yaml))
  }

  test("Char must work (not nullable)") {
    val inst       = SampleChar(Char.MaxValue, 'Z', '\u20A0')
    val yaml       = sj.render(inst)
    val insert     = "\\uffff"
    val comparison = s"""c1: "$insert"
                        |c2: Z
                        |c3: ₠""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst,sj.read[SampleChar](yaml))
  }

  test("Double must work (not nullable)") {
    val inst =
      SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
    val yaml       = sj.render(inst)
    val comparison = """d1: !!float '1.7976931348623157E308'
                        |d2: !!float '-1.7976931348623157E308'
                        |d3: 0.0
                        |d4: -123.4567""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleDouble](yaml))
  }

  test("Enumeration must work (not nullable)") {
    val inst =
      SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
    val yaml       = sj.render(inst)
    val comparison = """e1: Small
                        |e2: Medium
                        |e3: Large
                        |e4: null
                        |e5: Medium""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    // mutate e5 into an ordinal...
    val yaml2 = yaml.asInstanceOf[String].replace("e5: Medium", "e5: 1").asInstanceOf[YAML]
    assertEquals(inst, sj.read[SampleEnum](yaml2))
  }

  test("Enumerations as Ints must work") {
    val sj2 = sj.enumsAsInts()
    val inst =
      SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
    val yaml       = sj2.render(inst)
    val comparison = """e1: 0
                        |e2: 1
                        |e3: 2
                        |e4: null
                        |e5: 1""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj2.read[SampleEnum](yaml))
  }

  test("Float must work") {
    val inst       = SampleFloat(Float.MaxValue, Float.MinValue, 0.0F, -123.4567F)
    val yaml       = sj.render(inst)
    val comparison = """f1: !!float '3.4028235E38'
                        |f2: !!float '-3.4028235E38'
                        |f3: 0.0
                        |f4: -123.4567""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleFloat](yaml))
  }

  test("Int must work (not nullable)") {
    val inst       = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
    val yaml       = sj.render(inst)
    val comparison = """i1: 2147483647
                        |i2: -2147483648
                        |i3: 0
                        |i4: 123""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleInt](yaml))
  }

  test("Long must work (not nullable)") {
    val inst       = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
    val yaml       = sj.render(inst)
    val comparison = """l1: 9223372036854775807
                        |l2: -9223372036854775808
                        |l3: 0
                        |l4: 123""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleLong](yaml))
  }

  test("Short must work (not nullable)") {
    val inst       = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
    val yaml       = sj.render(inst)
    val comparison = """s1: 32767
                        |s2: -32768
                        |s3: 0
                        |s4: 123""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleShort](yaml))
  }

  test("String must work") {
    val inst       = SampleString("something\b\n\f\r\t☆", "", null)
    val yaml       = sj.render(inst)
    val comparison = """s1: "something\b\n\f\r\t☆"
                        |s2: ''
                        |s3: null""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleString](yaml))
  }

  test("UUID must work") {
    val inst = SampleUUID(
      null,
      UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9")
    )
    val yaml       = sj.render(inst)
    val comparison = """u1: null
                        |u2: 580afe0d-81c0-458f-9e09-4486c7af0fe9""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleUUID](yaml))
  }

  test("BigDecimal must break") {
    describe("--- Negative Tests ---")
    val yaml =
      """bd1: 123
        |bd2: 1.23
        |bd3: 0
        |bd4: 123.456
        |bd5: [1,2,3]
        |bd6: null
        |""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 4: Expected a Number value here: +SEQ"){
      sj.read[SampleBigDecimal](yaml)
    }
  }

  test("BigInt must break") {
    val yaml =
      """bi1: [1,2,3]
        |bi2: 90182736451928374653345
        |bi3: 0
        |bi4: null
        |""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 0: Expected a Number value here: +SEQ"){
      sj.read[SampleBigInt](yaml)
    }
  }

  test("Boolean must break") {
    val yaml =
      """bool1: true
        |bool2: 15""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 1: Expected a Boolean value here: =VAL :15"){
      sj.read[SampleBoolean](yaml)
    }
    val yaml2 =
      """bool1: true
        |bool2: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 1: Expected a Boolean value here: =VAL :null"){
      sj.read[SampleBoolean](yaml2)
    }
  }

  test("Byte must break") {
    val yaml =
      """b1: true
        |b2: -128
        |b3: 0
        |b4: 64""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 0: Expected a Number value here: =VAL :true"){
      sj.read[SampleByte](yaml)
    }
    val yaml2 =
      """b1: 12
        |b2: -128
        |b3: 0
        |b4: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 3: Cannot parse an Byte from value"){
      sj.read[SampleByte](yaml2)
    }
  }

  test("Char must break") {
    val yaml =
      """c1: null
        |c2: Y
        |c3: Z""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 0: A Char typed value cannot be null"){
      sj.read[SampleChar](yaml)
    }
    val yaml2 =
      """c1: G
        |c2: 
        |c3: Z""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 1: Tried to read a Char but empty string found"){
      sj.read[SampleChar](yaml2)
    }
    val yaml3 =
      """c1: G
        |c2: 
        |  - a
        |c3: Z""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 2: Expected a String here: +SEQ"){
      sj.read[SampleChar](yaml3)
    }
  }

  test("Double must break") {
    val yaml =
      """d1: 1.79769313486E23157E308
        |d2: -1.7976931348623157E308
        |d3: 0.0
        |d4: -123.4567""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 0: Cannot parse an Double from value"){
      sj.read[SampleDouble](yaml)
    }
  }

  test("Enumeration must break") {
    val yaml =
      """e1: Small
        |e2: Bogus
        |e3: Large
        |e4: null
        |e5: Medium""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]( "Line 1: No value found in enumeration co.blocke.scalajack.yaml.primitives.Size$ for Bogus"){
      sj.read[SampleEnum](yaml)
    }
    val yaml2 =
      """e1: Small
        |e2: Medium
        |e3: Large
        |e4: null
        |e5: 9""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 4: No value found in enumeration co.blocke.scalajack.yaml.primitives.Size$ for 9"){
      sj.read[SampleEnum](yaml2)
    }
    val yaml3 =
      """e1: Small
        |e2: Medium
        |e3: Large
        |e4: null
        |e5: false""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 4: No value found in enumeration co.blocke.scalajack.yaml.primitives.Size$ for false"){
      sj.read[SampleEnum](yaml3) 
    }
  }

  test("Float must break") {
    val yaml =
      """f1: 3.4028235E38
        |f2: [a,b]
        |f3: 0.0
        |f4: -123.4567""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 1: Expected a Number value here: +SEQ"){
      sj.read[SampleFloat](yaml) 
    }
  }

  test("Int must break") {
    val yaml =
      """i1: 2147483647
        |i2: -2147483648
        |i3: [a,b]
        |i4: 123""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]( "Line 2: Expected a Number value here: +SEQ"){
      sj.read[SampleInt](yaml)
    }
    val yaml2 =
      """i1: 2147483647
        |i2: -2147483648
        |i3: 2.3
        |i4: 123""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 2: Cannot parse an Int from value"){
      sj.read[SampleInt](yaml2)
    }
  }

  test("Long must break") {
    val yaml =
      """l1: 9223372036854775807
        |l2: -9223372036854775808
        |l3: true
        |l4: 123""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 2: Expected a Number value here: =VAL :true"){
      sj.read[SampleLong](yaml) 
    }
    val yaml2 =
      """l1: 9223372036854775807
        |l2: -9223372036854775808
        |l3: 0.3
        |l4: 123""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 2: Cannot parse an Long from value"){
      sj.read[SampleLong](yaml2)
    }
  }

  test("Short must break") {
    val yaml =
      """s1: 32767
        |s2: true
        |s3: 0
        |s4: 123""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 1: Expected a Number value here: =VAL :true"){
      sj.read[SampleShort](yaml)
    }
    val yaml2 =
      """s1: 32767
        |s2: 3.4
        |s3: 0
        |s4: 123""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 1: Cannot parse an Short from value"){
      sj.read[SampleShort](yaml2) 
    }
  }

  test("String must break") {
    val yaml =
      """s1: something
        |s2: [a,b]
        |s3: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 1: Expected a String here: +SEQ"){
      sj.read[SampleString](yaml)
    }
  }

  test("UUID must break") {
    val yaml =
      """u1: bogus
        |u2: 580afe0d-81c0-458f-9e09-4486c7af0fe9""".stripMargin.asInstanceOf[YAML]
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Line 0: Failed to create UUID value from parsed text bogus"){
      sj.read[SampleUUID](yaml) 
    }
  }
