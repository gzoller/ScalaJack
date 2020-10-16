package co.blocke.scalajack
package yaml
package mapkeys

import TestUtil._
import munit._
import munit.internal.console

class ScalaPrimKeys() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("With Any Key") {
    describe(
      "------------------------------------------\n:  Scala Primitive Map Key Tests (YAML)  :\n------------------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
    val inst = AnyShell(
      Map(
        List(1, 2, 3)                -> List("a", "b", "c"),
        DogPet("Fido", Food.Meat, 4) -> DogPet("Fifi", Food.Meat, 4),
        Size.Small                   -> "ok",
        123.456                      -> true,
        293845                       -> "Greg",
        false                        -> "16",
        "Fred"                       -> "Wilma",
        16.toByte                    -> null
      )
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  Fred: Wilma
                        |  293845: Greg
                        |  ? _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                        |    name: Fido
                        |    food: Meat
                        |    numLegs: 4
                        |  : _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                        |    name: Fifi
                        |    food: Meat
                        |    numLegs: 4
                        |  16: null
                        |  false: '16'
                        |  Small: ok
                        |  123.456: true
                        |  ? - 1
                        |    - 2
                        |    - 3
                        |  : - a
                        |    - b
                        |    - c
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(yaml, comparison)
    val read = sj.read[AnyShell](yaml).m.keySet.map(z => (z, z.getClass.getName))
    assertEquals(read.contains((16, "java.lang.Integer")), true)
    assertEquals(read.contains((293845, "java.lang.Integer")), true)
    assertEquals(read.contains((123.456, "java.lang.Double")), true)
    assertEquals(read.contains(("Small", "java.lang.String")), true)
    assertEquals(read.contains(("Fred", "java.lang.String")), true)
    assertEquals(read.contains((false, "java.lang.Boolean")), true)
    assertEquals(read.contains(
      (
        DogPet("Fido", Food.Meat, 4),
        "co.blocke.scalajack.yaml.mapkeys.DogPet"
      )
    ), true)
  }

  test("With BigDecimal Key") {
    val inst = SampleBigDecimal(
      Map(
        BigDecimal(123.456) -> BigDecimal(1),
        BigDecimal(789.123) -> BigDecimal(2)
      )
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  123.456: !!float '1'
                        |  789.123: !!float '2'
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleBigDecimal](yaml))
  }

  test("With BigInt Key") {
    val inst =
      SampleBigInt(Map(BigInt(123) -> BigInt(1), BigInt(789) -> BigInt(2)))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  123: 1
                        |  789: 2
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleBigInt](yaml))
  }

  test("With Boolean Key") {
    val inst       = SampleBoolean(Map(true -> false, false -> true))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  true: false
                        |  false: true
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleBoolean](yaml))
  }

  test("With Byte Key") {
    val inst       = SampleByte(Map(16.toByte -> 2.toByte, 48.toByte -> 9.toByte))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  16: 2
                        |  48: 9
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleByte](yaml))
  }

  test("With Char Key") {
    val inst       = SampleChar(Map('a' -> 'A', 'z' -> 'Z'))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  a: A
                        |  z: Z
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleChar](yaml))
  }

  test("With Double Key") {
    val inst       = SampleDouble(Map(12.34 -> 56.78, 90.12 -> 34.56))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12.34: 56.78
                        |  90.12: 34.56
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleDouble](yaml))
  }

  test("With Enumeration Key") {
    val inst = SampleEnumeration(
      Map(Size.Small -> Size.Large, Size.Large -> Size.Medium)
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  Small: Large
                        |  Large: Medium
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleEnumeration](yaml))
  }

  test("With Float Key") {
    val inst       = SampleFloat(Map(12.34F -> 56.78F, 90.12F -> 34.56F))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12.34: 56.78
                        |  90.12: 34.56
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleFloat](yaml))
  }

  test("With Int Key") {
    val inst       = SampleInt(Map(12 -> 56, 90 -> 34))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |  90: 34
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleInt](yaml))
  }

  test("With Long Key") {
    val inst       = SampleLong(Map(12L -> 56L, 90L -> 34L))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |  90: 34
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleLong](yaml))
  }

  test("With Short Key") {
    val inst =
      SampleShort(Map(12.toShort -> 56.toShort, 90.toShort -> 34.toShort))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |  90: 34
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleShort](yaml))
  }

  test("Bad BigDecimal Key") {
    describe("--- Negative Tests ---")
    val yaml =
      """m:
        |  789.123: 1
        |  fred: 2""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :fred"){
      sj.read[SampleBigDecimal](yaml)
    }
  }

  test("Bad BigInt Key") {
    val yaml =
      """m:
        |  fred: 1
        |  789: 2""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Number value here: =VAL :fred"){
      sj.read[SampleBigInt](yaml)
    }
  }

  test("Bad Boolean Key") {
    val yaml =
      """m:
        |  true: false
        |  123: true""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Boolean value here: =VAL :123"){
      sj.read[SampleBoolean](yaml)
    }
  }

  test("Bad Byte Key") {
    val yaml =
      """m:
        |  16: 2
        |  x48: 9""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :x48"){
      sj.read[SampleByte](yaml)
    }
  }

  test("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
    val yaml =
      """m:
        |  null: A
        |  z: Z""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: A Char typed value cannot be null"){
      sj.read[SampleChar](yaml)
    }
  }

  test("Bad Double Key") {
    val yaml =
      """m:
        |  12.34: 56.78
        |  true: 34.56""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :true"){
      sj.read[SampleDouble](yaml)
    }
  }

  test("Bad Enumeration Key") {
    val yaml =
      """m:
        |  Small: Large
        |  Bogus: Medium""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: No value found in enumeration co.blocke.scalajack.yaml.mapkeys.Size$ for Bogus"){
      sj.read[SampleEnumeration](yaml)
    }
  }

  test("Bad Float Key") {
    val yaml =
      """m:
        |  12.34: 56.78
        |  90.12.3: 34.56""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Cannot parse an Float from value"){
      sj.read[SampleFloat](yaml)
    }
  }

  test("Bad Int Key") {
    val yaml =
      """m:
        |  12.0: 56
        |  90: 34""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Cannot parse an Int from value"){
      sj.read[SampleInt](yaml)
    }
  }

  test("Bad Long Key") {
    val yaml =
      """m: 
        |  12: 56
        |  hey: 34""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :hey"){
      sj.read[SampleLong](yaml)
    }
  }

  test("Bad Short Key") {
    val yaml =
      """m: 
        |  p99999: 56
        |  90: 34""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Number value here: =VAL :p99999"){
      sj.read[SampleShort](yaml)
    }
  }