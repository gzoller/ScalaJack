package co.blocke.scalajack
package yaml
package mapkeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ScalaPrimKeys() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "------------------------------------------\n:  Scala Primitive Map Key Tests (YAML)  :\n------------------------------------------"
  ) {
    describe("+++ Positive Tests +++") {
      it("With Any Key") {
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
                           |""".stripMargin
        yaml should be(comparison)
        val read =
          sj.read[AnyShell](yaml).m.keySet.map(z => (z, z.getClass.getName))
        read.contains((16, "java.lang.Integer")) should be(true)
        read.contains((293845, "java.lang.Integer")) should be(true)
        read.contains((123.456, "java.lang.Double")) should be(true)
        read.contains(("Small", "java.lang.String")) should be(true)
        read.contains(("Fred", "java.lang.String")) should be(true)
        read.contains((false, "java.lang.Boolean")) should be(true)
        read.contains(
          (
            DogPet("Fido", Food.Meat, 4),
            "co.blocke.scalajack.yaml.mapkeys.DogPet"
          )
        ) should be(true)
      }
      it("With BigDecimal Key") {
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleBigDecimal](yaml)
        }
      }
      it("With BigInt Key") {
        val inst =
          SampleBigInt(Map(BigInt(123) -> BigInt(1), BigInt(789) -> BigInt(2)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  123: 1
                           |  789: 2
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleBigInt](yaml)
        }
      }
      it("With Boolean Key") {
        val inst       = SampleBoolean(Map(true -> false, false -> true))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  true: false
                           |  false: true
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleBoolean](yaml)
        }
      }
      it("With Byte Key") {
        val inst       = SampleByte(Map(16.toByte -> 2.toByte, 48.toByte -> 9.toByte))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  16: 2
                           |  48: 9
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleByte](yaml)
        }
      }
      it("With Char Key") {
        val inst       = SampleChar(Map('a' -> 'A', 'z' -> 'Z'))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  a: A
                           |  z: Z
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleChar](yaml)
        }
      }
      it("With Double Key") {
        val inst       = SampleDouble(Map(12.34 -> 56.78, 90.12 -> 34.56))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12.34: 56.78
                           |  90.12: 34.56
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleDouble](yaml)
        }
      }
      it("With Enumeration Key") {
        val inst = SampleEnumeration(
          Map(Size.Small -> Size.Large, Size.Large -> Size.Medium)
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  Small: Large
                           |  Large: Medium
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleEnumeration](yaml)
        }
      }
      it("With Float Key") {
        val inst       = SampleFloat(Map(12.34F -> 56.78F, 90.12F -> 34.56F))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12.34: 56.78
                           |  90.12: 34.56
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleFloat](yaml)
        }
      }
      it("With Int Key") {
        val inst       = SampleInt(Map(12 -> 56, 90 -> 34))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |  90: 34
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleInt](yaml)
        }
      }
      it("With Long Key") {
        val inst       = SampleLong(Map(12L -> 56L, 90L -> 34L))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |  90: 34
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleLong](yaml)
        }
      }
      it("With Short Key") {
        val inst =
          SampleShort(Map(12.toShort -> 56.toShort, 90.toShort -> 34.toShort))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |  90: 34
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleShort](yaml)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("Bad BigDecimal Key") {
        val yaml =
          """m:
            |  789.123: 1
            |  fred: 2""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleBigDecimal](yaml) should have message "Line 2: Expected a Number value here: =VAL :fred"
      }
      it("Bad BigInt Key") {
        val yaml =
          """m:
            |  fred: 1
            |  789: 2""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleBigInt](yaml) should have message "Line 1: Expected a Number value here: =VAL :fred"
      }
      it("Bad Boolean Key") {
        val yaml =
          """m:
            |  true: false
            |  123: true""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleBoolean](yaml) should have message "Line 2: Expected a Boolean value here: =VAL :123"
      }
      it("Bad Byte Key") {
        val yaml =
          """m:
            |  16: 2
            |  x48: 9""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleByte](yaml) should have message "Line 2: Expected a Number value here: =VAL :x48"
      }
      it("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
        val yaml =
          """m:
            |  null: A
            |  z: Z""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleChar](yaml) should have message "Line 1: A Char typed value cannot be null"
      }
      it("Bad Double Key") {
        val yaml =
          """m:
            |  12.34: 56.78
            |  true: 34.56""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleDouble](yaml) should have message "Line 2: Expected a Number value here: =VAL :true"
      }
      it("Bad Enumeration Key") {
        val yaml =
          """m:
            |  Small: Large
            |  Bogus: Medium""".stripMargin
        the[ScalaJackError] thrownBy sj
          .read[SampleEnumeration](yaml) should have message "Line 2: No value found in enumeration co.blocke.scalajack.yaml.mapkeys.Size$ for Bogus"
      }
      it("Bad Float Key") {
        val yaml =
          """m:
            |  12.34: 56.78
            |  90.12.3: 34.56""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleFloat](yaml) should have message "Line 2: Cannot parse an Float from value"
      }
      it("Bad Int Key") {
        val yaml =
          """m:
            |  12.0: 56
            |  90: 34""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleInt](yaml) should have message "Line 1: Cannot parse an Int from value"
      }
      it("Bad Long Key") {
        val yaml =
          """m: 
            |  12: 56
            |  hey: 34""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleLong](yaml) should have message "Line 2: Expected a Number value here: =VAL :hey"
      }
      it("Bad Short Key") {
        val yaml =
          """m: 
            |  p99999: 56
            |  90: 34""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleShort](yaml) should have message "Line 1: Expected a Number value here: =VAL :p99999"
      }
    }
  }
}
