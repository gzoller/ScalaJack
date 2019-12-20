package co.blocke.scalajack
package yaml
package primitives

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import scala.math.BigDecimal
import java.util.UUID
import TestUtil._

class ScalaPrim() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "----------------------------------\n:  Scala Primitive Tests (YAML)  :\n----------------------------------"
  ) {
    describe("+++ Positive Tests +++") {
      it("BigDecimal must work") {
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
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleBigDecimal](yaml)
        }
      }
      it("BigInt must work") {
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
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleBigInt](yaml)
        }
      }
      it("Binary must work") {
        val inst = SampleBinary(
          null,
          hexStringToByteArray("e04fd020ea3a6910a2d808002b30309d")
        )
        val yaml = sj.render(inst)
        val comparison =
          """b1: null
            |b2: 4E/QIOo6aRCi2AgAKzAwnQ==""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        val inst2 = sj.read[SampleBinary](yaml)
        assertResult(null) { inst2.b1 }
        assertResult(true) {
          inst.b2.toList == inst2.b2.toList
        }
      }
      it("Boolean must work (not nullable)") {
        val inst = SampleBoolean(true, false)
        val yaml = sj.render(inst)
        val comparison =
          """bool1: true
            |bool2: false""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleBoolean](yaml)
        }
      }
      it("Byte must work (not nullable)") {
        val inst       = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
        val yaml       = sj.render(inst)
        val comparison = """b1: 127
                           |b2: -128
                           |b3: 0
                           |b4: 64""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleByte](yaml)
        }
      }
      it("Char must work (not nullable)") {
        val inst       = SampleChar(Char.MaxValue, 'Z', '\u20A0')
        val yaml       = sj.render(inst)
        val insert     = "\\uffff"
        val comparison = s"""c1: "$insert"
                            |c2: Z
                            |c3: ₠""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleChar](yaml)
        }
      }
      it("Double must work (not nullable)") {
        val inst =
          SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
        val yaml       = sj.render(inst)
        val comparison = """d1: 1.7976931348623157E308
                           |d2: -1.7976931348623157E308
                           |d3: 0.0
                           |d4: -123.4567""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleDouble](yaml)
        }
      }
      it("Enumeration must work (not nullable)") {
        val inst =
          SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
        val yaml       = sj.render(inst)
        val comparison = """e1: Small
                           |e2: Medium
                           |e3: Large
                           |e4: null
                           |e5: Medium""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        // mutate e5 into an ordinal...
        val yaml2 = yaml.replace("e5: Medium", "e5: 1")
        assertResult(inst) {
          sj.read[SampleEnum](yaml2)
        }
      }
      it("Enumerations as Ints must work") {
        val sj2 = sj.enumsAsInts()
        val inst =
          SampleEnum(Size.Small, Size.Medium, Size.Large, null, Size.Medium)
        val yaml       = sj2.render(inst)
        val comparison = """e1: 0
                           |e2: 1
                           |e3: 2
                           |e4: null
                           |e5: 1""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj2.read[SampleEnum](yaml)
        }
      }
      it("Float must work") {
        val inst       = SampleFloat(Float.MaxValue, Float.MinValue, 0.0F, -123.4567F)
        val yaml       = sj.render(inst)
        val comparison = """f1: 3.4028235E38
                           |f2: -3.4028235E38
                           |f3: 0.0
                           |f4: -123.4567""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleFloat](yaml)
        }
      }
      it("Int must work (not nullable)") {
        val inst       = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
        val yaml       = sj.render(inst)
        val comparison = """i1: 2147483647
                           |i2: -2147483648
                           |i3: 0
                           |i4: 123""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleInt](yaml)
        }
      }
      it("Long must work (not nullable)") {
        val inst       = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val yaml       = sj.render(inst)
        val comparison = """l1: 9223372036854775807
                           |l2: -9223372036854775808
                           |l3: 0
                           |l4: 123""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleLong](yaml)
        }
      }
      it("Short must work (not nullable)") {
        val inst       = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
        val yaml       = sj.render(inst)
        val comparison = """s1: 32767
                           |s2: -32768
                           |s3: 0
                           |s4: 123""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleShort](yaml)
        }
      }
      it("String must work") {
        val inst       = SampleString("something\b\n\f\r\t☆", "", null)
        val yaml       = sj.render(inst)
        val comparison = """s1: "something\b\n\f\r\t☆"
                           |s2: ''
                           |s3: null""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleString](yaml)
        }
      }
      it("UUID must work") {
        val inst = SampleUUID(
          null,
          UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9")
        )
        val yaml       = sj.render(inst)
        val comparison = """u1: null
                           |u2: 580afe0d-81c0-458f-9e09-4486c7af0fe9""".stripMargin
        assertResult(Set.empty[String]) { yaml.split("\n").toSet.diff(comparison.split("\n").toSet) }
        assertResult(inst) {
          sj.read[SampleUUID](yaml)
        }
      }
    }
    describe("--- Negative Tests ---") {
      it("BigDecimal must break") {
        val yaml =
          """bd1: 123
            |bd2: 1.23
            |bd3: 0
            |bd4: 123.456
            |bd5: [1,2,3]
            |bd6: null
            |""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleBigDecimal](yaml) should have message "Line 4: Expected a Number value here: +SEQ"
      }
      it("BigInt must break") {
        val yaml =
          """bi1: [1,2,3]
            |bi2: 90182736451928374653345
            |bi3: 0
            |bi4: null
            |""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleBigInt](yaml) should have message "Line 0: Expected a Number value here: +SEQ"
      }
      it("Boolean must break") {
        val yaml =
          """bool1: true
            |bool2: 15""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleBoolean](yaml) should have message "Line 1: Expected a Boolean value here: =VAL :15"
        val yaml2 =
          """bool1: true
            |bool2: null""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleBoolean](yaml2) should have message "Line 1: Expected a Boolean value here: =VAL :null"
      }
      it("Byte must break") {
        val yaml =
          """b1: true
            |b2: -128
            |b3: 0
            |b4: 64""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleByte](yaml) should have message "Line 0: Expected a Number value here: =VAL :true"
        val yaml2 =
          """b1: 12
            |b2: -128
            |b3: 0
            |b4: null""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleByte](yaml2) should have message "Line 3: A Byte typed value cannot be null"
      }
      it("Char must break") {
        val yaml =
          """c1: null
            |c2: Y
            |c3: Z""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleChar](yaml) should have message "Line 0: A Char typed value cannot be null"
        val yaml2 =
          """c1: G
            |c2: 
            |c3: Z""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleChar](yaml2) should have message "Line 1: Tried to read a Char but empty string found"
        val yaml3 =
          """c1: G
            |c2: 
            |  - a
            |c3: Z""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleChar](yaml3) should have message "Line 2: Expected a String here: +SEQ"
      }
      it("Double must break") {
        val yaml =
          """d1: 1.79769313486E23157E308
            |d2: -1.7976931348623157E308
            |d3: 0.0
            |d4: -123.4567""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleDouble](yaml) should have message "Line 0: Cannot parse an Double from value"
      }
      it("Enumeration must break") {
        val yaml =
          """e1: Small
            |e2: Bogus
            |e3: Large
            |e4: null
            |e5: Medium""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleEnum](yaml) should have message "Line 1: No value found in enumeration co.blocke.scalajack.yaml.primitives.Size$ for Bogus"
        val yaml2 =
          """e1: Small
            |e2: Medium
            |e3: Large
            |e4: null
            |e5: 9""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleEnum](yaml2) should have message "Line 4: No value found in enumeration co.blocke.scalajack.yaml.primitives.Size$ for 9"
        val yaml3 =
          """e1: Small
            |e2: Medium
            |e3: Large
            |e4: null
            |e5: false""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleEnum](yaml3) should have message "Line 4: No value found in enumeration co.blocke.scalajack.yaml.primitives.Size$ for false"
      }
      it("Float must break") {
        val yaml =
          """f1: 3.4028235E38
            |f2: [a,b]
            |f3: 0.0
            |f4: -123.4567""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleFloat](yaml) should have message "Line 1: Expected a Number value here: +SEQ"
      }
      it("Int must break") {
        val yaml =
          """i1: 2147483647
            |i2: -2147483648
            |i3: [a,b]
            |i4: 123""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleInt](yaml) should have message "Line 2: Expected a Number value here: +SEQ"
        val yaml2 =
          """i1: 2147483647
            |i2: -2147483648
            |i3: 2.3
            |i4: 123""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleInt](yaml2) should have message "Line 2: Cannot parse an Int from value"
      }
      it("Long must break") {
        val yaml =
          """l1: 9223372036854775807
            |l2: -9223372036854775808
            |l3: true
            |l4: 123""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleLong](yaml) should have message "Line 2: Expected a Number value here: =VAL :true"
        val yaml2 =
          """l1: 9223372036854775807
            |l2: -9223372036854775808
            |l3: 0.3
            |l4: 123""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleLong](yaml2) should have message "Line 2: Cannot parse an Long from value"
      }
      it("Short must break") {
        val yaml =
          """s1: 32767
            |s2: true
            |s3: 0
            |s4: 123""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleShort](yaml) should have message "Line 1: Expected a Number value here: =VAL :true"
        val yaml2 =
          """s1: 32767
            |s2: 3.4
            |s3: 0
            |s4: 123""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleShort](yaml2) should have message "Line 1: Cannot parse an Short from value"
      }
      it("String must break") {
        val yaml =
          """s1: something
            |s2: [a,b]
            |s3: null""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleString](yaml) should have message "Line 1: Expected a String here: +SEQ"
      }
      it("UUID must break") {
        val yaml =
          """u1: bogus
            |u2: 580afe0d-81c0-458f-9e09-4486c7af0fe9""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleUUID](yaml) should have message "Line 0: Failed to create UUID value from parsed text bogus"
      }
    }
  }
}
