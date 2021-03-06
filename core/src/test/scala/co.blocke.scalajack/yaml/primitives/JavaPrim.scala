package co.blocke.scalajack
package yaml
package primitives

import TestUtil._
import munit._
import munit.internal.console
import java.lang.{Boolean => JBoolean, Byte => JByte, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal, BigInteger => JBigInteger}

class JavaPrim() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("BigDecimal must work") {
    describe(
      "---------------------------------\n:  Java Primitive Tests (YAML)  :\n---------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
    val inst = SampleJBigDecimal(
      JBigDecimal.ZERO,
      JBigDecimal.ONE,
      JBigDecimal.TEN,
      new JBigDecimal(
        "0.1499999999999999944488848768742172978818416595458984375"
      ),
      JBigDecimal.ZERO
    )
    val yaml       = sj.render(inst)
    val comparison = """bd5: !!float '0'
                        |bd1: !!float '0'
                        |bd3: !!float '10'
                        |bd4: 0.1499999999999999944488848768742172978818416595458984375
                        |bd2: !!float '1'""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJBigDecimal](yaml))
  }

  test("BigInteger must work") {
    val inst = SampleJBigInteger(
      JBigInteger.ZERO,
      JBigInteger.ONE,
      JBigInteger.TEN,
      new JBigInteger("-90182736451928374653345"),
      new JBigInteger("90182736451928374653345"),
      new JBigInteger("0"),
      JBigInteger.ZERO
    )
    val yaml       = sj.render(inst)
    val comparison = """bi6: 0
                        |bi2: 1
                        |bi5: 90182736451928374653345
                        |bi1: 0
                        |bi7: 0
                        |bi3: 10
                        |bi4: -90182736451928374653345""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJBigInteger](yaml))
  }

  test("Boolean must work") {
    val inst =
      SampleJBoolean(JBoolean.TRUE, JBoolean.FALSE, true, false, null)
    val yaml       = sj.render(inst)
    val comparison = """bool5: null
                        |bool3: true
                        |bool4: false
                        |bool2: false
                        |bool1: true""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJBoolean](yaml))
  }

  test("Byte must work") {
    val inst = SampleJByte(
      JByte.MAX_VALUE,
      JByte.MIN_VALUE,
      0.asInstanceOf[Byte],
      64.asInstanceOf[Byte],
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """b5: null
                        |b1: 127
                        |b3: 0
                        |b2: -128
                        |b4: 64""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet))
    assertEquals(inst, sj.read[SampleJByte](yaml))
  }

  test("Char must work") {
    val inst       = SampleJChar('Z', '\u20A0', null)
    val yaml       = sj.render(inst)
    val comparison = """c1: Z
                        |c2: ₠
                        |c3: null""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet))
    assertEquals(inst, sj.read[SampleJChar](yaml))
  }

  test("Double must work") {
    val inst = SampleJDouble(
      JDouble.MAX_VALUE,
      JDouble.MIN_VALUE,
      0.0,
      -123.4567,
      null
    )
    val yaml = sj.render(inst)
    val comparison =
      """d5: null
        |d3: 0.0
        |d4: -123.4567
        |d2: !!float '4.9E-324'
        |d1: !!float '1.7976931348623157E308'""".stripMargin
    assertEquals(Set.empty[String], yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet))
    assertEquals(inst, sj.read[SampleJDouble](yaml))
  }

  test("Float must work") {
    val inst = SampleJFloat(
      JFloat.MAX_VALUE,
      JFloat.MIN_VALUE,
      0.0F,
      -123.4567F,
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """f4: -123.4567
                        |f5: null
                        |f3: 0.0
                        |f2: !!float '1.4E-45'
                        |f1: !!float '3.4028235E38'""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJFloat](yaml))
  }

  test("Int must work") {
    val inst       = SampleJInt(JInt.MAX_VALUE, JInt.MIN_VALUE, 0, 123, null)
    val yaml       = sj.render(inst)
    val comparison = """i2: -2147483648
                        |i4: 123
                        |i3: 0
                        |i1: 2147483647
                        |i5: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJInt](yaml))
  }

  test("Long must work") {
    val inst       = SampleJLong(JLong.MAX_VALUE, JLong.MIN_VALUE, 0L, 123L, null)
    val yaml       = sj.render(inst)
    val comparison = """l2: -9223372036854775808
                        |l1: 9223372036854775807
                        |l4: 123
                        |l3: 0
                        |l5: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJLong](yaml))
  }

  test("Number must work") {
    val inst = SampleJNumber(
      JByte.valueOf("-128"),
      JByte.valueOf("127"),
      JShort.valueOf("-32768"),
      JShort.valueOf("32767"),
      JInt.valueOf("-2147483648"),
      JInt.valueOf("2147483647"),
      JLong.valueOf("-9223372036854775808"),
      JLong.valueOf("9223372036854755807"),
      null, //new JBigInteger("9923372036854755810"),
      JByte.valueOf("0"),
      JFloat.valueOf("3.4e-038"),
      JFloat.valueOf("3.4e+038"),
      JDouble.valueOf("1.7e-308"),
      JDouble.valueOf("1.7e+308"),
      null, //new JBigDecimal("1.8e+308"),
      JFloat.valueOf("0.0"),
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """n10: 0
                        |n4: 32767
                        |n8: 9223372036854755807
                        |n16: 0.0
                        |n1: -128
                        |n3: -32768
                        |n15: null
                        |n14: !!float '1.7E308'
                        |n12: !!float '3.4E38'
                        |n13: !!float '1.7E-308'
                        |n6: 2147483647
                        |n5: -2147483648
                        |n9: null
                        |n7: -9223372036854775808
                        |n11: !!float '3.4E-38'
                        |n2: 127
                        |n17: null""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJNumber](yaml))
  }

  test("Short must work") {
    val inst = SampleJShort(
      JShort.MAX_VALUE,
      JShort.MIN_VALUE,
      0.asInstanceOf[Short],
      123.asInstanceOf[Short],
      null
    )
    val yaml       = sj.render(inst)
    val comparison = """s4: 123
                        |s1: 32767
                        |s5: null
                        |s2: -32768
                        |s3: 0""".stripMargin
    assertEquals(Set.empty[String],  yaml.asInstanceOf[String].split("\n").toSet.diff(comparison.split("\n").toSet) )
    assertEquals(inst, sj.read[SampleJShort](yaml))
  }

  test("BigDecimal must break") {
    describe("--- Negative Tests ---")
    val yaml =
      """bd1: 0
        |bd2: 1
        |bd3: 10
        |bd4: [a,b,c]
        |bd5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 3: Expected a Number value here: +SEQ"){
      sj.read[SampleBigDecimal](yaml)
    }
  }

  test("BigInteger must break") {
    val yaml =
      """bi1: [a,b]
        |bi2: 1
        |bi3: 10
        |bi4: -90182736451928374653345
        |bi5: 90182736451928374653345
        |bi6: 0
        |bi7: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Expected a Number value here: +SEQ"){
      sj.read[SampleJBigInteger](yaml)
    }
  }

  test("Boolean must break") {
    val yaml =
      """bool1: true
        |bool2: false
        |bool3: true
        |bool4: [a,b]
        |bool5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 3: Expected a Boolean value here: +SEQ"){
      sj.read[SampleJBoolean](yaml)
    }
  }

  test("Byte must break") {
    val yaml =
      """b1: 127
        |b2: -128
        |b3: false
        |b4: 64
        |b5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :false"){
      sj.read[SampleJByte](yaml)
    }
  }

  test("Char must break") {
    val yaml =
      """c1: "Z"
        |c2: [a,b]
        |c3: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a String here: +SEQ"){
      sj.read[SampleJChar](yaml)
    }
    val yaml2 =
      """c1: "Z"
        |c2:
        |c3: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Tried to read a Character but empty string found"){
      sj.read[SampleJChar](yaml2)
    }
  }

  test("Double must break") {
    val yaml =
      """d1: 1.7976931348623157E308
        |d2: 4.9E-324
        |d3: fred
        |d4: -123.4567
        |d5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :fred"){
      sj.read[SampleJDouble](yaml)
    }
  }

  test("Float must break") {
    val yaml =
      """f1: 3.4028235E38
        |f2: fred
        |f3: 0.0
        |f4: -123.4567
        |f5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Number value here: =VAL :fred"){
      sj.read[SampleJFloat](yaml)
    }
  }

  test("Int must break") {
    val yaml =
      """i1: 2147483647
        |i2: -2147483648
        |i3: false
        |i4: 123
        |i5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :false"){
      sj.read[SampleJInt](yaml)
    }
    val yaml2 =
      """i1: 2147483647
        |i2: -2147483648
        |i3: 0.3
        |i4: 123
        |i5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"0.3\""){
      sj.read[SampleJInt](yaml2)
    }
  }

  test("Long must break") {
    val yaml =
      """l1: 9223372036854775807
        |l2: -9223372036854775808
        |l3: [a,b]
        |l4: 123
        |l5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: +SEQ"){
      sj.read[SampleJLong](yaml)
    }
    val yaml2 =
      """l1: 9223372036854775807
        |l2: -9223372036854775808
        |l3: 0.3
        |l4: 123
        |l5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"0.3\""){
      sj.read[SampleJLong](yaml2)
    }
  }

  test("Number must break") {
    val yaml =
      """n1: -128
        |n2: 127
        |n3: [a,b]
        |n4: 32767
        |n5: -2147483648
        |n6: 2147483647
        |n7: -9223372036854775808
        |n8: 9223372036854755807
        |n9: 9923372036854755810
        |n10: 0
        |n11: 3.4E-38
        |n12: 3.4E38
        |n13: 1.7E-308
        |n14: 1.7E308
        |n15: 1.8E+308
        |n16: 0.0
        |n17: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: +SEQ"){
      sj.read[SampleJNumber](yaml)
    }
  }

  test("Short must break") {
    val yaml =
      """s1: false
        |s2: -32768
        |s3: 0
        |s4: 123
        |s5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 0: Expected a Number value here: =VAL :false"){
      sj.read[SampleJShort](yaml)
    }
    val yaml2 =
      """s1: 2.3
        |s2: -32768
        |s3: 0
        |s4: 123
        |s5: null""".stripMargin.asInstanceOf[YAML]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"2.3\""){
      sj.read[SampleJShort](yaml2)
    }
  }
