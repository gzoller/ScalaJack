package co.blocke.scalajack
package json
package primitives

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

import java.lang.{Boolean as JBoolean, Byte as JByte, Double as JDouble, Float as JFloat, Integer as JInt, Long as JLong, Short as JShort}
import java.math.{BigDecimal as JBigDecimal, BigInteger as JBigInteger}

class JavaPrimSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("--------------------------\n:  Java Primitive Tests  :\n--------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("BigDecimal must work") {
        val inst = SampleJBigDecimal(
          JBigDecimal.ZERO,
          JBigDecimal.ONE,
          JBigDecimal.TEN,
          new JBigDecimal(
            "0.1499999999999999944488848768742172978818416595458984375"
          ),
          null
        )
        val js = sj[SampleJBigDecimal].toJson(inst)
        js should matchJson("""{"bd1":0,"bd2":1,"bd3":10,"bd4":0.1499999999999999944488848768742172978818416595458984375,"bd5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJBigDecimal](js)
      }

      it("BigInteger must work") {
        val inst = SampleJBigInteger(
          JBigInteger.ZERO,
          JBigInteger.ONE,
          JBigInteger.TEN,
          new JBigInteger("-90182736451928374653345"),
          new JBigInteger("90182736451928374653345"),
          new JBigInteger("0"),
          null
        )
        val js = sj[SampleJBigInteger].toJson(inst)
        js should matchJson("""{"bi1":0,"bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":90182736451928374653345,"bi6":0,"bi7":null}""")
        // inst shouldEqual ScalaJack.read[SampleJBigInteger](js)
      }

      it("Boolean must work") {
        val inst = SampleJBoolean(JBoolean.TRUE, JBoolean.FALSE, true, false, null)
        val js = sj[SampleJBoolean].toJson(inst)
        js should matchJson("""{"bool1":true,"bool2":false,"bool3":true,"bool4":false,"bool5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJBoolean](js)
      }

      it("Byte must work") {
        val inst = SampleJByte(
          JByte.MAX_VALUE,
          JByte.MIN_VALUE,
          0.asInstanceOf[Byte],
          64.asInstanceOf[Byte],
          null
        )
        val js = sj[SampleJByte].toJson(inst)
        js should matchJson("""{"b1":127,"b2":-128,"b3":0,"b4":64,"b5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJByte](js)
      }

      it("Character must work") {
        val inst = SampleJChar('Z', '\u20A0', null)
        val js = sj[SampleJChar].toJson(inst)
        js should matchJson("""{"c1":"Z","c2":"\""" + """u20a0","c3":null}""")
        // inst shouldEqual ScalaJack.read[SampleJChar](js)
      }

      it("Double must work") {
        val inst = SampleJDouble(
          JDouble.MAX_VALUE,
          JDouble.MIN_VALUE,
          0.0,
          -123.4567,
          null
        )
        val js = sj[SampleJDouble].toJson(inst)
        js should matchJson("""{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":0.0,"d4":-123.4567,"d5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJDouble](js)
      }

      it("Float must work") {
        val inst = SampleJFloat(
          JFloat.MAX_VALUE,
          JFloat.MIN_VALUE,
          0.0f,
          -123.4567f,
          null
        )
        val js = sj[SampleJFloat].toJson(inst)
        js should matchJson("""{"f1":3.4028235E38,"f2":1.4E-45,"f3":0.0,"f4":-123.4567,"f5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJFloat](js)
      }

      it("Integer must work") {
        val inst = SampleJInt(JInt.MAX_VALUE, JInt.MIN_VALUE, 0, 123, null)
        val js = sj[SampleJInt].toJson(inst)
        js should matchJson("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123,"i5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJInt](js)
      }

      it("Long must work") {
        val inst = SampleJLong(JLong.MAX_VALUE, JLong.MIN_VALUE, 0L, 123L, null)
        val js = sj[SampleJLong].toJson(inst)
        js should matchJson("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123,"l5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJLong](js)
      }

      it("Number must work") {
        val inst = SampleJNumber(
          JByte.valueOf("-128"),
          JByte.valueOf("127"),
          JShort.valueOf("-32768"),
          JShort.valueOf("32767"),
          JInt.valueOf("-2147483648"),
          JInt.valueOf("2147483647"),
          JLong.valueOf("-9223372036854775808"),
          JLong.valueOf("9223372036854755807"),
          null, // new JBigInteger("9923372036854755810"),
          JByte.valueOf("0"),
          JFloat.valueOf("3.4e-038"),
          JFloat.valueOf("3.4e+038"),
          JDouble.valueOf("1.7e-308"),
          JDouble.valueOf("1.7e+308"),
          null, // new JBigDecimal("1.8e+308"),
          JFloat.valueOf("0.0"),
          null
        )
        val js = sj[SampleJNumber].toJson(inst)
        js should matchJson(
          """{"n1":-128,"n2":127,"n3":-32768,"n4":32767,"n5":-2147483648,"n6":2147483647,"n7":-9223372036854775808,"n8":9223372036854755807,"n9":null,"n10":0,"n11":3.4E-38,"n12":3.4E38,"n13":1.7E-308,"n14":1.7E308,"n15":null,"n16":0.0,"n17":null}"""
        )
        // inst shouldEqual ScalaJack.read[SampleJNumber](js)
      }

      it("Short must work") {
        val inst = SampleJShort(
          JShort.MAX_VALUE,
          JShort.MIN_VALUE,
          0.asInstanceOf[Short],
          123.asInstanceOf[Short],
          null
        )
        val js = sj[SampleJShort].toJson(inst)
        js should matchJson("""{"s1":32767,"s2":-32768,"s3":0,"s4":123,"s5":null}""")
        // inst shouldEqual ScalaJack.read[SampleJShort](js)
      }
    }
  }

/*

  //--------------------------------------------------------


  test("BigDecimal must break") {
    describe("--- Negative Tests ---")
    val js =
      """{"bd1":0,"bd2":1,"bd3":10,"bd4":"0.1499999999999999944488848768742172978818416595458984375","bd5":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
        |{"bd1":0,"bd2":1,"bd3":10,"bd4":"0.149999999999999994448884876874217297881841...
        |--------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJBigDecimal](js)
    }
  }

  test("BigInt must break") {
    val js =
      """{"bi1":"0","bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":90182736451928374653345,"bi6":0,"bi7":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
        |{"bi1":"0","bi2":1,"bi3":10,"bi4":-90182736451928374653345,"bi5":901827364519...
        |-------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJBigInteger](js)
    }
  }

  test("Boolean must break") {
    val js = """{"bool1":true,"bool2":false,"bool3":true,"bool4":"false","bool5":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Boolean here
              |{"bool1":true,"bool2":false,"bool3":true,"bool4":"false","bool5":null}
              |-------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJBoolean](js)
    }
  }

  test("Byte must break") {
    val js = """{"b1":127,"b2":-128,"b3":false,"b4":64,"b5":null}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
      |{"b1":127,"b2":-128,"b3":false,"b4":64,"b5":null}
      |-------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJByte](js)
    }
  }

  test("Char must break") {
    val js = """{"c1":"Z","c2":3,"c3":null}""".asInstanceOf[JSON]
    val msg = """Expected a String here
      |{"c1":"Z","c2":3,"c3":null}
      |---------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJChar](js)
    }
    val js2 = """{"c1":"Z","c2":"","c3":null}""".asInstanceOf[JSON]
    val msg2 = """Tried to read a Character but empty string found
      |{"c1":"Z","c2":"","c3":null}
      |----------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleJChar](js2)
    }
  }

  test("Double must break") {
    val js =
      """{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":"0.0","d4":-123.4567,"d5":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |{"d1":1.7976931348623157E308,"d2":4.9E-324,"d3":"0.0","d4":-123.4567,"d5":null}
              |------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJDouble](js)
    }
  }

  test("Float must break") {
    val js =
      """{"f1":3.4028235E38,"f2":"1.4E-45","f3":0.0,"f4":-123.4567,"f5":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |{"f1":3.4028235E38,"f2":"1.4E-45","f3":0.0,"f4":-123.4567,"f5":null}
              |------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJFloat](js)
    }
  }

  test("Int must break") {
    val js = """{"i1":2147483647,"i2":-2147483648,"i3":false,"i4":123,"i5":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |{"i1":2147483647,"i2":-2147483648,"i3":false,"i4":123,"i5":null}
              |---------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJInt](js)
    }
    val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":0.3,"i4":123,"i5":null}""".asInstanceOf[JSON]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"0.3\""){
      sj.read[SampleJInt](js2)
    }
  }

  test("Long must break") {
    val js =
      """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":"0","l4":123,"l5":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |...23372036854775807,"l2":-9223372036854775808,"l3":"0","l4":123,"l5":null}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJLong](js)
    }
    val js2 =
      """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123,"l5":null}""".asInstanceOf[JSON]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"0.3\""){
      sj.read[SampleJLong](js2)
    }
  }

  test("Number must break") {
    val js = """{"n1":-128,"n2":127,"n3":"-32768","n4":32767,"n5":-2147483648,"n6":2147483647,"n7":-9223372036854775808,"n8":9223372036854755807,"n9":9923372036854755810,"n10":0,"n11":3.4E-38,"n12":3.4E38,"n13":1.7E-308,"n14":1.7E308,"n15":1.8E+308,"n16":0.0,"n17":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |{"n1":-128,"n2":127,"n3":"-32768","n4":32767,"n5":-2147483648,"n6":2147483647...
              |-------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJNumber](js)
    }
  }

  test("Short must break") {
    val js = """{"s1":false,"s2":-32768,"s3":0,"s4":123,"s5":null}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
      |{"s1":false,"s2":-32768,"s3":0,"s4":123,"s5":null}
      |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleJShort](js)
    }
    val js2 = """{"s1":2.3,"s2":-32768,"s3":0,"s4":123,"s5":null}""".asInstanceOf[JSON]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"2.3\""){
      sj.read[SampleJShort](js2)
    }
  }
 */
