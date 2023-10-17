package co.blocke.scalajack
package json.primitives

import co.blocke.scala_reflection._
import scala.math.BigDecimal
import java.util.UUID
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

class ScalaPrim() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("BigDecimal must work") {
    describe("---------------------------\n:  Scala Primitive Tests  :\n---------------------------", Console.BLUE)
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

    val js = sj.render(inst)
    assertEquals(
      """{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":0.1499999999999999944488848768742172978818416595458984375,"bd6":null}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[SampleBigDecimal](js))
  }

  test("BigInt must work") {
    val inst = SampleBigInt(
      BigInt("-90182736451928374653345"),
      BigInt("90182736451928374653345"),
      BigInt(0),
      null
    )
    val js = sj.render(inst)
    assertEquals(
      """{"bi1":-90182736451928374653345,"bi2":90182736451928374653345,"bi3":0,"bi4":null}""".asInstanceOf[JSON],
      js
    )
    assertEquals(inst, sj.read[SampleBigInt](js))
  }

  test("Binary must work") {
    val inst = SampleBinary(
      null,
      hexStringToByteArray("e04fd020ea3a6910a2d808002b30309d")
    )
    val js = sj.render(inst)
    // assertEquals("""{"b1":null,"b2":"4E/QIOo6aRCi2AgAKzAwnQ=="}""".asInstanceOf[JSON], js)
    val inst2 = sj.read[SampleBinary](js)
    assert(null == inst2.b1)
    assertEquals(inst.b2.toList, inst2.b2.toList)
  }

  test("Boolean must work (not nullable)") {
    val inst = SampleBoolean(bool1 = true, bool2 = false)
    val js = sj.render(inst)
    assertEquals("""{"bool1":true,"bool2":false}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleBoolean](js))
  }

  test("Byte must work (not nullable)") {
    val inst = SampleByte(Byte.MaxValue, Byte.MinValue, 0, 64)
    val js = sj.render(inst)
    assertEquals("""{"b1":127,"b2":-128,"b3":0,"b4":64}""".asInstanceOf[JSON], js )
    assertEquals(inst, sj.read[SampleByte](js))
  }

  test("Char must work (not nullable)") {
    val inst = SampleChar(Char.MaxValue, 'Z', '\u20A0')
    val js = sj.render(inst)
    assertEquals(
      ("""{"c1":"\""" + """uffff","c2":"Z","c3":"\""" + """u20a0"}""").asInstanceOf[JSON], 
      js)
    assertEquals(inst, sj.read[SampleChar](js))
  }

  test("Double must work (not nullable)") {
    val inst =
      SampleDouble(Double.MaxValue, Double.MinValue, 0.0, -123.4567)
    val js = sj.render(inst)
    assertEquals(
      """{"d1":1.7976931348623157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[SampleDouble](js))
  }

  test("Float must work") {
    val inst = SampleFloat(Float.MaxValue, Float.MinValue, 0.0F, -123.4567F)
    val js = sj.render(inst)
    assertEquals(
      """{"f1":3.4028235E38,"f2":-3.4028235E38,"f3":0.0,"f4":-123.4567}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[SampleFloat](js))
  }
  test("Int must work (not nullable)") {
    val inst = SampleInt(Int.MaxValue, Int.MinValue, 0, 123)
    val js = sj.render(inst)
    assertEquals("""{"i1":2147483647,"i2":-2147483648,"i3":0,"i4":123}""".asInstanceOf[JSON], js)
    assertEquals(inst, sj.read[SampleInt](js))
  }
  test("Long must work (not nullable)") {
    val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
    val js = sj.render(inst)
    assertEquals(
      """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[SampleLong](js))
  }
  test("Short must work (not nullable)") {
    val inst = SampleShort(Short.MaxValue, Short.MinValue, 0, 123)
    val js = sj.render(inst)
    assertEquals("""{"s1":32767,"s2":-32768,"s3":0,"s4":123}""".asInstanceOf[JSON], js )
    assertEquals(inst, sj.read[SampleShort](js))
  }
  test("String must work") {
    val inst = SampleString("something\b\n\f\r\t☆", "", null)
    val js = sj.render(inst)
    // The weird '+' here is to break up the unicode so it won't be interpreted and wreck the test.
    assertEquals(
      ("""{"s1":"something\b\n\f\r\t\""" + """u2606","s2":"","s3":null}""").asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[SampleString](js))
  }

  test("UUID must work") {
    val inst = SampleUUID(
      null,
      UUID.fromString("580afe0d-81c0-458f-9e09-4486c7af0fe9")
    )
    val js = sj.render(inst)
    assertEquals(
      """{"u1":null,"u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}""".asInstanceOf[JSON],
      js)
    assertEquals(inst, sj.read[SampleUUID](js))
  }


  //--------------------------------------------------------


  test("BigDecimal must break") {
    describe("--- Negative Tests ---")
    val js =
      """{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.1499999999999999944488848768742172978818416595458984375","bd6":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |{"bd1":123,"bd2":1.23,"bd3":0,"bd4":123.456,"bd5":"0.149999999999999994448884...
              |--------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleBigDecimal](js)
    }
  }

  test("BigInt must break") {
    val js =
      """{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4":null}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |{"bi1":"-90182736451928374653345","bi2":90182736451928374653345,"bi3":0,"bi4"...
              |-------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleBigInt](js)
    }
  }

  test("Boolean must break") {
    val js = """{"bool1":true,"bool2":"false"}""".asInstanceOf[JSON]
    val msg = """Expected a Boolean here
              |{"bool1":true,"bool2":"false"}
              |----------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleBoolean](js)
    }
    val js2 = """{"bool1":true,"bool2":123}""".asInstanceOf[JSON]
    val msg2 = """Expected a Boolean here
                |{"bool1":true,"bool2":123}
                |----------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleBoolean](js2)
    }
    val js3 = """{"bool1":true,"bool2":null}""".asInstanceOf[JSON]
    val msg3 = """Expected a Boolean here
                |{"bool1":true,"bool2":null}
                |----------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg3){
      sj.read[SampleBoolean](js3)
    }
  }

  test("Byte must break") {
    val js = """{"b1":true,"b2":-128,"b3":0,"b4":64}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |{"b1":true,"b2":-128,"b3":0,"b4":64}
              |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleByte](js)
    }
    val js2 = """{"b1":12,"b2":-128,"b3":0,"b4":null}""".asInstanceOf[JSON]
    val msg2 = """Expected a Number here
               |{"b1":12,"b2":-128,"b3":0,"b4":null}
               |-------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleByte](js2)
    }
  }

  test("Char must break") {
    val js = """{"c1":null,"c2":"Y","c3":"Z"}""".asInstanceOf[JSON]
    val msg = """A Char typed value cannot be null
              |{"c1":null,"c2":"Y","c3":"Z"}
              |---------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleChar](js)
    }
    val js2 = """{"c1":"","c2":"Y","c3":"Z"}""".asInstanceOf[JSON]
    val msg2 = """Tried to read a Char but empty string found
               |{"c1":"","c2":"Y","c3":"Z"}
               |-------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleChar](js2)
    }
  }

  test("Double must break") {
    val js =
      """{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123.4567}""".asInstanceOf[JSON]
    val msg =
      """Cannot parse an Double from value
              |{"d1":1.79769313486E23157E308,"d2":-1.7976931348623157E308,"d3":0.0,"d4":-123...
              |----------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleDouble](js)
    }
  }

  test("Float must break") {
    val js =
      """{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |{"f1":3.4028235E38,"f2":"-3.4028235E38","f3":0.0,"f4":-123.4567}
              |------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleFloat](js)
    }
  }

  test("Int must break") {
    val js = """{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |{"i1":2147483647,"i2":-2147483648,"i3":"0","i4":123}
              |---------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleInt](js)
    }
    val js2 = """{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}""".asInstanceOf[JSON]
    val msg2 = """Cannot parse an Int from value
                 |{"i1":2147483647,"i2":-2147483648,"i3":2.3,"i4":123}
                 |-----------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleInt](js2)
    }
  }

  test("Long must break") {
    val js =
      """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}""".asInstanceOf[JSON]
    val msg =
      """Expected a Number here
              |...23372036854775807,"l2":-9223372036854775808,"l3":true,"l4":123}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLong](js)
    }
    val js2 =
      """{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}""".asInstanceOf[JSON]
    val msg2 =
      """Cannot parse an Long from value
        |...372036854775807,"l2":-9223372036854775808,"l3":0.3,"l4":123}
        |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleLong](js2)
    }
  }

  test("Short must break") {
    val js = """{"s1":32767,"s2":true,"s3":0,"s4":123}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |{"s1":32767,"s2":true,"s3":0,"s4":123}
              |-----------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleShort](js)
    }
    val js2 = """{"s1":32767,"s2":3.4,"s3":0,"s4":123}""".asInstanceOf[JSON]
    val msg2 = """Cannot parse an Short from value
                 |{"s1":32767,"s2":3.4,"s3":0,"s4":123}
                 |-------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg2){
      sj.read[SampleShort](js2)
    }
  }

  test("String must break") {
    val js = """{"s1":"something","s2":-19,"s3":null}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{"s1":"something","s2":-19,"s3":null}
              |-----------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleString](js)
    }
  }

  test("UUID must break") {
    val js =
      """{"u1":"bogus","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}""".asInstanceOf[JSON]
    val msg = """Failed to create UUID value from parsed text bogus
              |{"u1":"bogus","u2":"580afe0d-81c0-458f-9e09-4486c7af0fe9"}
              |------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleUUID](js)
    }
  }
