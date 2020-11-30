package co.blocke.scalajack
package json.mapkeys

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON

class ScalaPrimKeys() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("With Any Key") {
    describe(
      "-----------------------------------\n:  Scala Primitive Map Key Tests  :\n-----------------------------------", Console.BLUE
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
    val js = sj.render(inst)
    assert( json.JsonMatcher.jsonMatches(js,
      """{"m":{"false":"16","Small":"ok","123.456":true,"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}":{"_hint":"co.blocke.scalajack.json.mapkeys.DogPet","name":"Fifi","food":"Meat","numLegs":4},"Fred":"Wilma","[1,2,3]":["a","b","c"],"293845":"Greg","16":null}}""".asInstanceOf[JSON]
    ) )
    val read = sj.read[AnyShell](js).m.keySet.map(z => (z, z.getClass.getName))
    assert(read.contains((16, "java.lang.Integer")))
    assert(read.contains((293845, "java.lang.Integer")))
    assert(read.contains((123.456, "java.lang.Double")))
    assert(read.contains(("Small", "java.lang.String")))
    assert(read.contains(("Fred", "java.lang.String")))
    assert(read.contains((false, "java.lang.Boolean")))
    assert(read.contains(
      (
        DogPet("Fido", Food.Meat, 4),
        "co.blocke.scalajack.json.mapkeys.DogPet"
      )
    ))
  }

  test("With BigDecimal Key") {
    val inst = SampleBigDecimal(
      Map(
        BigDecimal(123.456) -> BigDecimal(1),
        BigDecimal(789.123) -> BigDecimal(2)
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"123.456":1,"789.123":2}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleBigDecimal](js))
  }

  test("With BigInt Key") {
    val inst =
      SampleBigInt(Map(BigInt(123) -> BigInt(1), BigInt(789) -> BigInt(2)))
    val js = sj.render(inst)
    assertEquals("""{"m":{"123":1,"789":2}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleBigInt](js))
  }

  test("With Boolean Key") {
    val inst = SampleBoolean(Map(true -> false, false -> true))
    val js   = sj.render(inst)
    assertEquals("""{"m":{"true":false,"false":true}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleBoolean](js))
  }

  test("With Byte Key") {
    val inst = SampleByte(Map(16.toByte -> 2.toByte, 48.toByte -> 9.toByte))
    val js   = sj.render(inst)
    assertEquals("""{"m":{"16":2,"48":9}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleByte](js))
  }

  test("With Char Key") {
    val inst = SampleChar(Map('a' -> 'A', 'z' -> 'Z'))
    val js   = sj.render(inst)
    assertEquals("""{"m":{"a":"A","z":"Z"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleChar](js))
  }

  test("With Double Key") {
    val inst = SampleDouble(Map(12.34 -> 56.78, 90.12 -> 34.56))
    val js   = sj.render(inst)
    assertEquals("""{"m":{"12.34":56.78,"90.12":34.56}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleDouble](js))
  }

  test("With Enumeration Key") {
    val inst = SampleEnumeration(
      Map(Size.Small -> Size.Large, Size.Large -> Size.Medium)
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"Small":"Large","Large":"Medium"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleEnumeration](js))
  }

  test("With Float Key") {
    val inst = SampleFloat(Map(12.34F -> 56.78F, 90.12F -> 34.56F))
    val js   = sj.render(inst)
    assertEquals("""{"m":{"12.34":56.78,"90.12":34.56}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleFloat](js))
  }

  test("With Int Key") {
    val inst = SampleInt(Map(12 -> 56, 90 -> 34))
    val js   = sj.render(inst)
    assertEquals("""{"m":{"12":56,"90":34}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleInt](js))
  }

  test("With Long Key") {
    val inst = SampleLong(Map(12L -> 56L, 90L -> 34L))
    val js   = sj.render(inst)
    assertEquals("""{"m":{"12":56,"90":34}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleLong](js))
  }

  test("With Short Key") {
    val inst =
      SampleShort(Map(12.toShort -> 56.toShort, 90.toShort -> 34.toShort))
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56,"90":34}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleShort](js))
  }

  test("Bad BigDecimal Key") {
    describe("--- Negative Tests ---")

    val js  = """{"m":{"789.123":1,"fred":2}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
                |fred
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleBigDecimal](js)
    }
  }

  test("Bad BigInt Key") {
    val js  = """{"m":{"fred":1,"789":2}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
                |fred
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleBigInt](js)
    }
  }

  test("Bad Boolean Key") {
    val js  = """{"m":{"true":false,"123":true}}""".asInstanceOf[JSON]
    val msg = """Expected a Boolean here
                |123
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleBoolean](js)
    }
  }

  test("Bad Byte Key") {
    val js  = """{"m":{"16":2,"x48":9}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
                |x48
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleByte](js)
    }
  }

  test("Bad Char Key") { // NOTE: This comprehensively tests for any null keyed Map
    val js  = """{"m":{null:"A","z":"Z"}}""".asInstanceOf[JSON]
    val msg = """A Char typed value cannot be null
                |{"m":{null:"A","z":"Z"}}
                |---------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleChar](js)
    }
  }

  test("Bad Double Key") {
    val js  = """{"m":{"12.34":56.78,"true":34.56}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
                |true
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleDouble](js)
    }
  }

  test("Bad Enumeration Key") {
    val js = """{"m":{"Small":"Large","Bogus":"Medium"}}""".asInstanceOf[JSON]
    val msg =
      """No value found in enumeration co.blocke.scalajack.json.mapkeys.Size$ for Bogus
        |{"m":{"Small":"Large","Bogus":"Medium"}}
        |----------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleEnumeration](js)
    }
  }

  test("Bad Float Key") {
    val js  = """{"m":{"12.34":56.78,"90.12.3":34.56}}""".asInstanceOf[JSON]
    val msg = """Cannot parse an Float from value
                |90.12.3
                |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleFloat](js)
    }
  }

  test("Bad Int Key") {
    val js  = """{"m":{"12.0":56,"90":34}}""".asInstanceOf[JSON]
    val msg = """Cannot parse an Int from value
                |12.0
                |---^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleInt](js)
    }
  }

  test("Bad Long Key") {
    val js  = """{"m":{"12":56,"hey":34}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
                |hey
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleLong](js)
    }
  }

  test("Bad Short Key") {
    val js  = """{"m":{"p99999":56,"90":34}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
                |p99999
                |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleShort](js)
    }
  }
