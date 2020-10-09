package co.blocke.scalajack
package json.mapkeys

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import java.util.UUID
import co.blocke.scalajack.model._

class ValueClassKeys() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("Value class of BigDecimal") {
    describe(
      "------------------------------\n:  ValueClass Map Key Tests  :\n------------------------------", Console.BLUE
    )
    describe("+++ Positive Primitive Tests +++")

    val inst = SampleVCBigDecimal(
      Map(
        VCBigDecimal(BigDecimal(12.34)) -> VCBigDecimal(BigDecimal(56.78))
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12.34":56.78}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCBigDecimal](js))
  }

  test("Value class of BigInt") {
    val inst =
      SampleVCBigInt(Map(VCBigInt(BigInt(12)) -> VCBigInt(BigInt(56))))
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCBigInt](js))
  }

  test("Value class of Byte") {
    val inst = SampleVCByte(
      Map(VCByte(12.asInstanceOf[Byte]) -> VCByte(56.asInstanceOf[Byte]))
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCByte](js))
  }

  test("Value class of Boolean") {
    val inst = SampleVCBoolean(Map(VCBoolean(true) -> VCBoolean(false)))
    val js = sj.render(inst)
    assertEquals("""{"m":{"true":false}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCBoolean](js))
  }

  test("Value class of Char") {
    val inst = SampleVCChar(Map(VCChar('a') -> VCChar('A')))
    val js = sj.render(inst)
    assertEquals("""{"m":{"a":"A"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCChar](js))
  }
  
  test("Value class of Double") {
    val inst = SampleVCDouble(Map(VCDouble(12.34) -> VCDouble(56.78)))
    val js = sj.render(inst)
    assertEquals("""{"m":{"12.34":56.78}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCDouble](js))
  }

  test("Value class of Enumeration") {
    val inst = SampleVCEnumeration(
      Map(VCEnumeration(Food.Veggies) -> VCEnumeration(Food.Meat))
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"Veggies":"Meat"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCEnumeration](js))
  }

  test("Value class of Float") {
    val inst = SampleVCFloat(Map(VCFloat(12.34F) -> VCFloat(56.2F)))
    val js = sj.render(inst)
    assertEquals("""{"m":{"12.34":56.2}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCFloat](js))
  }

  test("Value class of Int") {
    val inst = SampleVCInt(Map(VCInt(12) -> VCInt(56)))
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCInt](js))
  }

  test("Value class of Long") {
    val inst = SampleVCLong(Map(VCLong(12L) -> VCLong(56L)))
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCLong](js))
  }

  test("Value class of Short") {
    val inst = SampleVCShort(
      Map(
        VCShort(12.asInstanceOf[Short]) -> VCShort(56.asInstanceOf[Short])
      )
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"12":56}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCShort](js))
  }

  test("Value class of String") {
    val inst = SampleVCString(Map(VCString("A") -> VCString("B")))
    val js = sj.render(inst)
    assertEquals("""{"m":{"A":"B"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCString](js))
  }

  test("Value class of UUID") {
    val inst = SampleVCUUID(
      Map(
        VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e56")) -> VCUUID(
          UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55")
        )
      )
    )
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"54cab778-7b9e-4b07-9d37-87b97a011e56":"54cab778-7b9e-4b07-9d37-87b97a011e55"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCUUID](js))
  }

  test("Value class of List") {
    describe("+++ Positive Collection Tests +++")

    val inst =
      SampleVCList(Map(VCList(List(1, 2, 3)) -> VCList(List(4, 5, 6))))
    val js = sj.render(inst)
    assertEquals("""{"m":{"[1,2,3]":[4,5,6]}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCList](js))
  }

  test("Value class of List (empty List)") {
    val inst =
      SampleVCList(Map(VCList(List.empty[Int]) -> VCList(List.empty[Int])))
    val js = sj.render(inst)
    assertEquals("""{"m":{"[]":[]}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCList](js))
  }

  test("Value class of Map") {
    val inst = SampleVCMap(Map(VCMap(Map(1 -> 2)) -> VCMap(Map(3 -> 4))))
    val js = sj.render(inst)
    assertEquals("""{"m":{"{\"1\":2}":{"3":4}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCMap](js))
  }

  test("Value class of Map (empty Map") {
    val inst = SampleVCMap(
      Map(VCMap(Map.empty[Int, Int]) -> VCMap(Map.empty[Int, Int]))
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"{}":{}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCMap](js))
  }

  test("Value class of Tupple") {
    val inst = SampleVCTuple(
      Map(VCTuple((1, "one", true)) -> VCTuple((2, "two", false)))
    )
    val js = sj.render(inst)
    assertEquals("""{"m":{"[1,\"one\",true]":[2,"two",false]}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCTuple](js))
  }

  test("Value class of Case Class") {
    describe("+++ Positive Complex Tests +++")

    val a = SimpleClass("Larry", 32, isOk = true, "golf")
    val b = SimpleClass("Mike", 27, isOk = false, 125)
    val c1 = ComplexClass(
      UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),
      a,
      allDone = true
    )
    val c2 = ComplexClass(
      UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d"),
      b,
      allDone = false
    )
    val inst = SampleVCClass(Map(VCClass(c1) -> VCClass(c2)))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"name\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCClass](js))
  }

  test("Value class of Trait") {
    val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet = DogPet("Fido", Food.Meat, 3)
    val inst = SampleVCTrait(Map(VCTrait(a) -> VCTrait(b)))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.FishPet\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCTrait](js))
  }

  test("Value class of Parameterized Case Class") {
    val a = AThing(5, "wow")
    val b = AThing(6, "zoom")
    val inst = SampleVCParamClass(Map(VCParamClass(a) -> VCParamClass(b)))
    val js = sj.render(inst)
    assertEquals("""{"m":{"{\"a\":5,\"b\":\"wow\"}":{"a":6,"b":"zoom"}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCParamClass[String, Int]](js))
  }

  test("Value class of Parameterized Trait") {
    val a: Thing[Int, String] = AThing(5, "wow")
    val b: Thing[Int, String] = AThing(6, "zoom")
    val inst = SampleVCParamTrait(Map(VCParamTrait(a) -> VCParamTrait(b)))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.AThing\",\"a\":5,\"b\":\"wow\"}":{"_hint":"co.blocke.scalajack.json.mapkeys.AThing","a":6,"b":"zoom"}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCParamTrait[Int, String]](js))
  }

  test("Value class of Option") {
    val inst =
      SampleVCOption(Map(VCOption(Some("here")) -> VCOption(Some("there"))))
    val js = sj.render(inst)
    assertEquals("""{"m":{"here":"there"}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCOption](js))
  }

  test("Value class of nested collection") {
    val a = VCNested(List(Map("a" -> "b"), Map("c" -> "d")))
    val b = VCNested(List(Map("t" -> "u"), Map("x" -> "y")))
    val inst = SampleVCNested(Map(a -> b))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"[{\"a\":\"b\"},{\"c\":\"d\"}]":[{"t":"u"},{"x":"y"}]}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleVCNested](js))
  }

  test("Bad BigDecimal Key") {
    describe("--- Negative Primitive Tests ---")

    val js = """{"m":{"true":56.78}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |true
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCBigDecimal](js)
    }
  }

  test("Bad BigInt Key") {
    val js = """{"m":{"12.5":56}}""".asInstanceOf[JSON]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"12.5\""){
      sj.read[SampleVCBigInt](js)
    }
  }

  test("Bad Boolean Key") {
    val js = """{"m":{"1":false}}""".asInstanceOf[JSON]
    val msg = """Expected a Boolean here
              |1
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCBoolean](js)
    }
  }

  test("Bad Char Key") {
    val js = """{"m":{null:"A"}}""".asInstanceOf[JSON]
    val msg = """A Char typed value cannot be null
              |{"m":{null:"A"}}
              |---------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCChar](js)
    }
  }

  test("Bad Double Key") {
    val js = """{"m":{"false":56.78}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |false
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCDouble](js)
    }
  }

  test("Bad Enumeration Key") {
    val js = """{"m":{"Bogus":"Meat"}}""".asInstanceOf[JSON]
    val msg =
      """No value found in enumeration co.blocke.scalajack.json.mapkeys.Food$ for Bogus
              |{"m":{"Bogus":"Meat"}}
              |------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCEnumeration](js)
    }
  }

  test("Bad Float Key") {
    val js = """{"m":{"hey":56.2}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |hey
              |^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCFloat](js)
    }
  }

  test("Bad Int Key") {
    val js = """{"m":{"12.5":56}}""".asInstanceOf[JSON]
    val msg = """Cannot parse an Int from value
              |12.5
              |---^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCInt](js)
    }
  }

  test("Bad Long Key") {
    val js = """{"m":{"12.5":56}}""".asInstanceOf[JSON]
    val msg = """Cannot parse an Long from value
              |12.5
              |---^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCLong](js)
    }
  }

  test("Bad Short Key") {
    val js = """{"m":{"12.5":56}}""".asInstanceOf[JSON]
    val msg = """Cannot parse an Short from value
              |12.5
              |---^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCShort](js)
    }
  }

  test("Bad String Key") {
    val js = """{"m":{true:"B"}}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{"m":{true:"B"}}
              |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCString](js)
    }
  }

  test("Bad UUID Key") {
    val js = """{"m":{"bogus":"54cab778-7b9e-4b07-9d37-87b97a011e55"}}""".asInstanceOf[JSON]
    val msg = """Failed to create UUID value from parsed text bogus
              |{"m":{"bogus":"54cab778-7b9e-4b07-9d37-87b97a011e55"}}
              |------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCUUID](js)
    }
  }

  test("Bad List Key") {
    describe("--- Negative Collection Tests ---")

    val js = """{"m":{"[1,2,\"a\"]":[4,5,6]}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |[1,2,"a"]
              |-----^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCList](js)
    }
  }

  test("Bad Map Key") {
    val js = """{"m":{"{[true]:2}":{"3":4}}}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{[true]:2}
              |-^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCMap](js)
    }
  }

  test("Bad Tuple Key") {
    val js = """{"m":{"[1,\"one\",true,1]":[2,"two",false]}}""".asInstanceOf[JSON]
    val msg = """Expected end of tuple here
              |[1,"one",true,1]
              |-------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCTuple](js)
    }
  }

  test("Bad Case Class Key") {
    describe("--- Negative Complex Tests ---")

    val js =
      """{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"bogus\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}""".asInstanceOf[JSON]
    val msg =
      """Class co.blocke.scalajack.json.mapkeys.SimpleClass missing required fields: name
              |...s":"Larry","age":32,"isOk":true,"favorite":"golf"},"allDone":true}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCClass](js)
    }
  }

  test("Bad Trait Key") {
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.Bogus\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""".asInstanceOf[JSON]
    interceptMessage[java.lang.ClassNotFoundException]("co.blocke.scalajack.json.mapkeys.Bogus"){
      sj.read[SampleVCTrait](js)
    }
  }

  test("Bad Parameterized Case Class Key") {
    val js = """{"m":{"{\"a\":5.5,\"b\":\"wow\"}":{"a":6,"b":"zoom"}}}""".asInstanceOf[JSON]
    val msg = """Cannot parse an Int from value
                |{"a":5.5,"b":"wow"}
                |-------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCParamClass[String, Int]](js)
    }
  }

  test("Bad Parameterized Trait Key") {
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.ZThing\",\"a\":5,\"b\":\"wow\"}":{"_hint":"co.blocke.scalajack.mapkeys.AThing","a":6,"b":"zoom"}}}""".asInstanceOf[JSON]
    interceptMessage[java.lang.ClassNotFoundException]("co.blocke.scalajack.json.mapkeys.ZThing"){
      sj.read[SampleVCParamTrait[Int, String]](js)
    }
  }

  test("Bad Option Key") {
    val js = """{"m":{true:"there"}}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |{"m":{true:"there"}}
              |------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCOption](js)
    }
  }

  test("Bad Nested Collection Key") {
    val js = """{"m":{"[{\"a\":\"b\"},{\"c\":9}]":[{"t":"u"},{"x":"y"}]}}""".asInstanceOf[JSON]
    val msg = """Expected a String here
              |[{"a":"b"},{"c":9}]
              |----------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleVCNested](js)
    }
  }