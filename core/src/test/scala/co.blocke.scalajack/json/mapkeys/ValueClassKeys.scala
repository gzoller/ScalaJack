package co.blocke.scalajack
package json.test.mapkeys

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID

class ValueClassKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------------------\n:  ValueClass Map Key Tests  :\n------------------------------") {
    describe("+++ Positive Primitive Tests +++") {
      it("Value class of BigDecimal") {
        val inst = SampleVCBigDecimal(Map(VCBigDecimal(BigDecimal(12.34)) -> VCBigDecimal(BigDecimal(56.78))))
        val js = sj.render(inst)
        assertResult("""{"m":{"12.34":56.78}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCBigDecimal](js)
        }
      }
      it("Value class of BigInt") {
        val inst = SampleVCBigInt(Map(VCBigInt(BigInt(12)) -> VCBigInt(BigInt(56))))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCBigInt](js)
        }
      }
      it("Value class of Byte") {
        val inst = SampleVCByte(Map(VCByte(12.asInstanceOf[Byte]) -> VCByte(56.asInstanceOf[Byte])))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCByte](js)
        }
      }
      it("Value class of Boolean") {
        val inst = SampleVCBoolean(Map(VCBoolean(true) -> VCBoolean(false)))
        val js = sj.render(inst)
        assertResult("""{"m":{"true":false}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCBoolean](js)
        }
      }
      it("Value class of Char") {
        val inst = SampleVCChar(Map(VCChar('a') -> VCChar('A')))
        val js = sj.render(inst)
        assertResult("""{"m":{"\"a\"":"A"}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCChar](js)
        }
      }
      it("Value class of Double") {
        val inst = SampleVCDouble(Map(VCDouble(12.34) -> VCDouble(56.78)))
        val js = sj.render(inst)
        assertResult("""{"m":{"12.34":56.78}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCDouble](js)
        }
      }
      it("Value class of Enumeration") {
        val inst = SampleVCEnumeration(Map(VCEnumeration(Food.Veggies) -> VCEnumeration(Food.Meat)))
        val js = sj.render(inst)
        assertResult("""{"m":{"\"Veggies\"":"Meat"}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCEnumeration](js)
        }
      }
      it("Value class of Float") {
        val inst = SampleVCFloat(Map(VCFloat(12.34F) -> VCFloat(56.2F)))
        val js = sj.render(inst)
        assertResult("""{"m":{"12.34":56.2}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCFloat](js)
        }
      }
      it("Value class of Int") {
        val inst = SampleVCInt(Map(VCInt(12) -> VCInt(56)))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCInt](js)
        }
      }
      it("Value class of Long") {
        val inst = SampleVCLong(Map(VCLong(12L) -> VCLong(56L)))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCLong](js)
        }
      }
      it("Value class of Short") {
        val inst = SampleVCShort(Map(VCShort(12.asInstanceOf[Short]) -> VCShort(56.asInstanceOf[Short])))
        val js = sj.render(inst)
        assertResult("""{"m":{"12":56}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCShort](js)
        }
      }
      it("Value class of String") {
        val inst = SampleVCString(Map(VCString("A") -> VCString("B")))
        val js = sj.render(inst)
        assertResult("""{"m":{"\"A\"":"B"}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCString](js)
        }
      }
      it("Value class of UUID") {
        val inst = SampleVCUUID(Map(VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e56")) -> VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))))
        val js = sj.render(inst)
        assertResult("""{"m":{"\"54cab778-7b9e-4b07-9d37-87b97a011e56\"":"54cab778-7b9e-4b07-9d37-87b97a011e55"}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCUUID](js)
        }
      }
    }
    describe("+++ Positive Collection Tests +++") {
      it("Value class of List") {
        val inst = SampleVCList(Map(VCList(List(1, 2, 3)) -> VCList(List(4, 5, 6))))
        val js = sj.render(inst)
        assertResult("""{"m":{"[1,2,3]":[4,5,6]}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCList](js)
        }
      }
      it("Value class of List (empty List)") {
        val inst = SampleVCList(Map(VCList(List.empty[Int]) -> VCList(List.empty[Int])))
        val js = sj.render(inst)
        assertResult("""{"m":{"[]":[]}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCList](js)
        }
      }
      it("Value class of Map") {
        val inst = SampleVCMap(Map(VCMap(Map(1 -> 2)) -> VCMap(Map(3 -> 4))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"1\":2}":{"3":4}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCMap](js)
        }
      }
      it("Value class of Map (empty Map") {
        val inst = SampleVCMap(Map(VCMap(Map.empty[Int, Int]) -> VCMap(Map.empty[Int, Int])))
        val js = sj.render(inst)
        assertResult("""{"m":{"{}":{}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCMap](js)
        }
      }
      it("Value class of Tupple") {
        val inst = SampleVCTuple(Map(VCTuple((1, "one", true)) -> VCTuple((2, "two", false))))
        val js = sj.render(inst)
        assertResult("""{"m":{"[1,\"one\",true]":[2,"two",false]}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCTuple](js)
        }
      }
    }
    describe("+++ Positive Complex Tests +++") {
      it("Value class of Case Class") {
        val a = SimpleClass("Larry", 32, true, "golf")
        val b = SimpleClass("Mike", 27, false, 125)
        val c1 = ComplexClass(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), a, true)
        val c2 = ComplexClass(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d"), b, false)
        val inst = SampleVCClass(Map(VCClass(c1) -> VCClass(c2)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"name\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCClass](js)
        }
      }
      it("Value class of Trait") {
        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val inst = SampleVCTrait(Map(VCTrait(a) -> VCTrait(b)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.FishPet\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.test.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCTrait](js)
        }
      }
      it("Value class of Parameterized Case Class") {
        val a = AThing(5, "wow")
        val b = AThing(6, "zoom")
        val inst = SampleVCParamClass(Map(VCParamClass(a) -> VCParamClass(b)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"a\":5,\"b\":\"wow\"}":{"a":6,"b":"zoom"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCParamClass[String, Int]](js)
        }
      }
      it("Value class of Parameterized Trait") {
        val a: Thing[Int, String] = AThing(5, "wow")
        val b: Thing[Int, String] = AThing(6, "zoom")
        val inst = SampleVCParamTrait(Map(VCParamTrait(a) -> VCParamTrait(b)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.AThing\",\"a\":5,\"b\":\"wow\"}":{"_hint":"co.blocke.scalajack.json.test.mapkeys.AThing","a":6,"b":"zoom"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCParamTrait[Int, String]](js)
        }
      }
      it("Value class of Option") {
        val inst = SampleVCOption(Map(VCOption(Some("here")) -> VCOption(Some("there"))))
        val js = sj.render(inst)
        assertResult("""{"m":{"\"here\"":"there"}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCOption](js)
        }
      }
      it("Value class of nested collection") {
        val a = VCNested(List(Map("a" -> "b"), Map("c" -> "d")))
        val b = VCNested(List(Map("t" -> "u"), Map("x" -> "y")))
        val inst = SampleVCNested(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[{\"a\":\"b\"},{\"c\":\"d\"}]":[{"t":"u"},{"x":"y"}]}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCNested](js)
        }
      }
    }
    describe("--- Negative Primitive Tests ---") {
      it("Bad BigDecimal Key") {
        val js = """{"m":{"true":56.78}}"""
        val msg = """[$.m.(map key)]: Expected a Number (Decimal) but parsed True
                    |true
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCBigDecimal](js) should have message msg
      }
      it("Bad BigInt Key") {
        val js = """{"m":{"12.5":56}}"""
        val msg = """[$.m.(map key)]: Failed to create BigInt value from parsed text 12.5
                    |12.5
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleVCBigInt](js) should have message msg
      }
      it("Bad Boolean Key") {
        val js = """{"m":{"1":false}}"""
        val msg = """[$.m.(map key)]: Expected a Boolean but parsed Number
                    |1
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCBoolean](js) should have message msg
      }
      it("Bad Char Key") {
        val js = """{"m":{"null":"A"}}"""
        val msg = """[$.m.(map key)]: A Char typed value cannot be null
                    |null
                    |----^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleVCChar](js) should have message msg
      }
      it("Bad Double Key") {
        val js = """{"m":{"false":56.78}}"""
        val msg = """[$.m.(map key)]: Expected a Double but parsed False
                    |false
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCDouble](js) should have message msg
      }
      it("Bad Enumeration Key") {
        val js = """{"m":{"\"Bogus\"":"Meat"}}"""
        val msg = """[$.m.(map key)]: No value found in enumeration co.blocke.scalajack.json.test.mapkeys.Food$ for Bogus
                    |"Bogus"
                    |-------^""".stripMargin
        the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[SampleVCEnumeration](js) should have message msg
      }
      it("Bad Float Key") {
        val js = """{"m":{"hey":56.2}}"""
        val msg = """[<tokenizing>]: Unexpected character h at position 0
                    |hey
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCFloat](js) should have message msg
      }
      it("Bad Int Key") {
        val js = """{"m":{"12.5":56}}"""
        val msg = """[$.m.(map key)]: Failed to create Int value from parsed text 12.5
                    |12.5
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleVCInt](js) should have message msg
      }
      it("Bad Long Key") {
        val js = """{"m":{"12.5":56}}"""
        val msg = """[$.m.(map key)]: Failed to create Long value from parsed text 12.5
                    |12.5
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleVCLong](js) should have message msg
      }
      it("Bad Short Key") {
        val js = """{"m":{"12.5":56}}"""
        val msg = """[$.m.(map key)]: Failed to create Int value from parsed text 12.5
                    |12.5
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleVCShort](js) should have message msg
      }
      it("Bad String Key") {
        val js = """{"m":{"true":"B"}}"""
        val msg = """[$.m.(map key)]: Expected a String but parsed True
                    |true
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCString](js) should have message msg
      }
      it("Bad UUID Key") {
        val js = """{"m":{"\"bogus\"":"54cab778-7b9e-4b07-9d37-87b97a011e55"}}"""
        val msg = """[$.m.(map key)]: Failed to create UUID value from parsed text bogus
                    |"bogus"
                    |-------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleVCUUID](js) should have message msg
      }
    }
    describe("--- Negative Collection Tests ---") {
      it("Bad List Key") {
        val js = """{"m":{"[1,2,\"a\"]":[4,5,6]}}"""
        val msg = """[$.m.(map key)[2]]: Expected an Int but parsed String
                    |[1,2,"a"]
                    |------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCList](js) should have message msg
      }
      it("Bad Map Key") {
        val js = """{"m":{"{[true]:2}":{"3":4}}}"""
        val msg = """[$.m.(map key).(map key)]: Expected a String but parsed BeginArray
                    |{[true]:2}
                    |-^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCMap](js) should have message msg
      }
      it("Bad Tupple Key") {
        val js = """{"m":{"[1,\"one\",true,1]":[2,"two",false]}}"""
        val msg = """[$.m.(map key)]: Too many values in tuple list.  Should be 3 but actually there's 5
                    |[1,"one",true,1]
                    |---------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCTuple](js) should have message msg
      }
    }
    describe("--- Negative Complex Tests ---") {
      it("Bad Case Class Key") {
        val js = """{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"bogus\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}"""
        val msg = """[$.m.(map key).simple]: Class SimpleClass missing field name
                    |s":"Larry","age":32,"isOk":true,"favorite":"golf"},"allDone":true}
                    |--------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMissingError] thrownBy sj.read[SampleVCClass](js) should have message msg
      }
      it("Bad Trait Key") {
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.Bogus\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.test.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
        val msg = """[$.m.(map key)]: Unable to find class named "co.blocke.scalajack.json.test.mapkeys.Bogus"
                    |int":"co.blocke.scalajack.json.test.mapkeys.Bogus","name":"Flipper","food":"Veggies","waterTemp":74.
                    |--------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMissingError] thrownBy sj.read[SampleVCTrait](js) should have message msg
      }
      it("Bad Parameterized Case Class Key") {
        val js = """{"m":{"{\"a\":5.5,\"b\":\"wow\"}":{"a":6,"b":"zoom"}}}"""
        val msg = """[$.m.(map key).a]: Failed to create Int value from parsed text 5.5
                    |{"a":5.5,"b":"wow"}
                    |-----^""".stripMargin
        the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[SampleVCParamClass[String, Int]](js) should have message msg
      }
      it("Bad Parameterized Trait Key") {
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.ZThing\",\"a\":5,\"b\":\"wow\"}":{"_hint":"co.blocke.scalajack.test.mapkeys.AThing","a":6,"b":"zoom"}}}"""
        val msg = """[$.m.(map key)]: Unable to find class named "co.blocke.scalajack.json.test.mapkeys.ZThing"
                    |nt":"co.blocke.scalajack.json.test.mapkeys.ZThing","a":5,"b":"wow"}
                    |--------------------------------------------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadMissingError] thrownBy sj.read[SampleVCParamTrait[Int, String]](js) should have message msg
      }
      it("Bad Option Key") {
        val js = """{"m":{"true":"there"}}"""
        val msg = """[$.m.(map key)]: Expected a String but parsed True
                    |true
                    |^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCOption](js) should have message msg
      }
      it("Bad Nested Collection Key") {
        val js = """{"m":{"[{\"a\":\"b\"},{\"c\":9}]":[{"t":"u"},{"x":"y"}]}}"""
        val msg = """[$.m.(map key)[1].c]: Expected a String but parsed Number
                    |[{"a":"b"},{"c":9}]
                    |----------------^""".stripMargin
        the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[SampleVCNested](js) should have message msg
      }
    }
  }
}
