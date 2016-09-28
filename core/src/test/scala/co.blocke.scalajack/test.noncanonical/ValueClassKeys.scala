package co.blocke.scalajack
package test
package noncanonical

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

class ValueClassKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------------------\n:  ValueClass Noncanonical Tests  :\n-----------------------------------") {
    describe("+++ Positive Primitive Tests +++") {
      it("Value class of BigDecimal") {
        val inst = SampleVCBigDecimal(Map(VCBigDecimal(BigDecimal(12.34)) -> VCBigDecimal(BigDecimal(56.78))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12.34}":{"vc":56.78}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCBigDecimal](js)
        }
      }
      it("Value class of BigInt") {
        val inst = SampleVCBigInt(Map(VCBigInt(BigInt(12)) -> VCBigInt(BigInt(56))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12}":{"vc":56}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCBigInt](js)
        }
      }
      it("Value class of Byte") {
        val inst = SampleVCByte(Map(VCByte(12.asInstanceOf[Byte]) -> VCByte(56.asInstanceOf[Byte])))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12}":{"vc":56}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCByte](js)
        }
      }
      it("Value class of Boolean") {
        val inst = SampleVCBoolean(Map(VCBoolean(true) -> VCBoolean(false)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":true}":{"vc":false}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCBoolean](js)
        }
      }
      it("Value class of Char") {
        val inst = SampleVCChar(Map(VCChar('a') -> VCChar('A')))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":\"a\"}":{"vc":"A"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCChar](js)
        }
      }
      it("Value class of Double") {
        val inst = SampleVCDouble(Map(VCDouble(12.34) -> VCDouble(56.78)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12.34}":{"vc":56.78}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCDouble](js)
        }
      }
      it("Value class of Enumeration") {
        val inst = SampleVCEnumeration(Map(VCEnumeration(Food.Veggies) -> VCEnumeration(Food.Meat)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":\"Veggies\"}":{"vc":"Meat"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCEnumeration](js)
        }
      }
      it("Value class of Float") {
        val inst = SampleVCFloat(Map(VCFloat(12.34F) -> VCFloat(56.2F)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12.34}":{"vc":56.2}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCFloat](js)
        }
      }
      it("Value class of Int") {
        val inst = SampleVCInt(Map(VCInt(12) -> VCInt(56)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12}":{"vc":56}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCInt](js)
        }
      }
      it("Value class of Long") {
        val inst = SampleVCLong(Map(VCLong(12L) -> VCLong(56L)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12}":{"vc":56}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCLong](js)
        }
      }
      it("Value class of Short") {
        val inst = SampleVCShort(Map(VCShort(12.asInstanceOf[Short]) -> VCShort(56.asInstanceOf[Short])))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":12}":{"vc":56}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCShort](js)
        }
      }
      it("Value class of String") {
        val inst = SampleVCString(Map(VCString("A") -> VCString("B")))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":\"A\"}":{"vc":"B"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCString](js)
        }
      }
      it("Value class of UUID") {
        val inst = SampleVCUUID(Map(VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e56")) -> VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":\"54cab778-7b9e-4b07-9d37-87b97a011e56\"}":{"vc":"54cab778-7b9e-4b07-9d37-87b97a011e55"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCUUID](js)
        }
      }
    }
    describe("+++ Positive Collection Tests +++") {
      it("Value class of List") {
        val inst = SampleVCList(Map(VCList(List(1, 2, 3)) -> VCList(List(4, 5, 6))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":[1,2,3]}":{"vc":[4,5,6]}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCList](js)
        }
      }
      it("Value class of List (empty List)") {
        val inst = SampleVCList(Map(VCList(List.empty[Int]) -> VCList(List.empty[Int])))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":[]}":{"vc":[]}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCList](js)
        }
      }
      it("Value class of Map") {
        val inst = SampleVCMap(Map(VCMap(Map(1 -> 2)) -> VCMap(Map(3 -> 4))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":{\"1\":2}}":{"vc":{"3":4}}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCMap](js)
        }
      }
      it("Value class of Map (empty Map") {
        val inst = SampleVCMap(Map(VCMap(Map.empty[Int, Int]) -> VCMap(Map.empty[Int, Int])))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":{}}":{"vc":{}}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCMap](js)
        }
      }
      it("Value class of Tupple") {
        val inst = SampleVCTuple(Map(VCTuple((1, "one", true)) -> VCTuple((2, "two", false))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":[1,\"one\",true]}":{"vc":[2,"two",false]}}}""") { js }
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
        assertResult("""{"m":{"{\"vc\":{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"name\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}}":{"vc":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCClass](js)
        }
      }
      it("Value class of Trait") {
        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val inst = SampleVCTrait(Map(VCTrait(a) -> VCTrait(b)))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":{\"_hint\":\"co.blocke.scalajack.test.noncanonical.FishPet\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"vc":{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCTrait](js)
        }
      }
      it("Value class of Parameterized Case Class") {
        (pending)
      }
      it("Value class of Parameterized Trait") {
        (pending)
      }
      it("Value class of Option") {
        val inst = SampleVCOption(Map(VCOption(Some("here")) -> VCOption(Some("there"))))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":\"here\"}":{"vc":"there"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCOption](js)
        }
      }
      it("Value class of nested collection") {
        val a = VCNested(List(Map("a" -> "b"), Map("c" -> "d")))
        val b = VCNested(List(Map("t" -> "u"), Map("x" -> "y")))
        val inst = SampleVCNested(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"vc\":[{\"a\":\"b\"},{\"c\":\"d\"}]}":{"vc":[{"t":"u"},{"x":"y"}]}}}""") { js }
        assertResult(inst) {
          sj.read[SampleVCNested](js)
        }
      }
    }
    describe("--- Negative Primitive Tests ---") {
      it("Bad BigDecimal Key") {
        val js = """{"m":{"{\"vc\":true}":{"vc":56.78}}}"""
        val msg = """Expected value token of type Number, not True when reading BigDecimal value.  (Is your value wrapped in quotes?)
          |{"m":{"{\"vc\":true}":{"vc":56.78}}}
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCBigDecimal](js) should have message msg
      }
      it("Bad BigInt Key") {
        val js = """{"m":{"{\"vc\":12.5}":{"vc":56}}}"""
        val msg = """For input string: "12.5"
          |{"m":{"{\"vc\":12.5}":{"vc":56}}}
          |-------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleVCBigInt](js) should have message msg
      }
      it("Bad Byte Key") {
        val js = """{"m":{"{\"vc\":12234}":{"vc":56}}}"""
        val msg = """Value out of range. Value:"12234" Radix:10
          |{"m":{"{\"vc\":12234}":{"vc":56}}}
          |-------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleVCByte](js) should have message msg
      }
      it("Bad Boolean Key") {
        val js = """{"m":{"{\"vc\":1}":{"vc":false}}}"""
        val msg = """Expected value token of type True or False, not Number when reading Boolean value.  (Is your value wrapped in quotes or a number?)
          |{"m":{"{\"vc\":1}":{"vc":false}}}
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCBoolean](js) should have message msg
      }
      it("Bad Char Key") {
        val js = """{"m":{"{\"vc\":null}":{"vc":"A"}}}"""
        val msg = """Expected token of type String, not Null
          |{"m":{"{\"vc\":null}":{"vc":"A"}}}
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCChar](js) should have message msg
      }
      it("Bad Double Key") {
        val js = """{"m":{"{\"vc\":false}":{"vc":56.78}}}"""
        val msg = """Expected token of type Number, not False
          |{"m":{"{\"vc\":false}":{"vc":56.78}}}
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCDouble](js) should have message msg
      }
      it("Bad Enumeration Key") {
        val js = """{"m":{"{\"vc\":\"Bogus\"}":{"vc":"Meat"}}}"""
        val msg = """No value found in enumeration co.blocke.scalajack.test.noncanonical.Food$ for 'Bogus'
          |{"vc":"Bogus"}
          |-------^""".stripMargin
        the[java.util.NoSuchElementException] thrownBy sj.read[SampleVCEnumeration](js) should have message msg
      }
      it("Bad Float Key") {
        val js = """{"m":{"{\"vc\":"hey"}":{"vc":56.2}}}"""
        val msg = """Character out of place. Un-quoted literal not expected here.  (Possile un-terminated string earlier in your JSON.)
          |{"m":{"{\"vc\":"hey"}":{"vc":56.2}}}
          |----------------^""".stripMargin
        the[java.lang.IllegalArgumentException] thrownBy sj.read[SampleVCFloat](js) should have message msg
      }
      it("Bad Int Key") {
        val js = """{"m":{"{\"vc\":12.5}":{"vc":56}}}"""
        val msg = """For input string: "12.5"
          |{"m":{"{\"vc\":12.5}":{"vc":56}}}
          |-------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleVCInt](js) should have message msg
      }
      it("Bad Long Key") {
        val js = """{"m":{"{\"vc\":12.5}":{"vc":56}}}"""
        val msg = """For input string: "12.5"
          |{"m":{"{\"vc\":12.5}":{"vc":56}}}
          |-------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleVCLong](js) should have message msg
      }
      it("Bad Short Key") {
        val js = """{"m":{"{\"vc\":12.5}":{"vc":56}}}"""
        val msg = """For input string: "12.5"
          |{"m":{"{\"vc\":12.5}":{"vc":56}}}
          |-------^""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleVCShort](js) should have message msg
      }
      it("Bad String Key") {
        val js = """{"m":{"{\"vc\":true}":{"vc":"B"}}}"""
        val msg = """Expected value token of type String, not True when reading String value.
          |{"m":{"{\"vc\":true}":{"vc":"B"}}}
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCString](js) should have message msg
      }
      it("Bad UUID Key") {
        val js = """{"m":{"{\"vc\":\"bogus\"}":{"vc":"54cab778-7b9e-4b07-9d37-87b97a011e55"}}}"""
        val msg = """Invalid UUID string: bogus
          |{"vc":"bogus"}
          |-------^""".stripMargin
        the[java.lang.IllegalArgumentException] thrownBy sj.read[SampleVCUUID](js) should have message msg
      }
    }
    describe("--- Negative Collection Tests ---") {
      it("Bad List Key") {
        val js = """{"m":{"{\"vc\":[1,2,\"a\"]}":{"vc":[4,5,6]}}}"""
        val msg = """Expected token of type Number, not String
          |{"m":{"{\"vc\":[1,2,\"a\"]}":{"vc":[4,5,6]}}}
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCList](js) should have message msg
      }
      it("Bad Map Key") {
        val js = """{"m":{"{\"vc\":{[true]:2}}":{"vc":{"3":4}}}}"""
        val msg = """Character out of place. '[' not expected here.
          |{"vc":{[true]:2}}
          |-------^ Extracted from source here:
          |{"m":{"{\"vc\":{[true]:2}}":{"vc":{"3":4}}}}
          |-------^""".stripMargin
        the[java.lang.IllegalArgumentException] thrownBy sj.read[SampleVCMap](js) should have message msg
      }
      it("Bad Tupple Key") {
        val js = """{"m":{"{\"vc\":[1,\"one\",true,1]}":{"vc":[2,"two",false]}}}"""
        val msg = """Expected token of type EndArray, not Number
          |{"m":{"{\"vc\":[1,\"one\",true,1]}":{"vc":[2,"two",false]
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCTuple](js) should have message msg
      }
    }
    describe("--- Negative Complex Tests ---") {
      it("Bad Case Class Key") {
        val js = """{"m":{"{\"vc\":{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"bogus\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}}":{"vc":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}}"""
        val msg = """Required field name in class co.blocke.scalajack.test.noncanonical.SimpleClass is missing from input and has no specified default value
          |{"m":{"{\"vc\":{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCClass](js) should have message msg
      }
      it("Bad Trait Key") {
        val js = """{"m":{"{\"vc\":{\"_hint\":\"co.blocke.scalajack.test.noncanonical.Bogus\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"vc":{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}}"""
        val msg = """Unable to find class named "co.blocke.scalajack.test.noncanonical.Bogus"
          |{"m":{"{\"vc\":{\"_hint\":\"co.blocke.scalajack.test.nonc
          |-------^""".stripMargin
        the[java.lang.ClassNotFoundException] thrownBy sj.read[SampleVCTrait](js) should have message msg
      }
      it("Bad Parameterized Case Class Key") {
        (pending)
      }
      it("Bad Parameterized Trait Key") {
        (pending)
      }
      it("Bad Option Key") {
        val js = """{"m":{"{\"vc\":true}":{"vc":"there"}}}"""
        val msg = """Expected value token of type String, not True when reading String value.
          |{"m":{"{\"vc\":true}":{"vc":"there"}}}
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCOption](js) should have message msg
      }
      it("Bad Nested Collection Key") {
        val js = """{"m":{"{\"vc\":[{\"a\":\"b\"},{\"c\":9}]}":{"vc":[{"t":"u"},{"x":"y"}]}}}"""
        val msg = """Expected value token of type String, not Number when reading String value.
          |{"m":{"{\"vc\":[{\"a\":\"b\"},{\"c\":9}]}":{"vc":[{"t":"u
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleVCNested](js) should have message msg
      }
    }
  }
}