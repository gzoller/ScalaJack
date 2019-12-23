package co.blocke.scalajack
package yaml
package mapkeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import java.util.UUID

class ValueClassKeys() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "-------------------------------------\n:  ValueClass Map Key Tests (YAML)  :\n-------------------------------------"
  ) {
    describe("+++ Positive Primitive Tests +++") {
      it("Value class of BigDecimal") {
        val inst = SampleVCBigDecimal(
          Map(
            VCBigDecimal(BigDecimal(12.34)) -> VCBigDecimal(BigDecimal(56.78))
          )
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12.34: 56.78
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCBigDecimal](yaml)
        }
      }
      it("Value class of BigInt") {
        val inst =
          SampleVCBigInt(Map(VCBigInt(BigInt(12)) -> VCBigInt(BigInt(56))))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCBigInt](yaml)
        }
      }
      it("Value class of Byte") {
        val inst = SampleVCByte(
          Map(VCByte(12.asInstanceOf[Byte]) -> VCByte(56.asInstanceOf[Byte]))
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCByte](yaml)
        }
      }
      it("Value class of Boolean") {
        val inst       = SampleVCBoolean(Map(VCBoolean(true) -> VCBoolean(false)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  true: false
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCBoolean](yaml)
        }
      }
      it("Value class of Char") {
        val inst       = SampleVCChar(Map(VCChar('a') -> VCChar('A')))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  a: A
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCChar](yaml)
        }
      }
      it("Value class of Double") {
        val inst       = SampleVCDouble(Map(VCDouble(12.34) -> VCDouble(56.78)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12.34: 56.78
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCDouble](yaml)
        }
      }
      it("Value class of Enumeration") {
        val inst = SampleVCEnumeration(
          Map(VCEnumeration(Food.Veggies) -> VCEnumeration(Food.Meat))
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  Veggies: Meat
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCEnumeration](yaml)
        }
      }
      it("Value class of Float") {
        val inst       = SampleVCFloat(Map(VCFloat(12.34F) -> VCFloat(56.2F)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12.34: 56.2
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCFloat](yaml)
        }
      }
      it("Value class of Int") {
        val inst       = SampleVCInt(Map(VCInt(12) -> VCInt(56)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCInt](yaml)
        }
      }
      it("Value class of Long") {
        val inst       = SampleVCLong(Map(VCLong(12L) -> VCLong(56L)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCLong](yaml)
        }
      }
      it("Value class of Short") {
        val inst = SampleVCShort(
          Map(
            VCShort(12.asInstanceOf[Short]) -> VCShort(56.asInstanceOf[Short])
          )
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  12: 56
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCShort](yaml)
        }
      }
      it("Value class of String") {
        val inst       = SampleVCString(Map(VCString("A") -> VCString("B")))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  A: B
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCString](yaml)
        }
      }
      it("Value class of UUID") {
        val inst = SampleVCUUID(
          Map(
            VCUUID(UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e56")) -> VCUUID(
              UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55")
            )
          )
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  54cab778-7b9e-4b07-9d37-87b97a011e56: 54cab778-7b9e-4b07-9d37-87b97a011e55
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCUUID](yaml)
        }
      }
    }
    describe("+++ Positive Collection Tests +++") {
      it("Value class of List") {
        val inst =
          SampleVCList(Map(VCList(List(1, 2, 3)) -> VCList(List(4, 5, 6))))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? [1, 2, 3]
                           |  : [4, 5, 6]
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCList](yaml)
        }
      }
      it("Value class of List (empty List)") {
        val inst =
          SampleVCList(Map(VCList(List.empty[Int]) -> VCList(List.empty[Int])))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  []: []
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCList](yaml)
        }
      }
      it("Value class of Map") {
        val inst       = SampleVCMap(Map(VCMap(Map(1 -> 2)) -> VCMap(Map(3 -> 4))))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? 1: 2
                           |  : 3: 4
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCMap](yaml)
        }
      }
      it("Value class of Map (empty Map") {
        val inst = SampleVCMap(
          Map(VCMap(Map.empty[Int, Int]) -> VCMap(Map.empty[Int, Int]))
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  {}: {}
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCMap](yaml)
        }
      }
      it("Value class of Tupple") {
        val inst = SampleVCTuple(
          Map(VCTuple((1, "one", true)) -> VCTuple((2, "two", false)))
        )
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? [1, one, true]
                           |  : [2, two, false]
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCTuple](yaml)
        }
      }
    }
    describe("+++ Positive Complex Tests +++") {
      it("Value class of Case Class") {
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
        val inst       = SampleVCClass(Map(VCClass(c1) -> VCClass(c2)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? id: 1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c
                           |    simple:
                           |      name: Larry
                           |      age: 32
                           |      isOk: true
                           |      favorite: golf
                           |    allDone: true
                           |  : id: 1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d
                           |    simple:
                           |      name: Mike
                           |      age: 27
                           |      isOk: false
                           |      favorite: 125
                           |    allDone: false
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCClass](yaml)
        }
      }
      it("Value class of Trait") {
        val a: Pet     = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet     = DogPet("Fido", Food.Meat, 3)
        val inst       = SampleVCTrait(Map(VCTrait(a) -> VCTrait(b)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                           |    name: Flipper
                           |    food: Veggies
                           |    waterTemp: 74.33
                           |  : _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                           |    name: Fido
                           |    food: Meat
                           |    numLegs: 3
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCTrait](yaml)
        }
      }
      it("Value class of Parameterized Case Class") {
        val a          = AThing(5, "wow")
        val b          = AThing(6, "zoom")
        val inst       = SampleVCParamClass(Map(VCParamClass(a) -> VCParamClass(b)))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? a: 5
                           |    b: wow
                           |  : a: 6
                           |    b: zoom
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCParamClass[String, Int]](yaml)
        }
      }
      it("Value class of Parameterized Trait") {
        val a: Thing[Int, String] = AThing(5, "wow")
        val b: Thing[Int, String] = AThing(6, "zoom")
        val inst                  = SampleVCParamTrait(Map(VCParamTrait(a) -> VCParamTrait(b)))
        val yaml                  = sj.render(inst)
        val comparison            = """m:
                           |  ? _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                           |    a: 5
                           |    b: wow
                           |  : _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                           |    a: 6
                           |    b: zoom
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCParamTrait[Int, String]](yaml)
        }
      }
      it("Value class of Option") {
        val inst =
          SampleVCOption(Map(VCOption(Some("here")) -> VCOption(Some("there"))))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  here: there
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCOption](yaml)
        }
      }
      it("Value class of nested collection") {
        val a          = VCNested(List(Map("a" -> "b"), Map("c" -> "d")))
        val b          = VCNested(List(Map("t" -> "u"), Map("x" -> "y")))
        val inst       = SampleVCNested(Map(a -> b))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? - a: b
                           |    - c: d
                           |  : - t: u
                           |    - x: y
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleVCNested](yaml)
        }
      }
    }
    describe("--- Negative Primitive Tests ---") {
      it("Bad BigDecimal Key") {
        val yaml = """m: 
                     | true: 56.78""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCBigDecimal](yaml) should have message "Line 1: Expected a Number value here: =VAL :true"
      }
      it("Bad BigInt Key") {
        val yaml = """m: 
                     | "12.5": 56""".stripMargin
        the[java.lang.NumberFormatException] thrownBy sj.read[SampleVCBigInt](
          yaml
        ) should have message "For input string: \"12.5\""
      }
      it("Bad Boolean Key") {
        val yaml = """m: 
                     | 1: false""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCBoolean](yaml) should have message "Line 1: Expected a Boolean value here: =VAL :1"
      }
      it("Bad Char Key") {
        val yaml = """m: 
                     | null: A""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCChar](yaml) should have message "Line 1: A Char typed value cannot be null"
      }
      it("Bad Double Key") {
        val yaml = """m: 
                     | false: 56.78""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCDouble](yaml) should have message "Line 1: Expected a Number value here: =VAL :false"
      }
      it("Bad Enumeration Key") {
        val yaml = """m: 
                     | Bogus: Meat""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCEnumeration](yaml) should have message "Line 1: No value found in enumeration co.blocke.scalajack.yaml.mapkeys.Food$ for Bogus"
      }
      it("Bad Float Key") {
        val yaml = """m: 
                     | hey: 56.2""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCFloat](yaml) should have message "Line 1: Expected a Number value here: =VAL :hey"
      }
      it("Bad Int Key") {
        val yaml = """m: 
                     | "12.5": 56""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCInt](yaml) should have message "Line 1: Cannot parse an Int from value"
      }
      it("Bad Long Key") {
        val yaml = """m: 
                     | "12.5": 56""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCLong](yaml) should have message "Line 1: Cannot parse an Long from value"
      }
      it("Bad Short Key") {
        val yaml = """m: 
                     | "12.5": 56""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCShort](yaml) should have message "Line 1: Cannot parse an Short from value"
      }
      it("Bad String Key") {
        val yaml = """m: 
                     | [1,2]: B""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCString](yaml) should have message "Line 1: Expected a String here: +SEQ"
      }
      it("Bad UUID Key") {
        val yaml = """m: 
                     | bogus: "54cab778-7b9e-4b07-9d37-87b97a011e55"""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCUUID](yaml) should have message "Line 1: Failed to create UUID value from parsed text bogus"
      }
    }
    describe("--- Negative Collection Tests ---") {
      it("Bad List Key") {
        val yaml =
          """m: 
            |  [1,2,"a"]: [4,5,6]""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCList](yaml) should have message "Line 1: Expected a Number value here: =VAL \"a"
      }
      it("Bad Map Key") {
        val yaml =
          """m: 
            |  ?
            |    true: 2
            |  : 
            |    3: 4""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCMap](yaml) should have message "Line 2: Expected a Number value here: =VAL :true"
      }
      it("Bad Tuple Key") {
        val yaml =
          """m: 
            |  ?
            |    [1,one,true,1]
            |  : [2,two,false]""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCTuple](yaml) should have message "Line 2: Expected a String here: -SEQ"
      }
    }
    describe("--- Negative Complex Tests ---") {
      it("Bad Case Class Key") {
        val yaml =
          """m: 
            |  ?
            |    id: "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"
            |    simple: 
            |      bogus: Larry
            |      age: 32
            |      isOk: true
            |      favorite: golf
            |    allDone: true
            |  :
            |    id: "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d"
            |    simple: 
            |      name: Mike
            |      age: 27
            |      isOk: false
            |      favorite: 125
            |    allDone: false""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCClass](yaml) should have message "Line 8: Class SimpleClass missing required fields: name"
      }
      it("Bad Trait Key") {
        val yaml =
          """m: 
            |  ?
            |    _hint: "co.blocke.scalajack.yaml.mapkeys.Bogus"
            |    name: Flipper
            |    food: Veggies
            |    waterTemp: 74.33
            |  :
            |    _hint: "co.blocke.scalajack.yaml.mapkeys.DogPet"
            |    name: Fido
            |    food: Meat
            |    numLegs: 3""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCTrait](yaml) should have message "Line 3: Couldn't marshal class for co.blocke.scalajack.yaml.mapkeys.Bogus"
      }
      it("Bad Parameterized Case Class Key") {
        val yaml =
          """m: 
            | ?
            |   a: 5.5
            |   b: wow
            | : 
            |  a: 6
            |  b: zoom""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCParamClass[String, Int]](
          yaml
        ) should have message "Line 2: Cannot parse an Int from value"
      }
      it("Bad Parameterized Trait Key") {
        val yaml =
          """m: 
            | ?
            |   _hint: co.blocke.scalajack.yaml.mapkeys.ZThing
            |   a: 5
            |   b: wow
            | : 
            |  _hint: "co.blocke.scalajack.mapkeys.AThing"
            |  a: 6
            |  b: zoom""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCParamTrait[Int, String]](
          yaml
        ) should have message "Line 3: Couldn't marshal class for co.blocke.scalajack.yaml.mapkeys.ZThing"
      }
      it("Bad Option Key") {
        val yaml =
          """m:
            |  [1,2]: there""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCOption](yaml) should have message "Line 1: Expected a String here: +SEQ"
      }
      it("Bad Nested Collection Key") {
        val yaml =
          """m: 
            | ?
            |   - 
            |     a: b
            |   - 
            |     c: [1,2]
            | : 
            |   - 
            |     t: u
            |   - 
            |     x: y""".stripMargin
        the[ScalaJackError] thrownBy sj.read[SampleVCNested](yaml) should have message "Line 5: Expected a String here: +SEQ"
      }
    }
  }
}
