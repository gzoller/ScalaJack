package co.blocke.scalajack
package yaml
package mapkeys

import TestUtil._
import munit._
import munit.internal.console
import java.util.UUID

class ValueClassKeys() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Value class of BigDecimal") {
    describe(
      "-------------------------------------\n:  ValueClass Map Key Tests (YAML)  :\n-------------------------------------", Console.BLUE
    )
    describe("+++ Positive Primitive Tests +++")
    val inst = SampleVCBigDecimal(
      Map(
        VCBigDecimal(BigDecimal(12.34)) -> VCBigDecimal(BigDecimal(56.78))
      )
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12.34: 56.78
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCBigDecimal](yaml))
  }

  test("Value class of BigInt") {
    val inst =
      SampleVCBigInt(Map(VCBigInt(BigInt(12)) -> VCBigInt(BigInt(56))))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCBigInt](yaml))
  }

  test("Value class of Byte") {
    val inst = SampleVCByte(
      Map(VCByte(12.asInstanceOf[Byte]) -> VCByte(56.asInstanceOf[Byte]))
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCByte](yaml))
  }

  test("Value class of Boolean") {
    val inst       = SampleVCBoolean(Map(VCBoolean(true) -> VCBoolean(false)))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  true: false
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCBoolean](yaml))
  }

  test("Value class of Char") {
    val inst       = SampleVCChar(Map(VCChar('a') -> VCChar('A')))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  a: A
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCChar](yaml))
  }

  test("Value class of Double") {
    val inst       = SampleVCDouble(Map(VCDouble(12.34) -> VCDouble(56.78)))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12.34: 56.78
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCDouble](yaml))
  }

  test("Value class of Enumeration") {
    val inst = SampleVCEnumeration(
      Map(VCEnumeration(Food.Veggies) -> VCEnumeration(Food.Meat))
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  Veggies: Meat
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCEnumeration](yaml))
  }

  test("Value class of Float") {
    val inst       = SampleVCFloat(Map(VCFloat(12.34F) -> VCFloat(56.2F)))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12.34: 56.2
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCFloat](yaml))
  }

  test("Value class of Int") {
    val inst       = SampleVCInt(Map(VCInt(12) -> VCInt(56)))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCInt](yaml))
  }

  test("Value class of Long") {
    val inst       = SampleVCLong(Map(VCLong(12L) -> VCLong(56L)))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCLong](yaml))
  }

  test("Value class of Short") {
    val inst = SampleVCShort(
      Map(
        VCShort(12.asInstanceOf[Short]) -> VCShort(56.asInstanceOf[Short])
      )
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  12: 56
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCShort](yaml))
  }

  test("Value class of String") {
    val inst       = SampleVCString(Map(VCString("A") -> VCString("B")))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  A: B
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCString](yaml))
  }

  test("Value class of UUID") {
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
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCUUID](yaml))
  }

  test("Value class of List") {
    describe("+++ Positive Collection Tests +++")
    val inst =
      SampleVCList(Map(VCList(List(1, 2, 3)) -> VCList(List(4, 5, 6))))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? [1, 2, 3]
                        |  : [4, 5, 6]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCList](yaml))
  }

  test("Value class of List (empty List)") {
    val inst =
      SampleVCList(Map(VCList(List.empty[Int]) -> VCList(List.empty[Int])))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  []: []
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCList](yaml))
  }

  test("Value class of Map") {
    val inst       = SampleVCMap(Map(VCMap(Map(1 -> 2)) -> VCMap(Map(3 -> 4))))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? 1: 2
                        |  : 3: 4
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCMap](yaml))
  }

  test("Value class of Map (empty Map") {
    val inst = SampleVCMap(
      Map(VCMap(Map.empty[Int, Int]) -> VCMap(Map.empty[Int, Int]))
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  {}: {}
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCMap](yaml))
  }

  test("Value class of Tupple") {
    val inst = SampleVCTuple(
      Map(VCTuple((1, "one", true)) -> VCTuple((2, "two", false)))
    )
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? [1, one, true]
                        |  : [2, two, false]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCTuple](yaml))
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
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCClass](yaml))
  }

  test("Value class of Trait") {
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
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCTrait](yaml))
  }

  test("Value class of Parameterized Case Class") {
    val a          = AThing(5, "wow")
    val b          = AThing(6, "zoom")
    val inst       = SampleVCParamClass(Map(VCParamClass(a) -> VCParamClass(b)))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? a: 5
                        |    b: wow
                        |  : a: 6
                        |    b: zoom
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCParamClass[String, Int]](yaml))
  }

  test("Value class of Parameterized Trait") {
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
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCParamTrait[Int, String]](yaml))
  }

  test("Value class of Option") {
    val inst =
      SampleVCOption(Map(VCOption(Some("here")) -> VCOption(Some("there"))))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  here: there
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCOption](yaml))
  }

  test("Value class of nested collection") {
    val a          = VCNested(List(Map("a" -> "b"), Map("c" -> "d")))
    val b          = VCNested(List(Map("t" -> "u"), Map("x" -> "y")))
    val inst       = SampleVCNested(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - a: b
                        |    - c: d
                        |  : - t: u
                        |    - x: y
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleVCNested](yaml))
  }

  test("Bad BigDecimal Key") {
    describe("--- Negative Primitive Tests ---")
    val yaml = """m: 
                  | true: 56.78""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Number value here: =VAL :true"){
      sj.read[SampleVCBigDecimal](yaml)
    }
  }

  test("Bad BigInt Key") {
    val yaml = """m: 
                  | "12.5": 56""".stripMargin.asInstanceOf[YAML]
    interceptMessage[java.lang.NumberFormatException]("For input string: \"12.5\""){
      sj.read[SampleVCBigInt](yaml)
    }
  }

  test("Bad Boolean Key") {
    val yaml = """m: 
                  | 1: false""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Boolean value here: =VAL :1"){
      sj.read[SampleVCBoolean](yaml)
    }
  }

  test("Bad Char Key") {
    val yaml = """m: 
                  | null: A""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: A Char typed value cannot be null"){
      sj.read[SampleVCChar](yaml)
    }
  }

  test("Bad Double Key") {
    val yaml = """m: 
                  | false: 56.78""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Number value here: =VAL :false"){
      sj.read[SampleVCDouble](yaml)
    }
  }

  test("Bad Enumeration Key") {
    val yaml = """m: 
                  | Bogus: Meat""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: No value found in enumeration co.blocke.scalajack.yaml.mapkeys.Food$ for Bogus"){
      sj.read[SampleVCEnumeration](yaml)
    }
  }

  test("Bad Float Key") {
    val yaml = """m: 
                  | hey: 56.2""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Number value here: =VAL :hey"){
      sj.read[SampleVCFloat](yaml)
    }
  }

  test("Bad Int Key") {
    val yaml = """m: 
                  | "12.5": 56""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Cannot parse an Int from value"){
      sj.read[SampleVCInt](yaml)
    }
  }

  test("Bad Long Key") {
    val yaml = """m: 
                  | "12.5": 56""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Cannot parse an Long from value"){
      sj.read[SampleVCLong](yaml)
    }
  }

  test("Bad Short Key") {
    val yaml = """m: 
                  | "12.5": 56""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Cannot parse an Short from value"){
      sj.read[SampleVCShort](yaml)
    }
  }

  test("Bad String Key") {
    val yaml = """m: 
                  | [1,2]: B""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a String here: +SEQ"){
      sj.read[SampleVCString](yaml)
    }
  }

  test("Bad UUID Key") {
    val yaml = """m: 
                  | bogus: "54cab778-7b9e-4b07-9d37-87b97a011e55"""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Failed to create UUID value from parsed text bogus"){
      sj.read[SampleVCUUID](yaml)
    }
  }

  test("Bad List Key") {
    describe("--- Negative Collection Tests ---")
    val yaml =
      """m: 
        |  [1,2,"a"]: [4,5,6]""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a Number value here: =VAL \"a"){
      sj.read[SampleVCList](yaml)
    }
  }

  test("Bad Map Key") {
    val yaml =
      """m: 
        |  ?
        |    true: 2
        |  : 
        |    3: 4""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a Number value here: =VAL :true"){
      sj.read[SampleVCMap](yaml)
    }
  }

  test("Bad Tuple Key") {
    val yaml =
      """m: 
        |  ?
        |    [1,one,true,1]
        |  : [2,two,false]""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Expected a String here: -SEQ"){
      sj.read[SampleVCTuple](yaml)
    }
  }

  test("Bad Case Class Key") {
    describe("--- Negative Complex Tests ---")
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
        |    allDone: false""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 8: Class co.blocke.scalajack.yaml.mapkeys.SimpleClass missing required fields: name"){
      sj.read[SampleVCClass](yaml)
    }
  }

  test("Bad Trait Key") {
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
        |    numLegs: 3""".stripMargin.asInstanceOf[YAML]
    interceptMessage[java.lang.ClassNotFoundException]("co.blocke.scalajack.yaml.mapkeys.Bogus"){
      sj.read[SampleVCTrait](yaml)
    }
  }

  test("Bad Parameterized Case Class Key") {
    val yaml =
      """m: 
        | ?
        |   a: 5.5
        |   b: wow
        | : 
        |  a: 6
        |  b: zoom""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 2: Cannot parse an Int from value"){
      sj.read[SampleVCParamClass[String, Int]](yaml)
    }
  }

  test("Bad Parameterized Trait Key") {
    val yaml =
      """m: 
        | ?
        |   _hint: co.blocke.scalajack.yaml.mapkeys.ZThing
        |   a: 5
        |   b: wow
        | : 
        |  _hint: "co.blocke.scalajack.mapkeys.AThing"
        |  a: 6
        |  b: zoom""".stripMargin.asInstanceOf[YAML]
    interceptMessage[java.lang.ClassNotFoundException]("co.blocke.scalajack.yaml.mapkeys.ZThing"){
      sj.read[SampleVCParamTrait[Int, String]](yaml)
    }

  }

  test("Bad Option Key") {
    val yaml =
      """m:
        |  [1,2]: there""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 1: Expected a String here: +SEQ"){
      sj.read[SampleVCOption](yaml)
    }
  }

  test("Bad Nested Collection Key") {
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
        |     x: y""".stripMargin.asInstanceOf[YAML]
    interceptMessage[ScalaJackError]("Line 5: Expected a String here: +SEQ"){
      sj.read[SampleVCNested](yaml)
    }
  }