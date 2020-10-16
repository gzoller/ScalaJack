package co.blocke.scalajack
package yaml
package mapkeys

import TestUtil._
import munit._
import munit.internal.console
import java.util.UUID
import model.StringMatchHintModifier
import co.blocke.scala_reflection.RType

class TupleCollKeys() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Tuple as key") {
    describe(
      "--------------------------------\n:  Tuple Map Key Tests (YAML)  :\n--------------------------------", Console.BLUE
    ) 
    val a          = (2, "Blather", 'Q')
    val b          = ("Foo", true)
    val inst       = SampleTuple(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? [2, Blather, Q]
                        |  : [Foo, true]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTuple](yaml))
  }

  test("Tuple as key, null tuple as value") {
    val a                    = (2, "Blather", 'Q')
    val b: (String, Boolean) = null
    val inst                 = SampleTuple(Map(a -> b))
    val yaml                 = sj.render(inst)
    val comparison           = """m:
                        |  ? [2, Blather, Q]
                        |  : null
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTuple](yaml))
  }

  test("Tuple of Lists as key") {
    val a          = (List("one", "two", "three"), List(1, 2, 3))
    val b          = (List("four", "five", "six"), List(4, 5, 6))
    val inst       = SampleTupleList(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - [one, two, three]
                        |    - [1, 2, 3]
                        |  : - [four, five, six]
                        |    - [4, 5, 6]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleList](yaml))
  }

  test("Tuple of Maps as key") {
    val a          = (Map("one" -> 1), Map(2 -> "two"))
    val b          = (Map("three" -> 3), Map(4 -> "four"))
    val inst       = SampleTupleMap(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - one: 1
                        |    - 2: two
                        |  : - three: 3
                        |    - 4: four
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleMap](yaml))
  }

  test("Tuple of Tuples as key") {
    val a          = (("yes", true), (1, 0.25))
    val b          = (("no", false), (2, 0.5))
    val inst       = SampleTupleTuple(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - [yes, true]
                        |    - [1, 0.25]
                        |  : - [no, false]
                        |    - [2, 0.5]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleTuple](yaml))
  }

  test("Tuple of Case Class as key") {
    val a          = (SampleChar(Map('a' -> 'A')), SampleInt(Map(1 -> 2)))
    val b          = (SampleChar(Map('z' -> 'Z')), SampleInt(Map(99 -> 100)))
    val inst       = SampleTupleClass(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - m:
                        |        a: A
                        |    - m:
                        |        1: 2
                        |  : - m:
                        |        z: Z
                        |    - m:
                        |        99: 100
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleClass](yaml))
  }

  test("Tuple of Trait as key") {
    val a = (DogPet("Fido", Food.Meat, 4), FishPet("Jaws", Food.Meat, 69.8))
    val b =
      (DogPet("Chey", Food.Meat, 3), FishPet("Flipper", Food.Seeds, 80.1))
    val inst       = SampleTupleTrait(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                        |      name: Fido
                        |      food: Meat
                        |      numLegs: 4
                        |    - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |      name: Jaws
                        |      food: Meat
                        |      waterTemp: 69.8
                        |  : - _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                        |      name: Chey
                        |      food: Meat
                        |      numLegs: 3
                        |    - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |      name: Flipper
                        |      food: Seeds
                        |      waterTemp: 80.1
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleTrait](yaml))
  }

  test("Tuple of Any as key") {
    val a = (DogPet("Fido", Food.Meat, 4), FishPet("Jaws", Food.Meat, 69.8))
    val b =
      (DogPet("Chey", Food.Meat, 3), FishPet("Flipper", Food.Seeds, 80.1))
    val inst       = SampleTupleTrait(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                        |      name: Fido
                        |      food: Meat
                        |      numLegs: 4
                        |    - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |      name: Jaws
                        |      food: Meat
                        |      waterTemp: 69.8
                        |  : - _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                        |      name: Chey
                        |      food: Meat
                        |      numLegs: 3
                        |    - _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |      name: Flipper
                        |      food: Seeds
                        |      waterTemp: 80.1
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleTrait](yaml))
  }

  test("Tuple of Optional as key") {
    val a          = (Some(5), Some("Fred"))
    val b          = (None, Some(Food.Meat))
    val inst       = SampleTupleOptional(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - 5
                        |    - Fred
                        |  : - null
                        |    - Meat
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleOptional](yaml))
  }

  test("Tuple of ValueClass as key") {
    val a          = (VCChar('a'), VCChar('A'))
    val b          = (VCChar('z'), VCChar('Z'))
    val inst       = SampleTupleVC(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - a
                        |    - A
                        |  : - z
                        |    - Z
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleVC](yaml))
  }

  test("Complex class (having members that are classes) as key") {
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
    val t1         = (c1, c2)
    val t2         = (c2, c1)
    val inst       = SampleTupleComplex(Map(t1 -> t2))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - id: 1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c
                        |      simple:
                        |        name: Larry
                        |        age: 32
                        |        isOk: true
                        |        favorite: golf
                        |      allDone: true
                        |    - id: 1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d
                        |      simple:
                        |        name: Mike
                        |        age: 27
                        |        isOk: false
                        |        favorite: 125
                        |      allDone: false
                        |  : - id: 1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d
                        |      simple:
                        |        name: Mike
                        |        age: 27
                        |        isOk: false
                        |        favorite: 125
                        |      allDone: false
                        |    - id: 1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c
                        |      simple:
                        |        name: Larry
                        |        age: 32
                        |        isOk: true
                        |        favorite: golf
                        |      allDone: true
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTupleComplex](yaml))
  }

  test("Class having collections as members") {
    val a          = PolyClass(Map("a" -> 1, "b" -> 2), List("one", "two"))
    val b          = PolyClass(Map("x" -> 9, "y" -> 10), List("aye", "you"))
    val t1         = (a, b)
    val t2         = (b, a)
    val inst       = SampleTuplePolyClass(Map(t1 -> t2))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? - lookup:
                        |        a: 1
                        |        b: 2
                        |      favs: [one, two]
                        |    - lookup:
                        |        x: 9
                        |        y: 10
                        |      favs: [aye, you]
                        |  : - lookup:
                        |        x: 9
                        |        y: 10
                        |      favs: [aye, you]
                        |    - lookup:
                        |        a: 1
                        |        b: 2
                        |      favs: [one, two]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleTuplePolyClass](yaml))
  }

  test("Class having collections as members (empty collections") {
    val a          = PolyClass(Map.empty[String, Int], List.empty[String])
    val b          = PolyClass(Map.empty[String, Int], List.empty[String])
    val t1         = (a, b)
    val t2         = (b, a)
    val inst       = Map(t1 -> t2)
    val yaml       = sj.render(inst)
    val comparison = """? - lookup: {}
                        |    favs: []
                        |  - lookup: {}
                        |    favs: []
                        |: - lookup: {}
                        |    favs: []
                        |  - lookup: {}
                        |    favs: []
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[(PolyClass, PolyClass), (PolyClass, PolyClass)]](yaml))
  }

  test("Custom trait hint field and value for key trait") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.yaml.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.yaml.mapkeys.DogPet")
    )
    val sj2 = sj
      .withHints(RType.of[Pet] -> "kind")
      .withHintModifiers(RType.of[Pet] -> petHintMod)
    val a: Pet     = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet     = DogPet("Fido", Food.Meat, 3)
    val t1         = (a, b)
    val t2         = (b, a)
    val inst       = Map(t1 -> t2)
    val yaml       = sj2.render(inst)
    val comparison = """? - kind: BreathsWater
                        |    name: Flipper
                        |    food: Veggies
                        |    waterTemp: 74.33
                        |  - kind: BreathsAir
                        |    name: Fido
                        |    food: Meat
                        |    numLegs: 3
                        |: - kind: BreathsAir
                        |    name: Fido
                        |    food: Meat
                        |    numLegs: 3
                        |  - kind: BreathsWater
                        |    name: Flipper
                        |    food: Veggies
                        |    waterTemp: 74.33
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj2.read[Map[(Pet, Pet), (Pet, Pet)]](yaml))
  }

  test("Custom trait hint field and value for key member's trait") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.yaml.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.yaml.mapkeys.DogPet")
    )
    val sj2 = sj
      .withHints(RType.of[Pet] -> "kind")
      .withHintModifiers(RType.of[Pet] -> petHintMod)

    val a: PetHolder =
      ShinyPetHolder("123 Main", FishPet("Flipper", Food.Veggies, 74.33))
    val b: PetHolder =
      ShinyPetHolder("210 North", DogPet("Fido", Food.Meat, 3))
    val t1         = (a, b)
    val t2         = (b, a)
    val inst       = Map(t1 -> t2)
    val yaml       = sj2.render(inst)
    val comparison = """? - _hint: co.blocke.scalajack.yaml.mapkeys.ShinyPetHolder
                        |    address: 123 Main
                        |    pet:
                        |      kind: BreathsWater
                        |      name: Flipper
                        |      food: Veggies
                        |      waterTemp: 74.33
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.ShinyPetHolder
                        |    address: 210 North
                        |    pet:
                        |      kind: BreathsAir
                        |      name: Fido
                        |      food: Meat
                        |      numLegs: 3
                        |: - _hint: co.blocke.scalajack.yaml.mapkeys.ShinyPetHolder
                        |    address: 210 North
                        |    pet:
                        |      kind: BreathsAir
                        |      name: Fido
                        |      food: Meat
                        |      numLegs: 3
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.ShinyPetHolder
                        |    address: 123 Main
                        |    pet:
                        |      kind: BreathsWater
                        |      name: Flipper
                        |      food: Veggies
                        |      waterTemp: 74.33
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj2.read[Map[(PetHolder, PetHolder), (PetHolder, PetHolder)]](yaml))
  }

  test("Parameterized class") {
    val t1         = (AThing("wow", 4), AThing("boom", 1))
    val t2         = (AThing("yep", 3), AThing("yikes", 11))
    val inst       = Map(t1 -> t2)
    val yaml       = sj.render(inst)
    val comparison = """? - a: wow
                        |    b: 4
                        |  - a: boom
                        |    b: 1
                        |: - a: yep
                        |    b: 3
                        |  - a: yikes
                        |    b: 11
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[(AThing[Int, String], AThing[Int, String]), (AThing[Int, String], AThing[Int, String])]](yaml))
  }

  test("Parameterized trait") {
    val t1: (Thing[String, Int], Thing[String, Int]) =
      (AThing("wow", 4), AThing("boom", 1))
    val t2: (Thing[String, Int], Thing[String, Int]) =
      (AThing("yep", 3), AThing("yikes", 11))
    val inst       = Map(t1 -> t2)
    val yaml       = sj.render(inst)
    val comparison = """? - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: wow
                        |    b: 4
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: boom
                        |    b: 1
                        |: - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: yep
                        |    b: 3
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: yikes
                        |    b: 11
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[(Thing[String, Int], Thing[String, Int]), (Thing[String, Int], Thing[String, Int])]](yaml))
  }

  test("Parameterized trait having parameterized trait members") {
    val t1: (Thing[String, Part[Double]], Thing[String, Part[Double]]) =
      (AThing("wow", APart(1.2)), AThing("boom", APart(2.3)))
    val t2: (Thing[String, Part[Double]], Thing[String, Part[Double]]) =
      (AThing("yep", APart(4.5)), AThing("yikes", APart(6.7)))
    val inst       = Map(t1 -> t2)
    val yaml       = sj.render(inst)
    val comparison = """? - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: wow
                        |    b:
                        |      _hint: co.blocke.scalajack.yaml.mapkeys.APart
                        |      p: 1.2
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: boom
                        |    b:
                        |      _hint: co.blocke.scalajack.yaml.mapkeys.APart
                        |      p: 2.3
                        |: - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: yep
                        |    b:
                        |      _hint: co.blocke.scalajack.yaml.mapkeys.APart
                        |      p: 4.5
                        |  - _hint: co.blocke.scalajack.yaml.mapkeys.AThing
                        |    a: yikes
                        |    b:
                        |      _hint: co.blocke.scalajack.yaml.mapkeys.APart
                        |      p: 6.7
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[Map[(Thing[String, Part[Double]], Thing[String, Part[Double]]), (Thing[String, Part[Double]], Thing[String, Part[Double]])]](yaml))
  }