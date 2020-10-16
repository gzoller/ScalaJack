package co.blocke.scalajack
package yaml
package mapkeys

import TestUtil._
import munit._
import munit.internal.console
import java.util.UUID
import model.StringMatchHintModifier
import co.blocke.scala_reflection.RType

class ClassPrimKeys() extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("Simple (flat) class as key") {
    describe(
      "--------------------------------\n:  Class Map Key Tests (YAML)  :\n--------------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++")
    val a          = SimpleClass("Larry", 32, isOk = true, "golf")
    val b          = SimpleClass("Mike", 27, isOk = false, 125)
    val inst       = SampleSimple(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? name: Larry
                        |    age: 32
                        |    isOk: true
                        |    favorite: golf
                        |  : name: Mike
                        |    age: 27
                        |    isOk: false
                        |    favorite: 125
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleSimple](yaml))
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
    val inst       = SampleComplex(Map(c1 -> c2))
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
    assertEquals(inst, sj.read[SampleComplex](yaml))
  }

  test("Simple (flat) trait as key") {
    val a: Pet     = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet     = DogPet("Fido", Food.Meat, 3)
    val inst       = SamplePet(Map(a -> b))
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
    assertEquals(inst, sj.read[SamplePet](yaml))
  }

  test("Complex trait (having members that are traits) as key") {
    val a: Pet     = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet     = DogPet("Fido", Food.Meat, 3)
    val c: Pet     = CompoundPet("Legion", Food.Pellets, b)
    val inst       = SamplePet(Map(c -> a))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? _hint: co.blocke.scalajack.yaml.mapkeys.CompoundPet
                        |    name: Legion
                        |    food: Pellets
                        |    pet:
                        |      _hint: co.blocke.scalajack.yaml.mapkeys.DogPet
                        |      name: Fido
                        |      food: Meat
                        |      numLegs: 3
                        |  : _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |    name: Flipper
                        |    food: Veggies
                        |    waterTemp: 74.33
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SamplePet](yaml))
  }

  test(
    "Complex trait (having members that are traits) as key where trait member is null"
  ) {
    val a: Pet     = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet     = null.asInstanceOf[Pet] // DogPet("Fido", Food.Meat, 3)
    val c: Pet     = CompoundPet("Legion", Food.Pellets, b)
    val inst       = SamplePet(Map(c -> a))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? _hint: co.blocke.scalajack.yaml.mapkeys.CompoundPet
                        |    name: Legion
                        |    food: Pellets
                        |    pet: null
                        |  : _hint: co.blocke.scalajack.yaml.mapkeys.FishPet
                        |    name: Flipper
                        |    food: Veggies
                        |    waterTemp: 74.33
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SamplePet](yaml))
  }

  test("Class having collections as members") {
    val a          = PolyClass(Map("a" -> 1, "b" -> 2), List("one", "two"))
    val b          = PolyClass(Map("x" -> 9, "y" -> 10), List("aye", "you"))
    val inst       = SamplePolyClass(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? lookup:
                        |      a: 1
                        |      b: 2
                        |    favs: [one, two]
                        |  : lookup:
                        |      x: 9
                        |      y: 10
                        |    favs: [aye, you]
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SamplePolyClass](yaml))
  }

  test("Class having collections as members (empty collections") {
    val a          = PolyClass(Map.empty[String, Int], List.empty[String])
    val b          = PolyClass(Map.empty[String, Int], List.empty[String])
    val inst       = SamplePolyClass(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? lookup: {}
                        |    favs: []
                        |  : lookup: {}
                        |    favs: []
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SamplePolyClass](yaml))
  }

  test("Custom trait hint field and value for key trait") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.yaml.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.yaml.mapkeys.DogPet")
    )
    val sj2 = sj
      .withHints((RType.of[Pet] -> "kind"))
      .withHintModifiers((RType.of[Pet] -> petHintMod))

    val a: Pet     = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet     = DogPet("Fido", Food.Meat, 3)
    val inst       = SamplePet(Map(a -> b))
    val yaml       = sj2.render(inst)
    val comparison = """m:
                        |  ? kind: BreathsWater
                        |    name: Flipper
                        |    food: Veggies
                        |    waterTemp: 74.33
                        |  : kind: BreathsAir
                        |    name: Fido
                        |    food: Meat
                        |    numLegs: 3
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj2.read[SamplePet](yaml))
  }

  test("Custom trait hint field and value for key member's trait") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.yaml.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.yaml.mapkeys.DogPet")
    )
    val sj2 = sj
      .withHints((RType.of[Pet] -> "kind"))
      .withHintModifiers((RType.of[Pet] -> petHintMod))

    val a: PetHolder =
      ShinyPetHolder("123 Main", FishPet("Flipper", Food.Veggies, 74.33))
    val b: PetHolder =
      ShinyPetHolder("210 North", DogPet("Fido", Food.Meat, 3))
    val inst       = SampleShiny(Map(a -> b))
    val yaml       = sj2.render(inst)
    val comparison = """m:
                        |  ? _hint: co.blocke.scalajack.yaml.mapkeys.ShinyPetHolder
                        |    address: 123 Main
                        |    pet:
                        |      kind: BreathsWater
                        |      name: Flipper
                        |      food: Veggies
                        |      waterTemp: 74.33
                        |  : _hint: co.blocke.scalajack.yaml.mapkeys.ShinyPetHolder
                        |    address: 210 North
                        |    pet:
                        |      kind: BreathsAir
                        |      name: Fido
                        |      food: Meat
                        |      numLegs: 3
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj2.read[SampleShiny](yaml))
  }

  test("Key value is a class having a noncanoncial map") {
    val a          = NCKey(Map(0 -> false, 1 -> true), "truth")
    val b          = NCKey(Map(1 -> false, 0 -> true), "lie")
    val inst       = SampleNCKey(Map(a -> b))
    val yaml       = sj.render(inst)
    val comparison = """m:
                        |  ? nc:
                        |      0: false
                        |      1: true
                        |    name: truth
                        |  : nc:
                        |      1: false
                        |      0: true
                        |    name: lie
                        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(comparison, yaml )
    assertEquals(inst, sj.read[SampleNCKey](yaml))
  }

  test("Extra/unneeded fields in key's JSON harmlessly ignored") {
    val yaml =
      """m:
        |  ?
        |    name: Larry
        |    bogus: false
        |    age: 32
        |    isOk: true
        |    favorite: golf
        |  :
        |    name: Mike
        |    age: 27
        |    isOk: false
        |    favorite: 125
        |""".stripMargin.asInstanceOf[YAML]
    val a    = SimpleClass("Larry", 32, isOk = true, "golf")
    val b    = SimpleClass("Mike", 27, isOk = false, 125)
    val inst = SampleSimple(Map(a -> b))
    assertEquals(inst, sj.read[SampleSimple](yaml))
  }