package co.blocke.scalajack
package yaml
package mapkeys

import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import java.util.UUID
import scala.reflect.runtime.universe.typeOf
import model.StringMatchHintModifier

class ClassPrimKeys() extends AnyFunSpec with Matchers {

  val sj = ScalaJack(YamlFlavor())

  describe(
    "--------------------------------\n:  Class Map Key Tests (YAML)  :\n--------------------------------"
  ) {
    describe("+++ Positive Tests +++") {
      it("Simple (flat) class as key") {
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleSimple](yaml)
        }
      }
      it("Complex class (having members that are classes) as key") {
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleComplex](yaml)
        }
      }
      it("Simple (flat) trait as key") {
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SamplePet](yaml)
        }
      }
      it("Complex trait (having members that are traits) as key") {
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SamplePet](yaml)
        }
      }
      it(
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SamplePet](yaml)
        }
      }
      it("Class having collections as members") {
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SamplePolyClass](yaml)
        }
      }
      it("Class having collections as members (empty collections") {
        val a          = PolyClass(Map.empty[String, Int], List.empty[String])
        val b          = PolyClass(Map.empty[String, Int], List.empty[String])
        val inst       = SamplePolyClass(Map(a -> b))
        val yaml       = sj.render(inst)
        val comparison = """m:
                           |  ? lookup: {}
                           |    favs: []
                           |  : lookup: {}
                           |    favs: []
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SamplePolyClass](yaml)
        }
      }
      it("Custom trait hint field and value for key trait") {
        val petHintMod = StringMatchHintModifier(
          Map("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet])
        )
        val sj2 = sj
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))

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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj2.read[SamplePet](yaml)
        }
      }
      it("Custom trait hint field and value for key member's trait") {
        val petHintMod = StringMatchHintModifier(
          Map("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet])
        )
        val sj2 = sj
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))

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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj2.read[SampleShiny](yaml)
        }
      }
      it("Key value is a class having a noncanoncial map") {
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
                           |""".stripMargin
        assertResult(comparison) { yaml }
        assertResult(inst) {
          sj.read[SampleNCKey](yaml)
        }
      }
      it("Extra/unneeded fields in key's JSON harmlessly ignored") {
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
            |""".stripMargin
        val a    = SimpleClass("Larry", 32, isOk = true, "golf")
        val b    = SimpleClass("Mike", 27, isOk = false, 125)
        val inst = SampleSimple(Map(a -> b))
        assertResult(inst) {
          sj.read[SampleSimple](yaml)
        }
      }
    }
  }
}
