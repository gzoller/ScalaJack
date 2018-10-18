package co.blocke.scalajack
package json.test.noncanonical

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

class ClassPrimKeys() extends FunSpec with Matchers {

  val sj = ScalaJack().isCanonical(false)

  describe("------------------------------\n:  Class Noncanonical Tests  :\n------------------------------") {
    describe("+++ Positive Tests +++") {
      it("Simple (flat) class as key") {
        val a = SimpleClass("Larry", 32, true, "golf")
        val b = SimpleClass("Mike", 27, false, 125)
        val inst = SampleSimple(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{{"name":"Larry","age":32,"isOk":true,"favorite":"golf"}:{"name":"Mike","age":27,"isOk":false,"favorite":125}}}""") { js }
        assertResult(inst) {
          sj.read[SampleSimple](js)
        }
      }
      it("Complex class (having members that are classes) as key") {
        val a = SimpleClass("Larry", 32, true, "golf")
        val b = SimpleClass("Mike", 27, false, 125)
        val c1 = ComplexClass(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), a, true)
        val c2 = ComplexClass(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d"), b, false)
        val inst = SampleComplex(Map(c1 -> c2))
        val js = sj.render(inst)
        assertResult("""{"m":{{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","simple":{"name":"Larry","age":32,"isOk":true,"favorite":"golf"},"allDone":true}:{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}""") { js }
        assertResult(inst) {
          sj.read[SampleComplex](js)
        }
      }
      it("Simple (flat) trait as key") {
        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val inst = SamplePet(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.FishPet","name":"Flipper","food":"Veggies","waterTemp":74.33}:{"_hint":"co.blocke.scalajack.json.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePet](js)
        }
      }
      it("Complex trait (having members that are traits) as key") {
        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val c: Pet = CompoundPet("Legion", Food.Pellets, b)
        val inst = SamplePet(Map(c -> a))
        val js = sj.render(inst)
        assertResult("""{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.CompoundPet","name":"Legion","food":"Pellets","pet":{"_hint":"co.blocke.scalajack.json.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}:{"_hint":"co.blocke.scalajack.json.test.noncanonical.FishPet","name":"Flipper","food":"Veggies","waterTemp":74.33}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePet](js)
        }
      }
      it("Class having collections as members") {
        val a = PolyClass(Map("a" -> 1, "b" -> 2), List("one", "two"))
        val b = PolyClass(Map("x" -> 9, "y" -> 10), List("aye", "you"))
        val inst = SamplePolyClass(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{{"lookup":{"a":1,"b":2},"favs":["one","two"]}:{"lookup":{"x":9,"y":10},"favs":["aye","you"]}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePolyClass](js)
        }
      }
      it("Class having collections as members (empty collections") {
        val a = PolyClass(Map.empty[String, Int], List.empty[String])
        val b = PolyClass(Map.empty[String, Int], List.empty[String])
        val inst = SamplePolyClass(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{{"lookup":{},"favs":[]}:{"lookup":{},"favs":[]}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePolyClass](js)
        }
      }
      it("Custom trait hint field and value for key trait") {
        val petHintMod = StringMatchHintModifier(Map("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet]))
        val sj2 = ScalaJack().isCanonical(false)
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))

        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val inst = SamplePet(Map(a -> b))
        val js = sj2.render(inst)
        assertResult("""{"m":{{"kind":"BreathsWater","name":"Flipper","food":"Veggies","waterTemp":74.33}:{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}""") { js }
        assertResult(inst) {
          sj2.read[SamplePet](js)
        }
      }
      it("Custom trait hint field and value for key member's trait") {
        val petHintMod = StringMatchHintModifier(Map("BreathesWater" -> typeOf[FishPet], "BreathesAir" -> typeOf[DogPet]))
        val sj2 = ScalaJack().isCanonical(false)
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))

        val a: PetHolder = ShinyPetHolder("123 Main", FishPet("Flipper", Food.Veggies, 74.33))
        val b: PetHolder = ShinyPetHolder("210 North", DogPet("Fido", Food.Meat, 3))
        val inst = SampleShiny(Map(a -> b))
        val js = sj2.render(inst)
        assertResult("""{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.ShinyPetHolder","address":"123 Main","pet":{"kind":"BreathesWater","name":"Flipper","food":"Veggies","waterTemp":74.33}}:{"_hint":"co.blocke.scalajack.json.test.noncanonical.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathesAir","name":"Fido","food":"Meat","numLegs":3}}}}""") { js }
        assertResult(inst) {
          sj2.read[SampleShiny](js)
        }
      }
      it("Key value is a class having a noncanoncial map") {
        val a = NCKey(Map(0 -> false, 1 -> true), "truth")
        val b = NCKey(Map(1 -> false, 0 -> true), "lie")
        val inst = SampleNCKey(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{{"nc":{0:false,1:true},"name":"truth"}:{"nc":{1:false,0:true},"name":"lie"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleNCKey](js)
        }
      }
      it("Extra/unneeded fields in key's JSON harmlessly ignored") {
        val js = """{"m":{{"name":"Larry","bogus":false,"age":32,"isOk":true,"favorite":"golf"}:{"name":"Mike","age":27,"isOk":false,"favorite":125}}}"""
        val a = SimpleClass("Larry", 32, true, "golf")
        val b = SimpleClass("Mike", 27, false, 125)
        val inst = SampleSimple(Map(a -> b))
        assertResult(inst) {
          sj.read[SampleSimple](js)
        }
      }
    }

    describe("--- Negative Tests ---") {
      it("Bad (invalid--missing field) class json as map key") {
        val js = """{"m":{{"nameLarry","age":32,"favorite":"golf:{"name":"Mike","age":27,"isOk":false,"favorite":125}}}"""
        val msg = """DeserializationException(1 error):
                    |  [???] Exception was thrown: java.lang.IllegalArgumentException: Skipped ',', not ':' (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleSimple](js) should have message msg
      }
      it("Bad class json as map key (valid json, but wrong for given class)") {
        val js = """{"m":{{"name":"Larry","age":32,"favorite":"golf"}:{"name":"Mike","age":27,"isOk":false,"favorite":125}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.isOk] Required field missing (reported by: co.blocke.scalajack.typeadapter.BooleanDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleSimple](js) should have message msg
      }
      it("Bad json for member class") {
        val js = """{"m":{{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","simple":{"name":"Larry","isOk":true,"favorite":"golf"},"allDone":true}:{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.simple.age] Required field missing (reported by: co.blocke.scalajack.typeadapter.IntDeserializer)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SampleComplex](js) should have message msg
      }
      it("Bad (invalid) trait json as map key") {
        val js = """{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.FishPet":"Flipper","food":"Veggies","waterTemp":74.33}:{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
        val msg = """DeserializationException(1 error):
                    |  [???] Exception was thrown: java.lang.IllegalArgumentException: Malformed JSON: Expected either ',' or '}' at position 67 (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (missing hint) as map key") {
        val js = """{"m":{{"name":"Flipper","food":"Veggies","waterTemp":74.33}:{"_hint":"co.blocke.scalajack.json.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m] Exception was thrown: java.lang.IllegalStateException: Could not find type field named "_hint"
                    | (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (hint to unknown classs) as map key") {
        val js = """{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.Bogus","name":"Flipper","food":"Veggies","waterTemp":74.33}:{"_hint":"co.blocke.scalajack.json.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m] Exception was thrown: java.lang.ClassNotFoundException: Unable to find class named "co.blocke.scalajack.json.test.noncanonical.Bogus"
                    | (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad (invalid) trait json for member trait") {
        val js = """{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.CompoundPet","name":"Legion","food":"Pellets","pet":{"_hint":"co.blocke.scalajack.json.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}:{"_hint":"co.blocke.scalajack.json.test.noncanonical.FishPet","name">"Flipper","food":"Veggies","waterTemp":74.33}}}"""
        val msg = """DeserializationException(1 error):
                    |  [???] Exception was thrown: java.lang.IllegalArgumentException: Skipped '>', not ':' (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (missing hint) for member trait") {
        val js = """{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.CompoundPet","name":"Legion","food":"Pellets","pet":{"_hint":"co.blocke.scalajack.json.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}:{"name":"Flipper","food":"Veggies","waterTemp":74.33}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m] Exception was thrown: java.lang.IllegalStateException: Could not find type field named "_hint"
                    | (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (hint to unknown classs) for member trait") {
        val js = """{"m":{{"_hint":"co.blocke.scalajack.json.test.noncanonical.CompoundPet","name":"Legion","food":"Pellets","pet":{"_hint":"co.blocke.scalajack.json.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}:{"_hint":"co.blocke.scalajack.json.test.noncanonical.Bogus","name":"Flipper","food":"Veggies","waterTemp":74.33}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m] Exception was thrown: java.lang.ClassNotFoundException: Unable to find class named "co.blocke.scalajack.json.test.noncanonical.Bogus"
                    | (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad collection value in map key class having collections") {
        val js = """{"m":{{"lookup":{"a":true,"b":2},"favs":["one","two"]}:{"lookup":{"x":9,"y":10},"favs":["aye","you"]}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.lookup.a] Expected a JSON int, not JBool(true) (reported by: co.blocke.scalajack.typeadapter.IntDeserializer)""".stripMargin
      }
      it("Bad custom hint value for map key trait") {
        val petHintMod = StringMatchHintModifier(Map("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet]))
        val sj = ScalaJack().isCanonical(false)
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))
        val js = """{"m":{{"_hint":"co.blocke.scalajack.test.noncanonical.ShinyPetHolder","address":"123 Main","pet":{"sort":"BreathsWater","name":"Flipper","food":"Veggies","waterTemp":74.33}}:{"_hint":"co.blocke.scalajack.test.noncanonical.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m] Exception was thrown: java.lang.IllegalStateException: Could not find type field named "kind"
                    | (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad custom hint value for key member's trait") {
        val petHintMod = StringMatchHintModifier(Map("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet]))
        val sj = ScalaJack().isCanonical(false)
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))
        val js = """{"m":{{"_hint":"co.blocke.scalajack.test.noncanonical.ShinyPetHolder","address":"123 Main","pet":{"kind":"BreathsLava","name":"Flipper","food":"Veggies","waterTemp":74.33}}:{"_hint":"co.blocke.scalajack.test.noncanonical.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}"""
        val msg = """DeserializationException(1 error):
                    |  [$.m] Exception was thrown: java.lang.IllegalStateException: Could not find type field named "kind"
                    | (reported by: unknown)""".stripMargin
        the[DeserializationException] thrownBy sj.read[SamplePet](js) should have message msg
      }
    }
  }
}
