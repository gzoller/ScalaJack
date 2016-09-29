package co.blocke.scalajack
package test
package noncanonical

import org.scalatest.{ FunSpec, Matchers }
import scala.reflect.runtime.universe.typeOf

class TupleCollPrimKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------------------\n:  Tuple Noncanonical Tests  :\n------------------------------") {
    describe("+++ Positive Tests +++") {
      it("Tuple as key") {
        val a = (2, "Blather", 'Q')
        val b = ("Foo", true)
        val inst = SampleTuple(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[2,\"Blather\",\"Q\"]":["Foo",true]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTuple](js)
        }
      }
      it("Tuple of Lists as key") {
        val a = (List("one", "two", "three"), List(1, 2, 3))
        val b = (List("four", "five", "six"), List(4, 5, 6))
        val inst = SampleTupleList(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[[\"one\",\"two\",\"three\"],[1,2,3]]":[["four","five","six"],[4,5,6]]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTupleList](js)
        }
      }
      it("Tuple of Maps as key") {
        val a = (Map("one" -> 1), Map(2 -> "two"))
        val b = (Map("three" -> 3), Map(4 -> "four"))
        val inst = SampleTupleMap(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[{\"one\":1},{\"2\":\"two\"}]":[{"three":3},{"4":"four"}]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTupleMap](js)
        }
      }
      it("Tuple of Tuples as key") {
        val a = (("yes", true), (1, 0.25))
        val b = (("no", false), (2, 0.5))
        val inst = SampleTupleTuple(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[[\"yes\",true],[1,0.25]]":[["no",false],[2,0.5]]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTupleTuple](js)
        }
      }
      it("Tuple of Case Class as key") {
        val a = (SampleChar(Map('a' -> 'A')), SampleInt(Map(1 -> 2)))
        val b = (SampleChar(Map('z' -> 'Z')), SampleInt(Map(99 -> 100)))
        val inst = SampleTupleClass(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[{\"m\":{\"a\":\"A\"}},{\"m\":{\"1\":2}}]":[{"m":{"z":"Z"}},{"m":{"99":100}}]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTupleClass](js)
        }
      }
      it("Tuple of Trait as key") {
        val a = (DogPet("Fido", Food.Meat, 4), FishPet("Jaws", Food.Meat, 69.8))
        val b = (DogPet("Chey", Food.Meat, 3), FishPet("Flipper", Food.Seeds, 80.1))
        val inst = SampleTupleTrait(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[{\"_hint\":\"co.blocke.scalajack.test.noncanonical.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4},{\"_hint\":\"co.blocke.scalajack.test.noncanonical.FishPet\",\"name\":\"Jaws\",\"food\":\"Meat\",\"waterTemp\":69.8}]":[{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Chey","food":"Meat","numLegs":3},{"_hint":"co.blocke.scalajack.test.noncanonical.FishPet","name":"Flipper","food":"Seeds","waterTemp":80.1}]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTupleTrait](js)
        }
      }
      it("Tuple of Any as key") {
        val a = (DogPet("Fido", Food.Meat, 4), FishPet("Jaws", Food.Meat, 69.8))
        val b = (DogPet("Chey", Food.Meat, 3), FishPet("Flipper", Food.Seeds, 80.1))
        val inst = SampleTupleTrait(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[{\"_hint\":\"co.blocke.scalajack.test.noncanonical.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4},{\"_hint\":\"co.blocke.scalajack.test.noncanonical.FishPet\",\"name\":\"Jaws\",\"food\":\"Meat\",\"waterTemp\":69.8}]":[{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Chey","food":"Meat","numLegs":3},{"_hint":"co.blocke.scalajack.test.noncanonical.FishPet","name":"Flipper","food":"Seeds","waterTemp":80.1}]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTupleTrait](js)
        }
      }
      it("Tuple of parameterized class as key") {
        (pending)
      }
      it("Tuple of parameterized trait as key") {
        (pending)
      }
      it("Tuple of Optional as key") {
        (pending)
        // val a = (Some(5), Some("Fred"))
        // val b = (None, Some(Food.Meat))
        // val inst = SampleTupleOptional(Map(a -> b))
        // val js = sj.render(inst)
        // println(js)
        // assertResult("""{"m":{"[{\"_hint\":\"co.blocke.scalajack.test.noncanonical.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4},{\"_hint\":\"co.blocke.scalajack.test.noncanonical.FishPet\",\"name\":\"Jaws\",\"food\":\"Meat\",\"waterTemp\":69.8}]":[{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Chey","food":"Meat","numLegs":3},{"_hint":"co.blocke.scalajack.test.noncanonical.FishPet","name":"Flipper","food":"Seeds","waterTemp":80.1}]}}""") { js }
        // assertResult(inst) {
        //   sj.read[SampleTupleOptional](js)
        // }
      }
      it("Tuple of ValueClass as key") {
        val a = (VCChar('a'), VCChar('A'))
        val b = (VCChar('z'), VCChar('Z'))
        val inst = SampleTupleVC(Map(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"[\"a\",\"A\"]":["z","Z"]}}""") { js }
        assertResult(inst) {
          sj.read[SampleTupleVC](js)
        }
      }

      /*
      it("Complex class (having members that are classes) as key") {
        val a = SimpleClass("Larry", 32, true, "golf")
        val b = SimpleClass("Mike", 27, false, 125)
        val c1 = ComplexClass(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), a, true)
        val c2 = ComplexClass(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d"), b, false)
        val inst = SampleComplex(Tuple(c1 -> c2))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"name\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}""") { js }
        assertResult(inst) {
          sj.read[SampleComplex](js)
        }
      }
      it("Simple (flat) trait as key") {
        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val inst = SamplePet(Tuple(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.FishPet\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePet](js)
        }
      }
      it("Complex trait (having members that are traits) as key") {
        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val c: Pet = CompoundPet("Legion", Food.Pellets, b)
        val inst = SamplePet(Tuple(c -> a))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.test.noncanonical.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"_hint":"co.blocke.scalajack.test.noncanonical.FishPet","name":"Flipper","food":"Veggies","waterTemp":74.33}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePet](js)
        }
      }
      it("Class having collections as members") {
        val a = PolyClass(Tuple("a" -> 1, "b" -> 2), List("one", "two"))
        val b = PolyClass(Tuple("x" -> 9, "y" -> 10), List("aye", "you"))
        val inst = SamplePolyClass(Tuple(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"lookup\":{\"a\":1,\"b\":2},\"favs\":[\"one\",\"two\"]}":{"lookup":{"x":9,"y":10},"favs":["aye","you"]}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePolyClass](js)
        }
      }
      it("Class having collections as members (empty collections") {
        val a = PolyClass(Tuple.empty[String, Int], List.empty[String])
        val b = PolyClass(Tuple.empty[String, Int], List.empty[String])
        val inst = SamplePolyClass(Tuple(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"lookup\":{},\"favs\":[]}":{"lookup":{},"favs":[]}}}""") { js }
        assertResult(inst) {
          sj.read[SamplePolyClass](js)
        }
      }
      it("Custom trait hint field and value for key trait") {
        val petHintMod = StringMatchHintModifier(Tuple("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet]))
        val sj2 = ScalaJack()
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))

        val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
        val b: Pet = DogPet("Fido", Food.Meat, 3)
        val inst = SamplePet(Tuple(a -> b))
        val js = sj2.render(inst)
        assertResult("""{"m":{"{\"kind\":\"BreathsWater\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}""") { js }
        assertResult(inst) {
          sj2.read[SamplePet](js)
        }
      }
      it("Custom trait hint field and value for key member's trait") {
        val petHintMod = StringMatchHintModifier(Tuple("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet]))
        val sj2 = ScalaJack()
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))

        val a: PetHolder = ShinyPetHolder("123 Main", FishPet("Flipper", Food.Veggies, 74.33))
        val b: PetHolder = ShinyPetHolder("210 North", DogPet("Fido", Food.Meat, 3))
        val inst = SampleShiny(Tuple(a -> b))
        val js = sj2.render(inst)
        assertResult("""{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.ShinyPetHolder\",\"address\":\"123 Main\",\"pet\":{\"kind\":\"BreathsWater\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"_hint":"co.blocke.scalajack.test.noncanonical.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}""") { js }
        assertResult(inst) {
          sj2.read[SampleShiny](js)
        }
      }
      it("Key value is a class having a noncanoncial map") {
        val a = NCKey(Tuple(0 -> false, 1 -> true), "truth")
        val b = NCKey(Tuple(1 -> false, 0 -> true), "lie")
        val inst = SampleNCKey(Tuple(a -> b))
        val js = sj.render(inst)
        assertResult("""{"m":{"{\"nc\":{\"0\":false,\"1\":true},\"name\":\"truth\"}":{"nc":{"1":false,"0":true},"name":"lie"}}}""") { js }
        assertResult(inst) {
          sj.read[SampleNCKey](js)
        }
      }
      it("Parameterized class") {
        (pending)
      }
      it("Parameterized trait") {
        (pending)
      }
      it("Parameterized trait having parameterized trait members") {
        (pending)
      }
      */
    }
    describe("--- Negative Tests ---") {
      /*
      it("Bad (invalid--missing field) class json as map key") {
        val js = """{"m":{"{\"nameLarry\",\"age\":32,\"favorite\":\"golf\"":{"name":"Mike","age":27,"isOk":false,"favorite":125}}}"""
        val msg = """Character out of place. ',' not expected here.
          |{"nameLarry","age":32,"favorite":"golf"
          |------------^ Extracted from source here:
          |{"m":{"{\"nameLarry\",\"age\":32,\"favorite\":\"golf\"":{
          |-------^""".stripMargin
        the[java.lang.IllegalArgumentException] thrownBy sj.read[SampleSimple](js) should have message msg
      }
      it("Bad class json as map key (valid json, but wrong for given class)") {
        val js = """{"m":{"{\"name\":\"Larry\",\"age\":32,\"favorite\":\"golf\"}":{"name":"Mike","age":27,"isOk":false,"favorite":125}}}"""
        val msg = """Required field isOk in class co.blocke.scalajack.test.noncanonical.SimpleClass is missing from input and has no specified default value
          |{"m":{"{\"name\":\"Larry\",\"age\":32,\"favorite\":\"golf
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleSimple](js) should have message msg
      }
      it("Bad json for member class") {
        val js = """{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"name\":\"Larry\",\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}"""
        val msg = """Required field age in class co.blocke.scalajack.test.noncanonical.SimpleClass is missing from input and has no specified default value
          |{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SampleComplex](js) should have message msg
      }
      it("Bad (invalid) trait json as map key") {
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.FishPet\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
        val msg = """Character out of place. ':' not expected here.
          |t":"co.blocke.scalajack.test.noncanonical.FishPet":"Flipper","food":"Veggies","waterTemp":74.33}
          |--------------------------------------------------^ Extracted from source here:
          |{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical
          |-------^""".stripMargin
        the[java.lang.IllegalArgumentException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (missing hint) as map key") {
        val js = """{"m":{"{\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
        val msg = """Could not find type field named "_hint"
          |{"m":{"{\"name\":\"Flipper\",\"food\":\"Veggies\",\"water
          |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (hint to unknown classs) as map key") {
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.Bogus\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.test.noncanonical.DogPet","name":"Fido","food":"Meat","numLegs":3}}}"""
        val msg = """Unable to find class named "co.blocke.scalajack.test.noncanonical.Bogus"
          |{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical
          |-------^""".stripMargin
        the[java.lang.ClassNotFoundException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad (invalid) trait json for member trait") {
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.test.noncanonical.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"_hint":"co.blocke.scalajack.test.noncanonical.FishPet","name""Flipper","food":"Veggies","waterTemp":74.33}}}"""
        val msg = """Character out of place. String not expected here.
        |blocke.scalajack.test.noncanonical.FishPet","name""Flipper","food":"Veggies","waterTemp":74.33}}}
        |--------------------------------------------------^""".stripMargin
        the[java.lang.IllegalArgumentException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (missing hint) for member trait") {
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.test.noncanonical.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"name":"Flipper","food":"Veggies","waterTemp":74.33}}}"""
        val msg = """Could not find type field named "_hint""""
        the[java.lang.IllegalStateException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad trait json (hint to unknown classs) for member trait") {
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.test.noncanonical.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"_hint":"co.blocke.scalajack.test.noncanonical.Bogus","name":"Flipper","food":"Veggies","waterTemp":74.33}}}"""
        val msg = """Unable to find class named "co.blocke.scalajack.test.noncanonical.Bogus""""
        the[java.lang.ClassNotFoundException] thrownBy sj.read[SamplePet](js) should have message msg
      }
      it("Bad collection value in map key class having collections") {
        val js = """{"m":{"{\"lookup\":{\"a\":true,\"b\":2},\"favs\":[\"one\",\"two\"]}":{"lookup":{"x":9,"y":10},"favs":["aye","you"]}}}"""
        val msg = """Expected token of type Number, not True
        |{"m":{"{\"lookup\":{\"a\":true,\"b\":2},\"favs\":[\"one\"
        |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj.read[SamplePolyClass](js) should have message msg
      }
      it("Bad custom hint value for map key trait") {
        val petHintMod = StringMatchHintModifier(Tuple("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet]))
        val sj2 = ScalaJack()
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.ShinyPetHolder\",\"address\":\"123 Main\",\"pet\":{\"sort\":\"BreathsWater\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"_hint":"co.blocke.scalajack.test.noncanonical.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}"""
        val msg = """Could not find type field named "kind"
        |{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical
        |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj2.read[SamplePet](js) should have message msg
      }
      it("Bad custom hint value for key member's trait") {
        val petHintMod = StringMatchHintModifier(Tuple("BreathsWater" -> typeOf[FishPet], "BreathsAir" -> typeOf[DogPet]))
        val sj2 = ScalaJack()
          .withHints((typeOf[Pet] -> "kind"))
          .withHintModifiers((typeOf[Pet] -> petHintMod))
        val js = """{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical.ShinyPetHolder\",\"address\":\"123 Main\",\"pet\":{\"kind\":\"BreathsLava\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"_hint":"co.blocke.scalajack.test.noncanonical.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}"""
        val msg = """Could not find type field named "kind"
        |{"m":{"{\"_hint\":\"co.blocke.scalajack.test.noncanonical
        |-------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy sj2.read[SamplePet](js) should have message msg
      }
      */
    }
  }
}