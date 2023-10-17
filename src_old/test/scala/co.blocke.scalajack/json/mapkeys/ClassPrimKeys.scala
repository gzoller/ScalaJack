package co.blocke.scalajack
package json.mapkeys

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import java.util.UUID
import co.blocke.scalajack.model._

class ClassPrimKeys() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("Simple (flat) class as key") {
    describe(
      "-------------------------\n:  Class Map Key Tests  :\n-------------------------", Console.BLUE
    )
    describe("+++ Positive Tests +++") 
    
    val a = SimpleClass("Larry", 32, isOk = true, "golf")
    val b = SimpleClass("Mike", 27, isOk = false, 125)
    val inst = SampleSimple(Map(a -> b))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"name\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"}":{"name":"Mike","age":27,"isOk":false,"favorite":125}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleSimple](js))
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
    val inst = SampleComplex(Map(c1 -> c2))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"name\":\"Larry\",\"age\":32,\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleComplex](js))
  }
  
  test("Simple (flat) trait as key") {
    val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet = DogPet("Fido", Food.Meat, 3)
    val inst = SamplePet(Map(a -> b))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.FishPet\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SamplePet](js))
  }

  test("Complex trait (having members that are traits) as key") {
    val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet = DogPet("Fido", Food.Meat, 3)
    val c: Pet = CompoundPet("Legion", Food.Pellets, b)
    val inst = SamplePet(Map(c -> a))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.json.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"_hint":"co.blocke.scalajack.json.mapkeys.FishPet","name":"Flipper","food":"Veggies","waterTemp":74.33}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SamplePet](js))
  }

  test(
    "Complex trait (having members that are traits) as key where trait member is null"
  ) {
    val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet = null.asInstanceOf[Pet] // DogPet("Fido", Food.Meat, 3)
    val c: Pet = CompoundPet("Legion", Food.Pellets, b)
    val inst = SamplePet(Map(c -> a))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":null}":{"_hint":"co.blocke.scalajack.json.mapkeys.FishPet","name":"Flipper","food":"Veggies","waterTemp":74.33}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SamplePet](js))
  }

  test("Class having collections as members") {
    val a = PolyClass(Map("a" -> 1, "b" -> 2), List("one", "two"))
    val b = PolyClass(Map("x" -> 9, "y" -> 10), List("aye", "you"))
    val inst = SamplePolyClass(Map(a -> b))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"lookup\":{\"a\":1,\"b\":2},\"favs\":[\"one\",\"two\"]}":{"lookup":{"x":9,"y":10},"favs":["aye","you"]}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SamplePolyClass](js))
  }

  test("Class having collections as members (empty collections") {
    val a = PolyClass(Map.empty[String, Int], List.empty[String])
    val b = PolyClass(Map.empty[String, Int], List.empty[String])
    val inst = SamplePolyClass(Map(a -> b))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"lookup\":{},\"favs\":[]}":{"lookup":{},"favs":[]}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SamplePolyClass](js))
  }

  test("Custom trait hint field and value for key trait") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.json.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.json.mapkeys.DogPet")
    )
    val sj2 = co.blocke.scalajack.ScalaJack()
      .withHints((RType.of[Pet] -> "kind"))
      .withHintModifiers((RType.of[Pet] -> petHintMod))

    val a: Pet = FishPet("Flipper", Food.Veggies, 74.33)
    val b: Pet = DogPet("Fido", Food.Meat, 3)
    val inst = SamplePet(Map(a -> b))
    val js = sj2.render(inst)
    assertEquals(
      """{"m":{"{\"kind\":\"BreathsWater\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj2.read[SamplePet](js))
  }

  test("Custom trait hint field and value for key member's trait") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.json.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.json.mapkeys.DogPet")
    )
    val sj2 = co.blocke.scalajack.ScalaJack()
      .withHints((RType.of[Pet] -> "kind"))
      .withHintModifiers((RType.of[Pet] -> petHintMod))

    val a: PetHolder =
      ShinyPetHolder("123 Main", FishPet("Flipper", Food.Veggies, 74.33))
    val b: PetHolder =
      ShinyPetHolder("210 North", DogPet("Fido", Food.Meat, 3))
    val inst = SampleShiny(Map(a -> b))
    val js = sj2.render(inst)
    assertEquals(
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.ShinyPetHolder\",\"address\":\"123 Main\",\"pet\":{\"kind\":\"BreathsWater\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"_hint":"co.blocke.scalajack.json.mapkeys.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj2.read[SampleShiny](js))
  }

  test("Key value is a class having a noncanoncial map") {
    val a = NCKey(Map(0 -> false, 1 -> true), "truth")
    val b = NCKey(Map(1 -> false, 0 -> true), "lie")
    val inst = SampleNCKey(Map(a -> b))
    val js = sj.render(inst)
    assertEquals(
      """{"m":{"{\"nc\":{\"0\":false,\"1\":true},\"name\":\"truth\"}":{"nc":{"1":false,"0":true},"name":"lie"}}}""".asInstanceOf[JSON],js)
    assertEquals(inst, sj.read[SampleNCKey](js))
  }

  test("Extra/unneeded fields in key's JSON harmlessly ignored") {
    val js =
      """{"m":{"{\"name\":\"Larry\",\"bogus\":false,\"age\":32,\"isOk\":true,\"favorite\":\"golf\"}":{"name":"Mike","age":27,"isOk":false,"favorite":125}}}""".asInstanceOf[JSON]
    val a = SimpleClass("Larry", 32, isOk = true, "golf")
    val b = SimpleClass("Mike", 27, isOk = false, 125)
    val inst = SampleSimple(Map(a -> b))
    assertEquals(inst, sj.read[SampleSimple](js))
  }

  test("Bad (invalid--missing field) class json as map key") {
    describe("--- Negative Tests ---")

    val js =
      """{"m":{"{\"age\":32,\"favorite\":\"golf\"}":{"name":"Mike","age":27,"isOk":false,"favorite":125}}}""".asInstanceOf[JSON]
    val msg = """Class co.blocke.scalajack.json.mapkeys.SimpleClass missing required fields: isOk, name
              |{"age":32,"favorite":"golf"}
              |---------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleSimple](js)
    }
  }

  test("Bad class json as map key (valid json, but wrong for given class)") {
    val js =
      """{"m":{"{\"name\":\"Larry\",\"age\":32,\"favorite\":\"golf\"}":{"name":"Mike","age":27,"isOk":false,"favorite":125}}}""".asInstanceOf[JSON]
    val msg = """Class co.blocke.scalajack.json.mapkeys.SimpleClass missing required fields: isOk
              |{"name":"Larry","age":32,"favorite":"golf"}
              |------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleSimple](js)
    }
  }

  test("Bad json for member class") {
    val js =
      """{"m":{"{\"id\":\"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c\",\"simple\":{\"name\":\"Larry\",\"isOk\":true,\"favorite\":\"golf\"},\"allDone\":true}":{"id":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9d","simple":{"name":"Mike","age":27,"isOk":false,"favorite":125},"allDone":false}}}""".asInstanceOf[JSON]
    val msg =
      """Class co.blocke.scalajack.json.mapkeys.SimpleClass missing required fields: age
              |...le":{"name":"Larry","isOk":true,"favorite":"golf"},"allDone":true}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SampleComplex](js)
    }
  }

  test("Bad (invalid) trait json as map key") {
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.FishPet\":\"Flipper\" \"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""".asInstanceOf[JSON]
    val msg =
      """Expected comma here
              |..._hint":"co.blocke.scalajack.json.mapkeys.FishPet":"Flipper" "food":"Veggies"...
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SamplePet](js)
    }
  }

  test("Bad trait json (missing hint) as map key") {
    val js =
      """{"m":{"{\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""".asInstanceOf[JSON]
    val msg = """Type hint '_hint' not found
              |...ame":"Flipper","food":"Veggies","waterTemp":74.33}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SamplePet](js)
    }
  }

  test("Bad trait json (hint to unknown class) as map key") {
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.Bogus\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}":{"_hint":"co.blocke.scalajack.json.mapkeys.DogPet","name":"Fido","food":"Meat","numLegs":3}}}""".asInstanceOf[JSON]
    // val msg =
    //   """Couldn't marshal class for co.blocke.scalajack.json.mapkeys.Bogus
    //           |{"_hint":"co.blocke.scalajack.json.mapkeys.Bogus","name":"Flipper","food":"Ve...
    //           |------------------------------------------------^""".stripMargin
    interceptMessage[java.lang.ClassNotFoundException]("co.blocke.scalajack.json.mapkeys.Bogus"){
      sj.read[SamplePet](js)
    }
  }

  test("Bad (invalid) trait json for member trait") {
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.json.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"_hint":"co.blocke.scalajack.json.mapkeys.FishPet","name"}"Flipper","food":"Veggies","waterTemp":74.33}}}""".asInstanceOf[JSON]
    val msg =
      """Expected colon here
              |..."co.blocke.scalajack.json.mapkeys.FishPet","name"}"Flipper","food":"Veggies"...
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SamplePet](js)
    }
  }

  test("Bad trait json (missing hint) for member trait") {
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.json.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"name":"Flipper","food":"Veggies","waterTemp":74.33}}}""".asInstanceOf[JSON]
    val msg = """Type hint '_hint' not found
              |...ame":"Flipper","food":"Veggies","waterTemp":74.33}}}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SamplePet](js)
    }
  }

  test("Bad trait json (hint to unknown classs) for member trait") {
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.CompoundPet\",\"name\":\"Legion\",\"food\":\"Pellets\",\"pet\":{\"_hint\":\"co.blocke.scalajack.json.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":3}}":{"_hint":"co.blocke.scalajack.json.mapkeys.Bogus","name":"Flipper","food":"Veggies","waterTemp":74.33}}}""".asInstanceOf[JSON]
    interceptMessage[java.lang.ClassNotFoundException]("co.blocke.scalajack.json.mapkeys.Bogus"){
      sj.read[SamplePet](js)
    }
  }

  test("Bad collection value in map key class having collections") {
    val js =
      """{"m":{"{\"lookup\":{\"a\":true,\"b\":2},\"favs\":[\"one\",\"two\"]}":{"lookup":{"x":9,"y":10},"favs":["aye","you"]}}}""".asInstanceOf[JSON]
    val msg = """Expected a Number here
              |{"lookup":{"a":true,"b":2},"favs":["one","two"]}
              |---------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj.read[SamplePolyClass](js)
    }
  }

  test("Bad custom hint value for map key trait (sort instead of kind") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.json.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.json.mapkeys.DogPet")
    )
    val sj2 = co.blocke.scalajack.ScalaJack()
      .withHints(RType.of[Pet] -> "kind")
      .withHintModifiers(RType.of[Pet] -> petHintMod)
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.ShinyPetHolder\",\"address\":\"123 Main\",\"pet\":{\"sort\":\"BreathsWater\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"_hint":"co.blocke.scalajack.json.mapkeys.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}""".asInstanceOf[JSON]
    val msg = """Type hint 'kind' not found
              |...ame":"Flipper","food":"Veggies","waterTemp":74.33}}
              |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj2.read[SampleShiny](js)
    }
  }
  
  test("Bad class for hint in Map key (trait)") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.json.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.json.mapkeys.DogPet")
    )
    val sj2 = co.blocke.scalajack.ScalaJack()
      .withHints(RType.of[Pet] -> "kind")
      .withHintModifiers(RType.of[Pet] -> petHintMod)
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.mapkeys.Bogus\",\"address\":\"123 Main\",\"pet\":{\"kind\":\"BreathsLava\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"_hint":"co.blocke.scalajack.json.mapkeys.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}""".asInstanceOf[JSON]
    interceptMessage[java.lang.ClassNotFoundException]("co.blocke.scalajack.mapkeys.Bogus"){
      sj2.read[SampleShiny](js)
    }
  }

  test("Bad custom hint value for Map key member's trait") {
    val petHintMod = StringMatchHintModifier(
      Map("BreathsWater" -> "co.blocke.scalajack.json.mapkeys.FishPet", "BreathsAir" -> "co.blocke.scalajack.json.mapkeys.DogPet")
    )
    val sj2 = co.blocke.scalajack.ScalaJack()
      .withHints(RType.of[Pet] -> "kind")
      .withHintModifiers((RType.of[Pet] -> petHintMod))
    val js =
      """{"m":{"{\"_hint\":\"co.blocke.scalajack.json.mapkeys.ShinyPetHolder\",\"address\":\"123 Main\",\"pet\":{\"kind\":\"BreathsLava\",\"name\":\"Flipper\",\"food\":\"Veggies\",\"waterTemp\":74.33}}":{"_hint":"co.blocke.scalajack.json.mapkeys.ShinyPetHolder","address":"210 North","pet":{"kind":"BreathsAir","name":"Fido","food":"Meat","numLegs":3}}}}""".asInstanceOf[JSON]
    val msg = """Couldn't marshal class for BreathsLava
      |...","address":"123 Main","pet":{"kind":"BreathsLava","name":"Flipper","food":"...
      |----------------------------------------------------^""".stripMargin
    interceptMessage[co.blocke.scalajack.ScalaJackError](msg){
      sj2.read[SampleShiny](js)
    }
  }