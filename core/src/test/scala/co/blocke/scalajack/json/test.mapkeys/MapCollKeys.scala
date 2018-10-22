package co.blocke.scalajack
package json.test.mapkeys

import org.scalatest.{ FunSpec, Matchers }

class MapCollKeys() extends FunSpec with Matchers {

  val sj = ScalaJack() //.isCanonical(false)

  describe("-----------------------\n:  Map Map Key Tests  :\n-----------------------") {
    it("Map as key") {
      val m1 = Map(1 -> 2)
      val m2 = Map(3 -> 4)
      val inst = Map(m1 -> m2)
      val js = sj.render(inst)
      assertResult("""{"{\"1\":2}":{"3":4}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Int, Int], Map[Int, Int]]](js)
      }
    }
    it("Map as key, map value is null") {
      val m1 = Map(1 -> 2)
      val m2: Map[Int, Int] = null
      val inst = Map(m1 -> m2)
      val js = sj.render(inst)
      assertResult("""{"{\"1\":2}":null}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Int, Int], Map[Int, Int]]](js)
      }
    }
    it("Map of Lists as key") {
      val m1 = List(Food.Meat, Food.Veggies)
      val m2 = List(Food.Seeds, Food.Pellets)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"[\\\"Meat\\\",\\\"Veggies\\\"]\":[\"Seeds\",\"Pellets\"]}":{"[\"Seeds\",\"Pellets\"]":["Meat","Veggies"]}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[List[Food.Value], List[Food.Value]], Map[List[Food.Value], List[Food.Value]]]](js)
      }
    }
    it("Map of Maps as key") {
      val m1 = Map(Food.Meat -> Food.Veggies)
      val m2 = Map(Food.Seeds -> Food.Pellets)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"Meat\\\":\\\"Veggies\\\"}\":{\"Seeds\":\"Pellets\"}}":{"{\"Seeds\":\"Pellets\"}":{"Meat":"Veggies"}}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Map[Food.Value, Food.Value], Map[Food.Value, Food.Value]], Map[Map[Food.Value, Food.Value], Map[Food.Value, Food.Value]]]](js)
      }
    }
    it("Map of Tuples as key") {
      val m1 = (Food.Meat, Food.Veggies)
      val m2 = (Food.Seeds, Food.Pellets)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"[\\\"Meat\\\",\\\"Veggies\\\"]\":[\"Seeds\",\"Pellets\"]}":{"[\"Seeds\",\"Pellets\"]":["Meat","Veggies"]}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[(Food.Value, Food.Value), (Food.Value, Food.Value)], Map[(Food.Value, Food.Value), (Food.Value, Food.Value)]]](js)
      }
    }

    it("Map of Case Class as key") {
      val m1 = Map(DogPet("Fido", Food.Meat, 4) -> FishPet("Flipper", Food.Meat, 87.3))
      val m2 = Map(FishPet("Flipper", Food.Meat, 87.3) -> DogPet("Fido", Food.Meat, 4))
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"{\\\\\\\"name\\\\\\\":\\\\\\\"Fido\\\\\\\",\\\\\\\"food\\\\\\\":\\\\\\\"Meat\\\\\\\",\\\\\\\"numLegs\\\\\\\":4}\\\":{\\\"name\\\":\\\"Flipper\\\",\\\"food\\\":\\\"Meat\\\",\\\"waterTemp\\\":87.3}}\":{\"{\\\"name\\\":\\\"Flipper\\\",\\\"food\\\":\\\"Meat\\\",\\\"waterTemp\\\":87.3}\":{\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}}}":{"{\"{\\\"name\\\":\\\"Flipper\\\",\\\"food\\\":\\\"Meat\\\",\\\"waterTemp\\\":87.3}\":{\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}}":{"{\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}":{"name":"Flipper","food":"Meat","waterTemp":87.3}}}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Map[DogPet, FishPet], Map[FishPet, DogPet]], Map[Map[FishPet, DogPet], Map[DogPet, FishPet]]]](js)
      }
    }
    it("Map of Trait as key") {
      val m1: Map[Pet, Pet] = Map(DogPet("Fido", Food.Meat, 4) -> FishPet("Flipper", Food.Meat, 87.3))
      val m2: Map[Pet, Pet] = Map(FishPet("Flipper", Food.Meat, 87.3) -> DogPet("Fido", Food.Meat, 4))
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"{\\\\\\\"_hint\\\\\\\":\\\\\\\"co.blocke.scalajack.json.test.mapkeys.DogPet\\\\\\\",\\\\\\\"name\\\\\\\":\\\\\\\"Fido\\\\\\\",\\\\\\\"food\\\\\\\":\\\\\\\"Meat\\\\\\\",\\\\\\\"numLegs\\\\\\\":4}\\\":{\\\"_hint\\\":\\\"co.blocke.scalajack.json.test.mapkeys.FishPet\\\",\\\"name\\\":\\\"Flipper\\\",\\\"food\\\":\\\"Meat\\\",\\\"waterTemp\\\":87.3}}\":{\"{\\\"_hint\\\":\\\"co.blocke.scalajack.json.test.mapkeys.FishPet\\\",\\\"name\\\":\\\"Flipper\\\",\\\"food\\\":\\\"Meat\\\",\\\"waterTemp\\\":87.3}\":{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}}}":{"{\"{\\\"_hint\\\":\\\"co.blocke.scalajack.json.test.mapkeys.FishPet\\\",\\\"name\\\":\\\"Flipper\\\",\\\"food\\\":\\\"Meat\\\",\\\"waterTemp\\\":87.3}\":{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}}":{"{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.DogPet\",\"name\":\"Fido\",\"food\":\"Meat\",\"numLegs\":4}":{"_hint":"co.blocke.scalajack.json.test.mapkeys.FishPet","name":"Flipper","food":"Meat","waterTemp":87.3}}}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Map[Pet, Pet], Map[Pet, Pet]], Map[Map[Pet, Pet], Map[Pet, Pet]]]](js)
      }
    }
    it("Map of Any as key") {
      val m1: Map[Any, Any] = Map(123.45 -> 2)
      val m2: Map[Any, Any] = Map(398328372 -> 0)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"123.45\\\":2}\":{\"398328372\":0}}":{"{\"398328372\":0}":{"123.45":2}}}""") { js }
      assertResult(true) {
        sj.read[Map[Map[Map[Any, Any], Map[Any, Any]], Map[Map[Any, Any], Map[Any, Any]]]](js).isInstanceOf[Map[Map[Map[Any, Any], Map[Any, Any]], Map[Map[Any, Any], Map[Any, Any]]]]
      }
    }
    it("Map of parameterized class as key") {
      val m1: Map[AThing[Int, String], AThing[Int, String]] = Map(AThing("one", 1) -> AThing("two", 2))
      val m2: Map[AThing[Int, String], AThing[Int, String]] = Map(AThing("four", 4) -> AThing("three", 3))
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"{\\\\\\\"a\\\\\\\":\\\\\\\"one\\\\\\\",\\\\\\\"b\\\\\\\":1}\\\":{\\\"a\\\":\\\"two\\\",\\\"b\\\":2}}\":{\"{\\\"a\\\":\\\"four\\\",\\\"b\\\":4}\":{\"a\":\"three\",\"b\":3}}}":{"{\"{\\\"a\\\":\\\"four\\\",\\\"b\\\":4}\":{\"a\":\"three\",\"b\":3}}":{"{\"a\":\"one\",\"b\":1}":{"a":"two","b":2}}}}""") { js }
      assertResult(true) {
        sj.read[Map[Map[Map[AThing[Int, String], AThing[Int, String]], Map[AThing[Int, String], AThing[Int, String]]], Map[Map[AThing[Int, String], AThing[Int, String]], Map[AThing[Int, String], AThing[Int, String]]]]](js).isInstanceOf[Map[Map[Map[AThing[Int, String], AThing[Int, String]], Map[AThing[Int, String], AThing[Int, String]]], Map[Map[AThing[Int, String], AThing[Int, String]], Map[AThing[Int, String], AThing[Int, String]]]]]
      }
    }
    it("Map of parameterized trait as key") {
      val m1: Map[Thing[String, Int], Thing[String, Int]] = Map(AThing("one", 1) -> AThing("two", 2))
      val m2: Map[Thing[String, Int], Thing[String, Int]] = Map(AThing("four", 4) -> AThing("three", 3))
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"{\\\\\\\"_hint\\\\\\\":\\\\\\\"co.blocke.scalajack.json.test.mapkeys.AThing\\\\\\\",\\\\\\\"a\\\\\\\":\\\\\\\"one\\\\\\\",\\\\\\\"b\\\\\\\":1}\\\":{\\\"_hint\\\":\\\"co.blocke.scalajack.json.test.mapkeys.AThing\\\",\\\"a\\\":\\\"two\\\",\\\"b\\\":2}}\":{\"{\\\"_hint\\\":\\\"co.blocke.scalajack.json.test.mapkeys.AThing\\\",\\\"a\\\":\\\"four\\\",\\\"b\\\":4}\":{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.AThing\",\"a\":\"three\",\"b\":3}}}":{"{\"{\\\"_hint\\\":\\\"co.blocke.scalajack.json.test.mapkeys.AThing\\\",\\\"a\\\":\\\"four\\\",\\\"b\\\":4}\":{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.AThing\",\"a\":\"three\",\"b\":3}}":{"{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.AThing\",\"a\":\"one\",\"b\":1}":{"_hint":"co.blocke.scalajack.json.test.mapkeys.AThing","a":"two","b":2}}}}""") { js }
      assertResult(true) {
        sj.read[Map[Map[Map[Thing[String, Int], Thing[String, Int]], Map[Thing[String, Int], Thing[String, Int]]], Map[Map[Thing[String, Int], Thing[String, Int]], Map[Thing[String, Int], Thing[String, Int]]]]](js).isInstanceOf[Map[Map[Map[Thing[String, Int], Thing[String, Int]], Map[Thing[String, Int], Thing[String, Int]]], Map[Map[Thing[String, Int], Thing[String, Int]], Map[Thing[String, Int], Thing[String, Int]]]]]
      }
    }
    it("Map of Optional as key") {
      val m1: Map[Option[Int], Option[Int]] = Map(Some(3) -> None)
      val m2: Map[Option[Int], Option[Int]] = Map(None -> Some(2), Some(5) -> None)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"3\\\":null}\":{\"\":2,\"5\":null}}":{"{\"\":2,\"5\":null}":{"3":null}}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]], Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]]]](js)
      }
    }
    it("Map of Option as key where Option is null") {
      val m1: Map[Option[Int], Option[Int]] = Map(Some(3) -> None)
      val m0 = Map.empty[Option[Int], Option[Int]]
      val bad: Option[Int] = null
      val m2 = m0 + (bad -> Some(99))
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"3\\\":null}\":{\"null\":99}}":{"{\"null\":99}":{"3":null}}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]], Map[Map[Option[Int], Option[Int]], Map[Option[Int], Option[Int]]]]](js)
      }
    }
    it("Map of ValueClass as key") {
      val m1: Map[VCChar, VCChar] = Map(VCChar('Z') -> VCChar('z'))
      val m2: Map[VCChar, VCChar] = Map(VCChar('A') -> VCChar('a'))
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      assertResult("""{"{\"{\\\"Z\\\":\\\"z\\\"}\":{\"A\":\"a\"}}":{"{\"A\":\"a\"}":{"Z":"z"}}}""") { js }
      assertResult(inst) {
        sj.read[Map[Map[Map[VCChar, VCChar], Map[VCChar, VCChar]], Map[Map[VCChar, VCChar], Map[VCChar, VCChar]]]](js)
      }
    }
  }
}
