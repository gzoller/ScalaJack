package co.blocke.scalajack
package json.test.mapkeys

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

class ListCollKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("------------------------\n:  List Map Key Tests  :\n------------------------") {
    it("List as key") {
      val l1 = List(1, 2, 3)
      val l2 = List(4, 5, 6)
      val inst = Map(l1 -> l2)
      val js = sj.render(inst)
      assertResult("""{"[1,2,3]":[4,5,6]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[Int], List[Int]]](js)
      }
    }
    it("List of Lists as key") {
      val l1 = List(List(1, 2, 3), List(9, 8, 7))
      val l2 = List(List(4, 5, 6), List(1, 3, 5))
      val inst = Map(l1 -> l2)
      val js = sj.render(inst)
      assertResult("""{"[[1,2,3],[9,8,7]]":[[4,5,6],[1,3,5]]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[List[Int]], List[List[Int]]]](js)
      }
    }
    it("List of Tuples as key") {
      val l1: List[(String, String)] = List(("A", "a"), ("B", "b"), (null, "c"))
      val l2: List[(String, String)] = List(("X", "x"), ("Y", "y"), (null, "z"))
      val inst = Map(l1 -> l2)
      val js = sj.render(inst)
      assertResult("""{"[[\"A\",\"a\"],[\"B\",\"b\"],[null,\"c\"]]":[["X","x"],["Y","y"],[null,"z"]]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[(String, String)], List[(String, String)]]](js)
      }
    }
    it("List of Maps as key") {
      val l1 = List(Map("wow" -> true), Map("ya" -> false))
      val l2 = List(Map("zing" -> false), Map("bling" -> true))
      val inst = Map(l1 -> l2)
      val js = sj.render(inst)
      assertResult("""{"[{\"wow\":true},{\"ya\":false}]":[{"zing":false},{"bling":true}]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[Map[String, Boolean]], List[Map[String, Boolean]]]](js)
      }
    }
    it("List of Case Class as key") {
      val fish = FishPet("Flipper", Food.Meat, 68.9)
      val inst = Map(List(fish, fish) -> List(fish, fish))
      val js = sj.render(inst)
      assertResult("""{"[{\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9},{\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9}]":[{"name":"Flipper","food":"Meat","waterTemp":68.9},{"name":"Flipper","food":"Meat","waterTemp":68.9}]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[FishPet], List[FishPet]]](js)
      }
    }
    it("List of Trait as key") {
      val fish: Pet = FishPet("Flipper", Food.Meat, 68.9)
      val inst = Map(List(fish, fish) -> List(fish, fish))
      val js = sj.render(inst)
      assertResult("""{"[{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.FishPet\",\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9},{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.FishPet\",\"name\":\"Flipper\",\"food\":\"Meat\",\"waterTemp\":68.9}]":[{"_hint":"co.blocke.scalajack.json.test.mapkeys.FishPet","name":"Flipper","food":"Meat","waterTemp":68.9},{"_hint":"co.blocke.scalajack.json.test.mapkeys.FishPet","name":"Flipper","food":"Meat","waterTemp":68.9}]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[Pet], List[Pet]]](js)
      }
    }
    it("List of Any as key") {
      val inst: Map[List[Any], List[Any]] = Map(List(23L, "wow", true) -> List(12.2, 0))
      val js = sj.render(inst)
      assertResult("""{"[23,\"wow\",true]":[12.2,0.0]}""") { js }
      assertResult(true) {
        sj.read[Map[List[Any], List[Any]]](js).isInstanceOf[Map[List[Any], List[Any]]]
      }
    }
    it("List of parameterized class as key") {
      val inst = Map(List(AThing(true, "True"), AThing(false, "False")) -> List(AThing(true, "Yes"), AThing(false, "No")))
      val js = sj.render(inst)
      assertResult("""{"[{\"a\":true,\"b\":\"True\"},{\"a\":false,\"b\":\"False\"}]":[{"a":true,"b":"Yes"},{"a":false,"b":"No"}]}""") { js }
      assertResult(true) {
        sj.read[Map[List[AThing[String, Boolean]], List[AThing[String, Boolean]]]](js).isInstanceOf[Map[List[AThing[String, Boolean]], List[AThing[String, Boolean]]]]
      }
    }
    it("List of parameterized trait as key") {
      val inst: Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]] = Map(List(AThing(true, "True"), AThing(false, "False")) -> List(AThing(true, "Yes"), AThing(false, "No")))
      val js = sj.render(inst)
      assertResult("""{"[{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.AThing\",\"a\":true,\"b\":\"True\"},{\"_hint\":\"co.blocke.scalajack.json.test.mapkeys.AThing\",\"a\":false,\"b\":\"False\"}]":[{"_hint":"co.blocke.scalajack.json.test.mapkeys.AThing","a":true,"b":"Yes"},{"_hint":"co.blocke.scalajack.json.test.mapkeys.AThing","a":false,"b":"No"}]}""") { js }
      assertResult(true) {
        sj.read[Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]]](js).isInstanceOf[Map[List[Thing[Boolean, String]], List[Thing[Boolean, String]]]]
      }
    }
    it("List of Optional as key") {
      val inst: Map[List[Option[String]], List[Option[String]]] = Map(List(Some("hey"), Some("you")) -> List(Some("stop"), Some("go")))
      val js = sj.render(inst)
      assertResult("""{"[\"hey\",\"you\"]":["stop","go"]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[Option[String]], List[Option[String]]]](js)
      }
    }
    it("List of ValueClass as key") {
      val inst = Map(List(VCChar('A'), VCChar('a')) -> List(VCChar('B'), VCChar('b')))
      val js = sj.render(inst)
      assertResult("""{"[\"A\",\"a\"]":["B","b"]}""") { js }
      assertResult(inst) {
        sj.read[Map[List[VCChar], List[VCChar]]](js)
      }
    }
  }
}
