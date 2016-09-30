package co.blocke.scalajack
package test
package noncanonical

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

class MapCollPrimKeys() extends FunSpec with Matchers {

  val sj = ScalaJack()

  // case class SampleMap(m: Map[Map[String, Char], Map[String, Char]])
  // case class SampleMapList(m: Map[Map[List[String], List[String]], Map[List[String], List[String]]])
  // case class SampleMapMap(m: Map[Map[String, Int], Map[Int, String]])
  // case class SampleMapTuple(m: Map[Map[(String,Boolean), (String,Boolean)], Map[(String,Boolean), (String,Boolean)]])
  // case class SampleMapClass(m: Map[Map[SampleChar, SampleInt], Map[SampleChar, SampleInt]])
  // case class SampleMapTrait(m: Map[Map[Pet, Pet], Map[Pet, Pet]])
  // case class SampleMapAny(m: Map[Map[Any, Any], Map[Any, Any]])
  // case class SampleMapOptional(m: Map[Map[Option[Int], Option[String]], Map[Option[Int], Option[String]]])
  // case class SampleMapVC(m: Map[Map[VCChar, VCChar], Map[VCChar, VCChar]])

  describe("----------------------------\n:  Map Noncanonical Tests  :\n----------------------------") {
/*
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
    it("Map of Lists as key") {
      val m1 = List(Food.Meat, Food.Veggies)
      val m2 = List(Food.Seeds, Food.Pellets)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      println(js)
    }
    it("Map of Maps as key") {
      val m1 = Map(Food.Meat -> Food.Veggies)
      val m2 = Map(Food.Seeds -> Food.Pellets)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      println(js)
    }
    */
    it("Map of Tuples as key") {
      val m1 = (Food.Meat, Food.Veggies)
      val m2 = (Food.Seeds, Food.Pellets)
      val inst = Map(Map(m1 -> m2) -> Map(m2 -> m1))
      val js = sj.render(inst)
      println(js)
    }
    it("Map of Case Class as key") {
      (pending)
    }
    it("Map of Trait as key") {
      (pending)
    }
    it("Map of Any as key") {
      (pending)
    }
    it("Map of parameterized class as key") {
      (pending)
    }
    it("Map of parameterized trait as key") {
      (pending)
    }
    it("Map of Optional as key") {
      (pending)
    }
    it("Map of ValueClass as key") {
      (pending)
    }
  }
}