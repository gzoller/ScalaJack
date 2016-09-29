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
    it("Map as key") {
      (pending)
    }
    it("Map of Lists as key") {
      (pending)
    }
    it("Map of Maps as key") {
      (pending)
    }
    it("Map of Tuples as key") {
      (pending)
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