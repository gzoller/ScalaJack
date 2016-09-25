package co.blocke.scalajack

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FunSpec, GivenWhenThen}

import scala.language.postfixOps

case class AwfulKey(f1: String, f2: Option[Int])

case class AwfulValue(f3: Double, f4: List[Boolean])

case class AwfulContainer(map: Map[AwfulKey, AwfulValue])

class AwfulKeys extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
  val sjJS = ScalaJack()

  describe("Awful keys") {
    it("should deal with them") {
      val before = AwfulContainer(Map(
        AwfulKey("cat", Some(1)) -> AwfulValue(1.0, List(false, true)),
        AwfulKey("dog", Some(2)) -> AwfulValue(2.0, List(true))
      ))

      val json = sjJS.render(before)
      json should be ("""{"map":{"{\"f1\":\"cat\",\"f2\":1}":{"f3":1.0,"f4":[false,true]},"{\"f1\":\"dog\",\"f2\":2}":{"f3":2.0,"f4":[true]}}}""")

      val after = sjJS.read[AwfulContainer](json)

      after should be(before)
    }
  }
}
