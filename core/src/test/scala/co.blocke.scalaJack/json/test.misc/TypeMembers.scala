package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }

import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.{ currentMirror, universe }
import scala.reflect.runtime.universe.{ TypeTag, typeOf, Type }

trait Body
case class FancyBody(message: String) extends Body
case class DefaultBody(message: String = "Unknown body") extends Body

case class Envelope[T <: Body](id: String, body: T) {

  type Giraffe = T

}

class TypeMembers extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack().parseOrElse((typeOf[Body] -> typeOf[DefaultBody]))

  describe("-----------------------------\n:  Externalized Type Tests  :\n-----------------------------") {
    /*
    it("Read") {
      val json = """{"Giraffe":"co.blocke.scalajack.json.test.misc.FancyBody","id":"ABC","body":{"message":"Hello"}}"""
      val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
      assertResult(expected) {
        val x = sj.read[Envelope[Body]](json)(TypeTags.of(typeOf[Envelope[Body]]))
        if (x.body.isInstanceOf[FancyBody]) println("Fancy!")
        // Test match functionality
        x.body match {
          case y: FancyBody => println("Again!")
          case _            => println("nope")
        }
        x
      }
    }
    it("Write") {
      val value: Any = Envelope("DEF", FancyBody("BOO"))
      val expected = """{"Giraffe":"co.blocke.scalajack.json.test.misc.FancyBody","id":"DEF","body":{"message":"BOO"}}"""
      assertResult(expected) {
        sj.render(value)(TypeTags.of(typeOf[Envelope[Body]]))
      }
    }
*/
    it("Works with ParseOrElse") {
      val js = """{"Giraffe":"co.blocke.scalajack.json.test.misc.UnknownBody","id":"DEF","body":{"message":"BOO"}}"""
      val x = sj.read[Envelope[Body]](js)(TypeTags.of(typeOf[Envelope[Body]]))
      println(x)
    }
  }
}
