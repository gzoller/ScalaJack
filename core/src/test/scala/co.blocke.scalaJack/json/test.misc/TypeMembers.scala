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

case class Bigger(foo: Int, env: Envelope[FancyBody])

class TypeMembers extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack().parseOrElse((typeOf[Body] -> typeOf[DefaultBody]))

  describe("-----------------------------\n:  Externalized Type Tests  :\n-----------------------------") {
    it("Read and match") {
      val json = """{"Giraffe":"co.blocke.scalajack.json.test.misc.FancyBody","id":"ABC","body":{"message":"Hello"}}"""
      val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
      assertResult((expected, 1)) {
        val x = sj.read[Envelope[Body]](json)
        // Test match functionality
        val num = x.body match {
          case y: FancyBody => 1
          case _            => 2
        }
        (x, num)
      }
    }
    it("Write") {
      val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
      val expected = """{"Giraffe":"co.blocke.scalajack.json.test.misc.FancyBody","id":"DEF","body":{"message":"BOO"}}"""
      assertResult(expected) {
        sj.render[Envelope[Body]](value)
      }
    }
    it("Wrapped") {
      val inst = Bigger(25, Envelope("abc", FancyBody("msg here")))
      val js = sj.render(inst)
      assertResult("""{"foo":25,"env":{"Giraffe":"co.blocke.scalajack.json.test.misc.FancyBody","id":"abc","body":{"message":"msg here"}}}""") { js }
      assertResult(inst) {
        sj.read[Bigger](js)
      }
    }
    it("Works with ParseOrElse") {
      val js = """{"Giraffe":"co.blocke.scalajack.json.test.misc.UnknownBody","id":"DEF","body":{"message":"BOO"}}"""
      val x = sj.read[Envelope[Body]](js)
      println(x)
    }
  }
}
