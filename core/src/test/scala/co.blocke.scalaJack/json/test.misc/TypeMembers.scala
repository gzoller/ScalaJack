package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }

import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.{ currentMirror, universe }
import scala.reflect.runtime.universe.{ TypeTag, typeOf }

trait Body
case class FancyBody(message: String) extends Body

case class Envelope[T <: Body](id: String, body: T) {

  type Giraffe = T

}

class TypeMembers extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-----------------------------\n:  Externalized Type Tests  :\n-----------------------------") {
    val t = currentMirror.staticClass("co.blocke.scalajack.json.test.misc.Envelope").asType.toType

    val tt = new TypeTag[Any] {
      override def in[U <: Universe with Singleton](otherMirror: Mirror[U]): U#TypeTag[Any] = ???
      override val mirror: universe.Mirror = currentMirror
      override def tpe: universe.Type = t
    }

    it("Read") {
      val json = """{"Giraffe":"co.blocke.scalajack.json.test.misc.FancyBody","id":"ABC","body":{"message":"Hello"}}"""

      val expected: Envelope[Body] = Envelope("ABC", FancyBody("Hello"))
      assertResult(expected) {
        sj.read(json)(TypeTags.of(typeOf[Envelope[Body]]))
      }
    }

    it("Write") {
      val value: Any = Envelope("DEF", FancyBody("BOO"))

      val expected = """{"Giraffe":"co.blocke.scalajack.json.test.misc.FancyBody","id":"DEF","body":{"message":"BOO"}}"""
      println("HERE: " + (TypeTags.of(typeOf[Envelope[Body]]) == tt))
      println(TypeTags.of(typeOf[Envelope[Body]]))
      println("------")
      println(tt)
      assertResult(expected) {
        sj.render(value)(TypeTags.of(typeOf[Envelope[Body]]))
      }
    }

  }
}
