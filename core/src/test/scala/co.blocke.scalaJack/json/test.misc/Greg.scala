package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import scala.reflect.runtime.universe.typeOf
import typeadapter.{ CaseClassTypeAdapter, PlainClassTypeAdapter }

// import MapName

case class MiFactor(
  @MapName(name = "foo_bar") fooBar:String,
  count:                           Int
)

class Greg extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-----------------------\n:  Greg Tests  :\n-----------------------") {
    it("Can find collection and key annotations on plaub class") {
      val js = """{"foo_bar":"thing","count":2}"""
      val r = sj.read[MiFactor](js)
      println(r)
      println(sj.render(r))
    }
  }
}
