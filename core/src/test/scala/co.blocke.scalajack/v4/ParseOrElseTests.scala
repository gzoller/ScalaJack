package co.blocke.scalajack
package test.v4

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat
import scala.reflect.runtime.universe.{ Type, typeOf }

case class DefaultBlah() extends Blah

class ParseOrElseTests extends FunSpec {

  val sjJS = ScalaJack().parseOrElse((typeOf[Blah] -> typeOf[DefaultBlah]))

  describe("===========================\n| -- ParseOrElse Tests -- |\n===========================") {
    it("Normal successful trait marshalling - class found") {
      val js = """{"_hint":"co.blocke.scalajack.test.v4.Foo","name":"Greg","age":50}"""
      sjJS.read[Blah](js) should be(Foo("Greg", 50))
    }
    it("Normal failure of trait marshalling - class not found") {
      val js = """{"_hint":"co.blocke.scalajack.test.v4.Eek","name":"Greg","age":50}"""
      an[ClassNotFoundException] should be thrownBy sjJS.read[Animal](js)
    }
    it("parseOrElse marshalling - provide given alternate if trait instance can't be marshalled") {
      val js = """{"_hint":"co.blocke.scalajack.test.v4.Eek","name":"Greg","age":50}"""
      sjJS.read[Blah](js) should be(DefaultBlah())
    }
  }
}