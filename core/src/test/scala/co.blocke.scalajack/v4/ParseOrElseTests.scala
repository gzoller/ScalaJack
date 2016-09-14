package co.blocke.scalajack
package test.v4

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat

case class DefaultBlah() extends Blah

class ParseOrElseTests extends FunSpec {

  val sjJS = ScalaJack()
  val vc = VisitorContext().copy(parseOrElse = Map("co.blocke.scalajack.test.v4.Blah" -> "co.blocke.scalajack.test.v4.DefaultBlah"))

  describe("===========================\n| -- ParseOrElse Tests -- |\n===========================") {
    it("Normal successful trait marshalling - class found") {
      val js = """{"_hint":"co.blocke.scalajack.test.v4.Foo","name":"Greg","age":50}"""
      sjJS.read[Blah](js, vc) should be(Foo("Greg", 50))
    }
    it("Normal failure of trait marshalling - class not found") {
      val js = """{"_hint":"co.blocke.scalajack.test.v4.Eek","name":"Greg","age":50}"""
      an[ClassNotFoundException] should be thrownBy sjJS.read[Animal](js, vc)
    }
    it("parseOrElse marshalling - provide given alternate if trait instance can't be marshalled") {
      val js = """{"_hint":"co.blocke.scalajack.test.v4.Eek","name":"Greg","age":50}"""
      sjJS.read[Blah](js, vc) should be(DefaultBlah())
    }
  }
}