package co.blocke.scalajack
package json.misc

import util.Path
import model._

import org.scalatest.{ FunSpec, Matchers }

class Errors extends FunSpec with Matchers {

  val sj = ScalaJack()
  val js = """{"foo":"bar","other":5}"""
  val reader = sj.parse(js)

  describe("-----------------\n:  Error Tests  :\n-----------------") {
    it("ReadUnexpectedError") {
      val err = new ReadUnexpectedError(reader.showError(Path.Root \ "foo" \ "bar" \ 5, "Boom"))
      err.toString should be(
        """co.blocke.scalajack.model.ReadUnexpectedError: [$.foo.bar[5]]: Boom
          |{"foo":"bar","other":5}
          |^""".stripMargin)
    }
    it("ReadMalformedError") {
      val err = new ReadMalformedError(reader.showError(Path.Root \ "foo" \ "bar" \ 5, "Boom"))
      err.toString should be(
        """co.blocke.scalajack.model.ReadMalformedError: [$.foo.bar[5]]: Boom
          |{"foo":"bar","other":5}
          |^""".stripMargin)
    }
    it("ReadInvalidError") {
      val err = new ReadInvalidError(reader.showError(Path.Root \ "foo" \ "bar" \ 5, "Boom"))
      err.toString should be(
        """co.blocke.scalajack.model.ReadInvalidError: [$.foo.bar[5]]: Boom
          |{"foo":"bar","other":5}
          |^""".stripMargin)
    }
    it("ReadMissingError") {
      val err = new ReadMissingError(reader.showError(Path.Root \ "foo" \ "bar" \ 5, "Boom"))
      err.toString should be(
        """co.blocke.scalajack.model.ReadMissingError: [$.foo.bar[5]]: Boom
          |{"foo":"bar","other":5}
          |^""".stripMargin)
    }
  }
}
