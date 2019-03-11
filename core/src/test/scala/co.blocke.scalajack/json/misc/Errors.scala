package co.blocke.scalajack
package json.misc

import util.Path
import model._

import org.scalatest.{ FunSpec, Matchers }

class Errors extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------\n:  Error Tests  :\n-----------------") {
    it("ReadUnexpectedError") {
      val err = new ReadUnexpectedError(Path.Root \ "foo" \ "bar" \ 5, "Boom")
      err.toString should be("""[$.foo.bar[5]]: Boom""")
    }
    it("ReadMalformedError") {
      val err = new ReadMalformedError(Path.Root \ "foo" \ "bar" \ 5, "Boom", List.empty[String], new Exception("Crash"))
      err.toString should be("""[$.foo.bar[5]] (wrapping java.lang.Exception): Boom""")
    }
    it("ReadInvalidError") {
      val err = new ReadInvalidError(Path.Root \ "foo" \ "bar" \ 5, "Boom")
      err.toString should be("""[$.foo.bar[5]]: Boom""")
    }
    it("ReadMissingError") {
      val err = new ReadMissingError(Path.Root \ "foo" \ "bar" \ 5, "Boom")
      err.toString should be("""[$.foo.bar[5]]: Boom""")
    }
  }
}
