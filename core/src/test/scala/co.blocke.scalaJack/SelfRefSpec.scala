package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class SelfRefSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
  describe("========================\n| -- Self Ref Tests -- |\n========================") {
    describe("Render Basic self-reference") {
      it("Null self-ref") {
        val data = HooLoo("Greg", null)
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","more":null}""")
      }
      it("Non-null self-ref") {
        val d2 = HooLoo("Garth", null)
        val data = HooLoo("Greg", d2)
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","more":{"name":"Garth","more":null}}""")
      }
    }
    describe("Render Collection of self-reference") {
      it("Collection of self-ref (null)") {
        val data = HooLoo5("Greg", null)
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","more":null}""")
      }
      it("Collection of self-ref (non-null)") {
        val data = HooLoo5("Greg", List(HooLoo5("Garth", null), HooLoo5("Graham", null)))
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","more":[{"name":"Garth","more":null},{"name":"Graham","more":null}]}""")
      }
      it("Collection of self-ref (empty but non-null)") {
        val data = HooLoo5("Greg", List.empty[HooLoo5])
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","more":[]}""")
      }
      it("Collection of self-ref (containing a mix of null/non-null elements)") {
        val data = HooLoo5("Greg", List(HooLoo5("Garth", null), null, HooLoo5("Graham", null)))
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","more":[{"name":"Garth","more":null},null,{"name":"Graham","more":null}]}""")
      }
      it("Option of self-ref (some)") {
        val data = HooLoo4("Greg", Some(HooLoo4("Garth", null)))
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","more":{"name":"Garth","more":null}}""")
      }
      it("Option of self-ref (none)") {
        val data = HooLoo4("Greg", None)
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg"}""")
      }
    }
    describe("Render parameterized self-ref") {
      it("Basic param self-ref (null)") {
        val data = HooLoo2("Greg", true, null)
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","x":true,"more":null}""")
      }
      it("Basic param self-ref (non-null)") {
        val data = HooLoo2("Greg", true, HooLoo2("Garth", 32, null))
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","x":true,"more":{"name":"Garth","x":32,"more":null}}""")
      }
      it("Full param self-ref (null)") {
        val data = HooLoo3("Greg", true, null)
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","x":true,"more":null}""")
      }
      it("Full param self-ref (non-null)") {
        val data = HooLoo3("Greg", true, HooLoo3("Garth", false, null))
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","x":true,"more":{"name":"Garth","x":false,"more":null}}""")
      }
    }
    describe("Render Collection of param self-reference") {
      it("Collection of param self-ref (null)") {
        val data = HooLoo6("Greg", "hey", null)
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","x":"hey","more":null}""")
      }
      it("Collection of param self-ref (non-null)") {
        val data = HooLoo6("Greg", "zero", List(HooLoo6("Garth", "one", null), HooLoo6("Graham", "two", null)))
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","x":"zero","more":[{"name":"Garth","x":"one","more":null},{"name":"Graham","x":"two","more":null}]}""")
      }
      it("Collection of param self-ref (empty but non-null)") {
        val data = HooLoo6("Greg", "hey", List.empty[HooLoo6[String]])
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","x":"hey","more":[]}""")
      }
    }
  }
}
