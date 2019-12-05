package co.blocke.scalajack
package json
package primitives.plain

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec

import scala.util._
import JsonMatcher._

class TryAndCapture() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe(
    "-----------------------------------\n:  Try and Capture Tests (Plain)  :\n-----------------------------------"
  ) {
      describe("Try:") {
        it("Try sucess") {
          val js = """{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}"""
          val obj = sj.read[Boom](js)
          assertResult(true) {
            obj.name == "Greg" && obj.other
              .asInstanceOf[Success[Embed]]
              .get
              .num == 2
          }
          assertResult(
            """{"other":{"num":2,"stuff":["a","b","c"]},"name":"Greg"}"""
          ) { sj.render(obj) }
        }
        it("Try failure") {
          val js = """{"name":"Greg","other":[1,2,3]}"""
          val obj = sj.read[Boom](js)
          assertResult("""Expected start of object here
                       |{"name":"Greg","other":[1,2,3]}
                       |-----------------------^""".stripMargin) {
            obj.other.asInstanceOf[Failure[_]].exception.getMessage
          }
          assertResult("""{"other":[1,2,3],"name":"Greg"}""") { sj.render(obj) }
        }
        it("Try failure 2") {
          val js = """{"name":"Greg","other":  -12.45  ,"num":2}"""
          val obj = sj.read[Boom](js)
          assertResult("""Expected start of object here
                       |{"name":"Greg","other":  -12.45  ,"num":2}
                       |-------------------------^""".stripMargin) {
            obj.other.asInstanceOf[Failure[_]].exception.getMessage
          }
          assertResult("""{"other":-12.45,"name":"Greg"}""") { sj.render(obj) }
        }
      }
      describe("Capture:") {
        it("Plain-class capture can write semantically equivalent JSON") {
          val js =
            """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }"""
          val h = sj.read[Cap](js)
          h.name should equal("Greg")
          val js2 = sj.render(h)
          parseJValue(js2) should matchJson(parseJValue(js))
        }
        it("Case class capture can write semantically equivalent JSON") {
          val js =
            """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"_hint":"a.b.com.Hey", "dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }"""
          val h = sj.read[CaseCap](js)
          h.name should equal("Greg")
          val js2 = sj.render(h)
          parseJValue(js2) should matchJson(parseJValue(js))
        }
        it("Java class capture can write semantically equivalent JSON") {
          val js =
            """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"_hint":"a.b.com.Hey", "dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }"""
          val h = sj.read[JavaCap](js)
          h.getName should equal("Greg")
          val js2 = sj.render(h)
          parseJValue(js2) should matchJson(parseJValue(js))
        }
      }
    }
}
