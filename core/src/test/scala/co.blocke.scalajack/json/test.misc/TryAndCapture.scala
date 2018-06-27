package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

import scala.util._
import co.blocke.scalaJack.json.JsonMatchers._

case class Embed(stuff: List[String], num: Int)
case class Boom(
    name:  String,
    other: Try[Embed])
case class Cap(name: String) extends SJCapture

class TryAndCapture() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("---------------------------\n:  Try and Capture Tests  :\n---------------------------") {
    describe("Try:") {
      it("Try sucess") {
        val js = """{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}"""
        val obj = sj.read[Boom](js)
        assertResult(Boom("Greg", Success(Embed(List("a", "b", "c"), 2)))) { obj }
        assertResult("""{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}""") { sj.render(obj) }
      }
      it("Try failure") {
        val js = """{"name":"Greg","other":[1,2,3]}"""
        val obj = sj.read[Boom](js)
        assertResult("DeserializationException(1 error):\n  [$.other] Unexpected(Expected a JSON object, not JArray(List(JLong(1), JLong(2), JLong(3))))") { obj.other.asInstanceOf[Failure[_]].exception.getMessage }
        assertResult(js) { sj.render(obj) }
      }
      it("Try failure 2") {
        val js = """{"name":"Greg","other":  -12.45  ,"num":2}"""
        val obj = sj.read[Boom](js)
        assertResult("DeserializationException(1 error):\n  [$.other] Unexpected(Expected a JSON object, not JDecimal(-12.45))") { obj.other.asInstanceOf[Failure[_]].exception.getMessage }
        assertResult("""{"name":"Greg","other":-12.45}""") { sj.render(obj) }
      }
    }
    describe("Capture:") {
      it("Capture can write semantically equivalent JSON") {
        val js = """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }"""
        val h = sj.read[Cap](js)
        h should equal(Cap("Greg"))
        val js2 = sj.render(h)
        parseJValue(js2) should matchJson(parseJValue(js))
        // Goofy here becuase ordering in JSON can be different--remember: JSON has no native ordering!
        //        js2.contains(""""foo":[1,2,"t"  ]""") should be(true)
        //        js2.contains(""""zing":{"dot":{"age":25,"food":"Pizza"}}""") should be(true)
        //        js2.contains(""""maybe":false""") should be(true)
        //        js2.contains(""""foo":[1,2,"t"  ]""") should be(true)
        //        js2.contains(""""blather":"wow"""") should be(true)
      }
    }
  }
}
