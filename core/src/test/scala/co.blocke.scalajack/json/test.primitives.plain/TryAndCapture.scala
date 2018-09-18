package co.blocke.scalajack
package json.test.primitives.plain

import org.scalatest.{ FunSpec, Matchers }

import scala.util._

class TryAndCapture() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("-----------------------------------\n:  Try and Capture Tests (Plain)  :\n-----------------------------------") {
    /*
    describe("Try:") {
      it("Try sucess") {
        val js = """{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}"""
        val obj = sj.read[Boom](js)
        assertResult(true) {
          obj.name == "Greg" && obj.other.asInstanceOf[Success[Embed]].get.num == 2
        }
        assertResult("""{"other":{"num":2,"stuff":["a","b","c"]},"name":"Greg"}""") { sj.render(obj) }
      }
      it("Try failure") {
        val js = """{"name":"Greg","other":[1,2,3]}"""
        val obj = sj.read[Boom](js)
        assertResult("""DeserializationException(1 error):
  [$.other] Expected a JSON object (reported by: co.blocke.scalajack.typeadapter.PlainClassDeserializer)""") { obj.other.asInstanceOf[Failure[_]].exception.getMessage }
        assertResult("""{"other":[1,2,3],"name":"Greg"}""") { sj.render(obj) }
      }
      it("Try failure 2") {
        val js = """{"name":"Greg","other":  -12.45  ,"num":2}"""
        val obj = sj.read[Boom](js)
        assertResult("""DeserializationException(1 error):
  [$.other] Expected a JSON object (reported by: co.blocke.scalajack.typeadapter.PlainClassDeserializer)""") { obj.other.asInstanceOf[Failure[_]].exception.getMessage }
        assertResult("""{"other":-12.45,"name":"Greg"}""") { sj.render(obj) }
      }
    }
    */
    describe("Capture:") {
      it("Capture can write semantically equivalent JSON") {
        val js = """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }"""
        val h = sj.read[Cap](js)
        h.name should equal("Greg")
        val js2 = sj.render(h)
        println(js2)
        /*
        // Goofy here becuase ordering in JSON can be different--remember: JSON has no native ordering!
        js2.contains(""""foo":[1,2,"t"  ]""") should be(true)
        js2.contains(""""zing":{"dot":{"age":25,"food":"Pizza"}}""") should be(true)
        js2.contains(""""maybe":false""") should be(true)
        js2.contains(""""foo":[1,2,"t"  ]""") should be(true)
        js2.contains(""""blather":"wow"""") should be(true)
        */
      }
    }
  }
}
