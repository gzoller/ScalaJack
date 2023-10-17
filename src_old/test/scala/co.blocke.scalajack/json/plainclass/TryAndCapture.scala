package co.blocke.scalajack
package json
package plainclass

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import scala.util.{Try, Success, Failure}
import JsonMatcher._


class TryAndCapture() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()

  test("Try sucess") {
    describe("-----------------------------------------\n:  Try and Capture Tests (Plain class)  :\n-----------------------------------------", Console.BLUE)
    describe("Try:")
    
    val js = """{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}""".asInstanceOf[JSON]
    val obj = sj.read[Boom](js)
    assert(
      obj.name == "Greg" && obj.other
        .asInstanceOf[Success[Embed]]
        .get
        .num == 2
    )
    assert(
      jsonMatches("""{"other":{"num":2,"stuff":["a","b","c"]},"name":"Greg"}""".asInstanceOf[JSON], sj.render(obj) ))
  }

  test("Try failure") {
    val js = """{"name":"Greg","other":[1,2,3]}""".asInstanceOf[JSON]
    val obj = sj.read[Boom](js)
    assertEquals("""Expected start of object here
                  |{"name":"Greg","other":[1,2,3]}
                  |-----------------------^""".stripMargin,
      obj.other.asInstanceOf[Failure[_]].exception.getMessage)
    assert(jsonMatches("""{"other":[1,2,3],"name":"Greg"}""".asInstanceOf[JSON], sj.render(obj) ))
  }

  test("Try failure 2") {
    val js = """{"name":"Greg","other":  -12.45  ,"num":2}""".asInstanceOf[JSON]
    val obj = sj.read[Boom](js)
    assertEquals("""Expected start of object here
                  |{"name":"Greg","other":  -12.45  ,"num":2}
                  |-------------------------^""".stripMargin,
      obj.other.asInstanceOf[Failure[_]].exception.getMessage)
    assert(jsonMatches("""{"other":-12.45,"name":"Greg"}""".asInstanceOf[JSON], sj.render(obj) ))
  }

  test("Plain-class capture can write semantically equivalent JSON") {
    describe("Capture:")
    val js =
      """{"name":"Greg", "foo":[1,2,"t"  ], "zing":  {"dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }""".asInstanceOf[JSON]
    val h = sj.read[Cap](js)
    assertEquals(h.name,"Greg")
    val js2 = sj.render(h)
    assert( jsonMatches(js2,js) )
  }

  test("Case class capture can write semantically equivalent JSON") {
    val js =
      """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"_hint":"a.b.com.Hey", "dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }""".asInstanceOf[JSON]
    val h = sj.read[CaseCap](js)
    assertEquals(h.name, "Greg")
    val js2 = sj.render(h)
    assert( jsonMatches(js2,js) )
  }

  test("Java class capture can write semantically equivalent JSON") {
    val js =
      """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"_hint":"a.b.com.Hey", "dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }""".asInstanceOf[JSON]
    val h = sj.read[JavaCap](js)
    assertEquals(h.getName ,"Greg")
    val js2 = sj.render(h)
    assert( jsonMatches(js2,js) )
  }
