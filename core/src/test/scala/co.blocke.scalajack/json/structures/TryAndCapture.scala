package co.blocke.scalajack
package json.structures

import co.blocke.scala_reflection._
import TestUtil._
import munit._
import munit.internal.console
import co.blocke.scalajack.json.JSON
import scala.util._
import json.JsonMatcher
import co.blocke.scalajack.SJCapture

case class Embed(stuff: List[String], num: Int)
case class Boom(name: String, other: Try[Embed])
case class Cap(name: String) extends SJCapture


class TryAndCapture() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack()
  
  test("Try success") {
    describe( "---------------------------\n:  Try and Capture Tests  :\n---------------------------",Console.BLUE)
    describe("Try:")

    val js = """{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}""".asInstanceOf[JSON]
    val obj = sj.read[Boom](js)
    assertEquals(Boom("Greg", Success(Embed(List("a", "b", "c"), 2))), obj)
    assertEquals("""{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}""".asInstanceOf[JSON], sj.render(obj) )
  }

  test("Try failure") {
    val js = """{"name":"Greg","other":[1,2,3]}""".asInstanceOf[JSON]
    val obj = sj.read[Boom](js)
    val msg = """Expected start of object here
              |{"name":"Greg","other":[1,2,3]}
              |-----------------------^""".stripMargin
    assertEquals(msg, obj.other.asInstanceOf[Failure[_]].exception.getMessage)
    assertEquals(js, sj.render(obj) )
  }

  test("Try failure 2") {
    val js = """{"name":"Greg","other":  -12.45  ,"num":2}""".asInstanceOf[JSON]
    val obj = sj.read[Boom](js)
    val msg = """Expected start of object here
              |{"name":"Greg","other":  -12.45  ,"num":2}
              |-------------------------^""".stripMargin
    assertEquals(msg, obj.other.asInstanceOf[Failure[_]].exception.getMessage)
    assert("""{"name":"Greg","other":-12.45}""".asInstanceOf[JSON] == sj.render(obj) )
  }

  test("Capture can write semantically equivalent JSON") {
    describe("Capture:") 

    val js =
      """{"name":"Greg", "foo":[1,2,"t"  ], "zing" :  {"dot":{"age":25,"food":"Pizza"}}, "blather":"wow", "boo": -29384.34, "maybe": false }""".asInstanceOf[JSON]
    val h = sj.read[Cap](js)
    assertEquals(h, Cap("Greg"))
    assert(JsonMatcher.jsonMatches(sj.render(h), js))
  }
