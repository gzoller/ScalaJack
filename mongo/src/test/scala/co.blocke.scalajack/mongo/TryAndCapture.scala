package co.blocke.scalajack
package mongo

import TestUtil._
import munit._
import munit.internal.console
import org.bson._
import org.bson.types.ObjectId

import scala.util.{ Failure, Success }

class TryAndCapture extends FunSuite:

  val sj = ScalaJack(MongoFlavor())

  test("Try success") {
    describe(
      "-------------------------------\n:  Try and Capture (MongoDB) :\n-------------------------------", Console.BLUE
    )
    val d = BsonDocument.parse(
      """{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}"""
    )
    val obj = sj.read[Boom](d)
    assertEquals(Boom("Greg", Success(Embed(List("a", "b", "c"), 2))), obj)
    assertEquals(sj.render(obj), d)
  }

  test("Try failure") {
    val d = BsonDocument.parse("""{"name":"Greg","other":[1,2,3]}""")
    val obj = sj.read[Boom](d)
    val msg =
      """Expected document (object) here, not 'BsonArray{values=[BsonInt32{value=1}, BsonInt32{value=2}, BsonInt32{value=3}]}'""".stripMargin
    assertEquals(msg, obj.other.asInstanceOf[Failure[_]].exception.getMessage)
    assertEquals(sj.render(obj), d)
  }

  test("Try failure 2") {
    val d =
      BsonDocument.parse("""{"name":"Greg","other":  -12.45  ,"num":2}""")
    val obj = sj.read[Boom](d)
    val msg =
      """Expected document (object) here, not 'BsonDouble{value=-12.45}'""".stripMargin
    assertEquals(msg, obj.other.asInstanceOf[Failure[_]].exception.getMessage)
    assert(BsonDocument.parse("""{"name":"Greg","other":-12.45}""") == sj.render(obj))
  }

  test("SJCapture should work") {
    val s = PersonCapture(new ObjectId(), "Fred", 52, Map(5 -> 1, 6 -> 2))
    val m = sj.render(s)
    m.asDocument.append("extra", new BsonString("hey"))
    val readIn = sj.read[PersonCapture](m)
    assertEquals(readIn, s)
    assert(sj.render(readIn)
      .asDocument
      .toJson
      .endsWith(""""stuff": {"5": 1, "6": 2}, "extra": "hey"}"""))
  }