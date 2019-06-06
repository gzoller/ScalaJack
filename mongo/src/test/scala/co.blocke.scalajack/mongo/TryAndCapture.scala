package co.blocke.scalajack
package mongo

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Matchers
import org.bson._
import org.bson.types.ObjectId

import scala.util.{ Failure, Success }

class TryAndCapture extends AnyFunSpec with Matchers {

  val sj = ScalaJack(MongoFlavor())

  describe("-------------------------------\n:  Try and Capture (MongoDB) :\n-------------------------------") {
    it("Try success") {
      val d = BsonDocument.parse("""{"name":"Greg","other":{"stuff":["a","b","c"],"num":2}}""")
      val obj = sj.read[Boom](d)
      assertResult(Boom("Greg", Success(Embed(List("a", "b", "c"), 2)))) { obj }
      sj.render(obj) should be(d)
    }
    it("Try failure") {
      val d = BsonDocument.parse("""{"name":"Greg","other":[1,2,3]}""")
      val obj = sj.read[Boom](d)
      val msg = """[$.other]: Expected BeginObject here but found BeginArray""".stripMargin
      assertResult(msg) { obj.other.asInstanceOf[Failure[_]].exception.getMessage }
      sj.render(obj) should be(d)
    }
    it("Try failure 2") {
      val d = BsonDocument.parse("""{"name":"Greg","other":  -12.45  ,"num":2}""")
      val obj = sj.read[Boom](d)
      val msg = """[$.other]: Expected BeginObject here but found Number""".stripMargin
      assertResult(msg) { obj.other.asInstanceOf[Failure[_]].exception.getMessage }
      assertResult(BsonDocument.parse("""{"name":"Greg","other":-12.45}""")) { sj.render(obj) }
    }
    it("SJCapture should work") {
      val s = PersonCapture(new ObjectId(), "Fred", 52, Map(5 -> 1, 6 -> 2))
      val m = sj.render(s)
      m.asDocument.append("extra", new BsonString("hey"))
      val readIn = sj.read[PersonCapture](m)
      readIn should be(s)
      sj.render(readIn).asDocument.toJson.endsWith(""""stuff": {"5": 1, "6": 2}, "extra": "hey"}""") should be(true)
    }
  }
}

