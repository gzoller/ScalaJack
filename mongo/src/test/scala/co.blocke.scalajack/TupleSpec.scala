package co.blocke.scalajack
package test

import mongo._
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat
import org.mongodb.scala._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson._

case class TT2(name: String, rec: Map[String, List[(String, Int, Boolean)]])

class TupleSpec extends FunSpec {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = Document(BsonDocument("name" -> "Larry", "rec" -> BsonDocument("foo" -> BsonArray(BsonArray("a", 1, true)), "hey" -> BsonArray(BsonArray("x", 8, false), BsonArray("r", 3, true)))))
  }

  object ScalaMaster {
    val r = List(("a", 1, true))
    val r2 = List(("x", 8, false), ("r", 3, true))
    val a = TT2("Larry", Map("foo" -> r, "hey" -> r2))
  }

  describe("===================\n| -- Any Tests -- |\n===================") {
    it("Render Tests") {
      sjM.render(ScalaMaster.a) should be(MongoMaster.a)
    }
    it("Read Tests") {
      sjM.read[TT2](MongoMaster.a) should be(ScalaMaster.a)
    }
  }
}
