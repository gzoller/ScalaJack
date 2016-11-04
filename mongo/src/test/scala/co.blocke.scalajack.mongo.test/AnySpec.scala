package co.blocke.scalajack
package mongo
package test

import mongo._
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.mongodb.scala._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson._

case class Something(
  name:  String,
  stuff: Map[String, Any]
)

class AnySpec extends FunSpec {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> true))
    val b = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(4, 5, 6)))
    val c = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(
      BsonDocument("x" -> "Fido", "y" -> false),
      BsonDocument("x" -> "Cat", "y" -> true)
    )))
    val e = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray("foo", BsonNull(), "bar")))
  }

  object ScalaMaster {
    val a = Something("Fred", Map("a" -> 1, "b" -> true))
    val b = Something("Fred", Map("a" -> 1, "b" -> List(4, 5, 6)))
    val c = Something("Fred", Map("a" -> 1, "b" -> List(Map("x" -> "Fido", "y" -> false), Map("x" -> "Cat", "y" -> true))))
    val e = Something("Fred", Map("a" -> 1, "b" -> List("foo", null, "bar")))
  }

  describe("-------------------------\n:  Any Tests (MongoDB)  :\n-------------------------") {
    describe("Render Tests") {
      sjM.render(ScalaMaster.a) should be(MongoMaster.a)
      sjM.render(ScalaMaster.b) should be(MongoMaster.b)
      sjM.render(ScalaMaster.c) should be(MongoMaster.c)
      sjM.render(ScalaMaster.e) should be(MongoMaster.e)
    }
    describe("Read Tests") {
      sjM.read[Something](MongoMaster.a) should equal(ScalaMaster.a)
      sjM.read[Something](MongoMaster.b) should equal(ScalaMaster.b)
      sjM.read[Something](MongoMaster.c) should equal(ScalaMaster.c)
      sjM.read[Something](MongoMaster.e) should equal(ScalaMaster.e)
    }
  }
}
