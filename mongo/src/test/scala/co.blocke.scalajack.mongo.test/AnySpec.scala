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
    stuff: Map[String, Any])

class AnySpec extends FunSpec {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = Document("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> true))
    val b = Document("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(4, 5, 6)))
    val c = Document("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(
      BsonDocument("x" -> "Fido", "y" -> false),
      BsonDocument("x" -> "Cat", "y" -> true))))
    val e = Document("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray("foo", BsonNull(), "bar")))
    val f = Document("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> 1.23))
    val g = Document("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> 25L))
  }

  object ScalaMaster {
    val a = Something("Fred", Map("a" -> 1, "b" -> true))
    val b = Something("Fred", Map("a" -> 1, "b" -> List(4, 5, 6)))
    val c = Something("Fred", Map("a" -> 1, "b" -> List(Map("x" -> "Fido", "y" -> false), Map("x" -> "Cat", "y" -> true))))
    val e = Something("Fred", Map("a" -> 1, "b" -> List("foo", null, "bar")))
    val f = Something("Fred", Map("a" -> 1, "b" -> 1.23))
    val g = Something("Fred", Map("a" -> 1, "b" -> 25L))
  }

  describe("-------------------------\n:  Any Tests (MongoDB)  :\n-------------------------") {
    describe("Render Tests") {
      it("Any 1") {
        sjM.render(ScalaMaster.a) should be(MongoMaster.a)
      }
      it("Any 2") {
        sjM.render(ScalaMaster.b) should be(MongoMaster.b)
      }
      it("Any 3") {
        sjM.render(ScalaMaster.c) should be(MongoMaster.c)
      }
      it("Any 4") {
        sjM.render(ScalaMaster.e) should be(MongoMaster.e)
      }
      it("Any 5") {
        sjM.render(ScalaMaster.f) should be(MongoMaster.f)
      }
      it("Any 6") {
        sjM.render(ScalaMaster.g) should be(MongoMaster.g)
      }
    }
    describe("Read Tests") {
      it("Any 1") {
        sjM.read[Something](MongoMaster.a) should equal(ScalaMaster.a)
      }
      it("Any 2") {
        sjM.read[Something](MongoMaster.b) should equal(ScalaMaster.b)
      }
      it("Any 3") {
        sjM.read[Something](MongoMaster.c) should equal(ScalaMaster.c)
      }
      it("Any 4") {
        sjM.read[Something](MongoMaster.e) should equal(ScalaMaster.e)
      }
      it("Any 5") {
        sjM.read[Something](MongoMaster.f) should equal(ScalaMaster.f)
      }
      it("Any 6") {
        sjM.read[Something](MongoMaster.g) should equal(ScalaMaster.g)
      }
    }
  }
}
