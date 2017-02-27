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
import java.time.ZonedDateTime

class LooseChange extends FunSpec {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> true))
    val b = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(4, 5, 6)))
    val c = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(
      BsonDocument("x" -> "Fido", "y" -> false),
      BsonDocument("x" -> "Cat", "y" -> true)
    )))
    val e = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray("foo", BsonNull(), "bar")))
    val f = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> 1.23))
    val g = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> 25L))
  }

  object ScalaMaster {
    val a = Something("Fred", Map("a" -> 1, "b" -> true))
    val b = Something("Fred", Map("a" -> 1, "b" -> List(4, 5, 6)))
    val c = Something("Fred", Map("a" -> 1, "b" -> List(Map("x" -> "Fido", "y" -> false), Map("x" -> "Cat", "y" -> true))))
    val e = Something("Fred", Map("a" -> 1, "b" -> List("foo", null, "bar")))
    val f = Something("Fred", Map("a" -> 1, "b" -> 1.23))
    val g = Something("Fred", Map("a" -> 1, "b" -> 25L))
  }

  describe("----------------------------\n:  Loose Change (MongoDB)  :\n----------------------------") {
    it("Handles null value") {
      val dbo = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> 15))
      sjM.read[Something](dbo) should be(Something("Fred", Map("a" -> 1, "b" -> 15)))
    }
    it("Should blow up for unsupported BSON type") {
      val dbo = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> BsonJavaScript("code here")))
      the[java.lang.IllegalArgumentException] thrownBy sjM.read[Something](dbo) should have message """Type for value BsonJavaScript{code='code here'} is either deprecated by Mongo, or unsupported by ScalaJack as unsafe (e.g. Javascript)"""
    }
    it("No withTypeModifier") {
      the[java.lang.UnsupportedOperationException] thrownBy
        ScalaJack(MongoFlavor()).withTypeModifier(null.asInstanceOf[HintModifier]) should have message "Not available for Mongo formatting"
    }
    it("ZonedDateTime must work") {
      val dbo = BsonDocument("o1" -> BsonDateTime(1196676930000L), "o2" -> BsonNull())
      sjM.read[SampleZonedDateTime](dbo)
      // Can't test value here as it is local-specific, i.e. value here will be different than in Jenkins build, etc.
      // So if it didn't crash--it worked, I guess...
    }
    it("Handles Null") {
      val dbo = BsonDocument("o1" -> BsonNull(), "o2" -> BsonNull())
      val inst = sjM.read[SampleZonedDateTime](dbo)
      inst should be(SampleZonedDateTime(null, null))
      sjM.render(inst) should be(dbo)
    }
    it("Field name remapping must work") {
      val mfp = MapFactor("wonder", 25L, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.asDocument.toJson should be("""{ "foo_bar" : "wonder", "a_b" : { "$numberLong" : "25" }, "count" : 3, "big_mac" : "hungry" }""")
      sjM.read[MapFactor](dbo) should be(mfp)
    }
    it("Field name remapping on dbkey must work") {
      // val mfp = MapFactorId2("wonder", 25L, 1, 3)
      val mfp = MapFactorId("wonder", 25L, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.asDocument.toJson should be("""{ "_id" : "wonder", "a_b" : { "$numberLong" : "25" }, "count" : 3, "big_mac" : "hungry" }""")
      sjM.read[MapFactorId](dbo) should be(mfp)
    }
    it("Field name remapping on dbkey with multi-part keys must work") {
      val mfp = MapFactorId2("wonder", 25L, 1, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.asDocument.toJson should be("""{ "_id" : { "foo_bar" : "wonder", "a_b" : { "$numberLong" : "25" }, "hey" : 1 }, "count" : 3, "big_mac" : "hungry" }""")
      sjM.read[MapFactorId2](dbo) should be(mfp)
    }
  }
}
