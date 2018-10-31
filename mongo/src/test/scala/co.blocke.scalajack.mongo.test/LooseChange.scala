package co.blocke.scalajack
package mongo
package test

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import org.mongodb.scala._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson._

class LooseChange extends FunSpec {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> true))
    val b = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(4, 5, 6)))
    val c = BsonDocument("name" -> "Fred", "stuff" -> BsonDocument("a" -> 1, "b" -> BsonArray(
      BsonDocument("x" -> "Fido", "y" -> false),
      BsonDocument("x" -> "Cat", "y" -> true))))
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
      val msg = """DeserializationException(1 error):
                  |  [$.stuff.a] Given value is of unknown type: BsonJavaScript{code='code here'} (reported by: co.blocke.scalajack.typeadapter.AnyDeserializer)""".stripMargin
      the[DeserializationException] thrownBy sjM.read[Something](dbo) should have message msg
    }
    it("No withTypeModifier") {
      the[java.lang.UnsupportedOperationException] thrownBy
        ScalaJack(MongoFlavor()).withTypeModifier(null.asInstanceOf[HintModifier]) should have message "Not available for Mongo formatting"
    }
    it("ZonedDateTime must work") {
      val dbo = Document("o1" -> BsonDateTime(1540922698874L), "o2" -> BsonNull())
      val z = sjM.read[SampleZonedDateTime](dbo)
      z.o1.toString should be("2018-10-30T18:04:58.874Z[UTC]")
      z.o2 should be(null)
      sjM.render(z) should be(dbo)
    }
    it("Handles Null") {
      val dbo = Document("o1" -> BsonNull(), "o2" -> BsonNull())
      val inst = sjM.read[SampleZonedDateTime](dbo)
      inst should be(SampleZonedDateTime(null, null))
      sjM.render(inst) should be(dbo)
    }
    it("Field name remapping must work") {
      val mfp = MapFactor("wonder", 25L, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.toJson should be("""{ "foo_bar" : "wonder", "a_b" : { "$numberLong" : "25" }, "count" : 3, "big_mac" : "hungry" }""")
      sjM.read[MapFactor](dbo) should be(mfp)
    }
    it("Field name remapping on dbkey must work") {
      // val mfp = MapFactorId2("wonder", 25L, 1, 3)
      val mfp = MapFactorId("wonder", 25L, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.toJson should be("""{ "_id" : "wonder", "a_b" : { "$numberLong" : "25" }, "count" : 3, "big_mac" : "hungry" }""")
      sjM.read[MapFactorId](dbo) should be(mfp)
    }
    it("Field name remapping on dbkey with multi-part keys must work") {
      val mfp = MapFactorId2("wonder", 25L, 1, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.toJson should be("""{ "_id" : { "a_b" : { "$numberLong" : "25" }, "hey" : 1, "foo_bar" : "wonder" }, "big_mac" : "hungry", "count" : 3 }""")
      sjM.read[MapFactorId2](dbo) should be(mfp)
    }
  }
}
