package co.blocke.scalajack
package mongo

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import org.mongodb.scala.bson._
import scala.reflect.runtime.universe.typeOf

class Custom extends FunSpec {

  describe("----------------------------\n:  Custom Tests (MongoDB) :\n----------------------------") {

    it("Supports withAdapters") {
      val sj = ScalaJack(MongoFlavor()).withAdapters(PhoneAdapter)
      val dbo = BsonDocument("_id" -> BsonString("Fred"), "phone" -> "123-456-7890")
      dbo.toJson should equal("""{"_id": "Fred", "phone": "123-456-7890"}""")
      sj.read[Person](dbo) should equal(Person("Fred", "1234567890"))
    }
    it("Supports withHints") {
      val sj = ScalaJack(MongoFlavor()).withHints(typeOf[Address] -> "addr_kind", typeOf[Demographic] -> "demo")
      val dbo = BsonDocument(
        "demo" -> BsonString("co.blocke.scalajack.mongo.USDemographic"),
        "_id" -> BsonString("34"),
        "address" -> BsonDocument("addr_kind" -> BsonString("co.blocke.scalajack.mongo.USAddress"), "street" -> BsonString("123 Main"), "city" -> BsonString("New York"), "state" -> BsonString("NY"), "postalCode" -> BsonString("39822"))
      )
      dbo.toJson should equal("""{"demo": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"addr_kind": "co.blocke.scalajack.mongo.USAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}""")
      sj.read[Demographic](dbo) should equal(USDemographic("34", USAddress("123 Main", "New York", "NY", "39822")))
    }

    // withHintModifiers tested in another case

    it("Supports withDefaultHint") {
      val sj = ScalaJack(MongoFlavor()).withDefaultHint("kind")
      val dbo = BsonDocument(
        "kind" -> BsonString("co.blocke.scalajack.mongo.USDemographic"),
        "_id" -> BsonString("34"),
        "address" -> BsonDocument("kind" -> BsonString("co.blocke.scalajack.mongo.USAddress"), "street" -> BsonString("123 Main"), "city" -> BsonString("New York"), "state" -> BsonString("NY"), "postalCode" -> BsonString("39822"))
      )
      dbo.toJson should equal("""{"kind": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"kind": "co.blocke.scalajack.mongo.USAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}""")
      sj.read[Demographic](dbo) should equal(USDemographic("34", USAddress("123 Main", "New York", "NY", "39822")))
    }
    it("Provide a default object if the object specified in the type hint is unknown (parseOrElse)") {
      val sj = ScalaJack(MongoFlavor()).parseOrElse((typeOf[Address] -> typeOf[DefaultAddress]))
      val dbo = BsonDocument(
        "_hint" -> BsonString("co.blocke.scalajack.mongo.USDemographic"),
        "_id" -> BsonString("34"),
        "address" -> BsonDocument("_hint" -> BsonString("co.blocke.scalajack.mongo.UnknownAddress"), "street" -> BsonString("123 Main"), "city" -> BsonString("New York"), "state" -> BsonString("NY"), "postalCode" -> BsonString("39822"))
      )
      dbo.toJson should equal("""{"_hint": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"_hint": "co.blocke.scalajack.mongo.UnknownAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}""")
      sj.read[Demographic](dbo) should equal(USDemographic("34", DefaultAddress("39822")))
    }
  }
}