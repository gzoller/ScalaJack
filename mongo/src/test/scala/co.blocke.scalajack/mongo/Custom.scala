package co.blocke.scalajack
package mongo

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.bson._
import scala.reflect.runtime.universe.typeOf
import scala.collection.JavaConverters._

class Custom extends AnyFunSpec with Matchers {

  describe(
    "----------------------------\n:  Custom Tests (MongoDB) :\n----------------------------"
  ) {

      it("Supports withAdapters") {
        val sj = ScalaJack(MongoFlavor()).withAdapters(PhoneAdapter)
        val dbo = new BsonDocument(
          List(
            new BsonElement("_id", new BsonString("Fred")),
            new BsonElement("phone", new BsonString("123-456-7890"))
          ).asJava
        )
        dbo.toJson should equal("""{"_id": "Fred", "phone": "123-456-7890"}""")
        sj.read[Person](dbo) should equal(Person("Fred", "1234567890"))
      }
      it("Supports withHints") {
        val sj = ScalaJack(MongoFlavor()).withHints(
          typeOf[Address] -> "addr_kind",
          typeOf[Demographic] -> "demo"
        )
        val dbo = new BsonDocument(
          List(
            new BsonElement(
              "demo",
              new BsonString("co.blocke.scalajack.mongo.USDemographic")
            ),
            new BsonElement("_id", new BsonString("34")),
            new BsonElement(
              "address",
              new BsonDocument(
                List(
                  new BsonElement(
                    "addr_kind",
                    new BsonString("co.blocke.scalajack.mongo.USAddress")
                  ),
                  new BsonElement("street", new BsonString("123 Main")),
                  new BsonElement("city", new BsonString("New York")),
                  new BsonElement("state", new BsonString("NY")),
                  new BsonElement("postalCode", new BsonString("39822"))
                ).asJava
              )
            )
          ).asJava
        )
        dbo.toJson should equal(
          """{"demo": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"addr_kind": "co.blocke.scalajack.mongo.USAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}"""
        )
        sj.read[Demographic](dbo) should equal(
          USDemographic("34", USAddress("123 Main", "New York", "NY", "39822"))
        )
      }

      // withHintModifiers tested in another case

      it("Supports withDefaultHint") {
        val sj = ScalaJack(MongoFlavor()).withDefaultHint("kind")
        val dbo = new BsonDocument(
          List(
            new BsonElement(
              "kind",
              new BsonString("co.blocke.scalajack.mongo.USDemographic")
            ),
            new BsonElement("_id", new BsonString("34")),
            new BsonElement(
              "address",
              new BsonDocument(
                List(
                  new BsonElement(
                    "kind",
                    new BsonString("co.blocke.scalajack.mongo.USAddress")
                  ),
                  new BsonElement("street", new BsonString("123 Main")),
                  new BsonElement("city", new BsonString("New York")),
                  new BsonElement("state", new BsonString("NY")),
                  new BsonElement("postalCode", new BsonString("39822"))
                ).asJava
              )
            )
          ).asJava
        )
        dbo.toJson should equal(
          """{"kind": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"kind": "co.blocke.scalajack.mongo.USAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}"""
        )
        sj.read[Demographic](dbo) should equal(
          USDemographic("34", USAddress("123 Main", "New York", "NY", "39822"))
        )
      }
      it(
        "Provide a default object if the object specified in the type hint is unknown (parseOrElse)"
      ) {
          val sj = ScalaJack(MongoFlavor())
            .parseOrElse((typeOf[Address] -> typeOf[DefaultAddress]))
          val dbo = new BsonDocument(
            List(
              new BsonElement(
                "_hint",
                new BsonString("co.blocke.scalajack.mongo.USDemographic")
              ),
              new BsonElement("_id", new BsonString("34")),
              new BsonElement(
                "address",
                new BsonDocument(
                  List(
                    new BsonElement(
                      "_hint",
                      new BsonString("co.blocke.scalajack.mongo.UnknownAddress")
                    ),
                    new BsonElement("street", new BsonString("123 Main")),
                    new BsonElement("city", new BsonString("New York")),
                    new BsonElement("state", new BsonString("NY")),
                    new BsonElement("postalCode", new BsonString("39822"))
                  ).asJava
                )
              )
            ).asJava
          )
          dbo.toJson should equal(
            """{"_hint": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"_hint": "co.blocke.scalajack.mongo.UnknownAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}"""
          )
          sj.read[Demographic](dbo) should equal(
            USDemographic("34", DefaultAddress("39822"))
          )
        }
    }
}
