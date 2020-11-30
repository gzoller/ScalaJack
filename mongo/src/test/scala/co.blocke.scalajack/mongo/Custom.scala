package co.blocke.scalajack
package mongo

import TestUtil._
import munit._
import munit.internal.console
import org.bson._
import scala.jdk.CollectionConverters._
import co.blocke.scala_reflection.RType

class Custom extends FunSuite:

  test("Supports withAdapters") {
    describe(
      "----------------------------\n:  Custom Tests (MongoDB) :\n----------------------------", Console.BLUE
    )
    val sj = ScalaJack(MongoFlavor()).withAdapters(PhoneAdapter)
    val dbo = new BsonDocument(
      List(
        new BsonElement("_id", new BsonString("Fred")),
        new BsonElement("phone", new BsonString("123-456-7890"))
      ).asJava
    )
    assertEquals(dbo.toJson, """{"_id": "Fred", "phone": "123-456-7890"}""")
    val read = sj.read[Person](dbo)
    assertEquals(read, Person("Fred", "1234567890".asInstanceOf[Phone]))
    assertEquals(sj.render(read), dbo)
  }

  test("Supports withHints") {
    val sj = ScalaJack(MongoFlavor()).withHints(
      RType.of[Address] -> "addr_kind",
      RType.of[Demographic] -> "demo"
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
    assertEquals(dbo.toJson, 
      """{"demo": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"addr_kind": "co.blocke.scalajack.mongo.USAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}"""
    )
    val read = sj.read[Demographic](dbo)
    assertEquals(read, USDemographic("34", USAddress("123 Main", "New York", "NY", "39822")))
    assertEquals(sj.render(read), dbo)
  }

  // withHintModifiers tested in another case

  test("Supports withDefaultHint") {
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
    assertEquals(dbo.toJson, 
      """{"kind": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"kind": "co.blocke.scalajack.mongo.USAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}"""
    )
    val read = sj.read[Demographic](dbo)
    assertEquals(read, USDemographic("34", USAddress("123 Main", "New York", "NY", "39822")) )
    assertEquals(sj.render(read), dbo)
  }

  test(
    "Provide a default object if the object specified in the type hint is unknown (parseOrElse)"
  ) {
      val sj = ScalaJack(MongoFlavor())
        .parseOrElse((RType.of[Address] -> RType.of[DefaultAddress]))
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
      val dboDefault = new BsonDocument(
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
                  new BsonString("co.blocke.scalajack.mongo.DefaultAddress")
                ),
                new BsonElement("postalCode", new BsonString("39822"))
              ).asJava
            )
          )
        ).asJava
      )
      assertEquals(dbo.toJson, 
        """{"_hint": "co.blocke.scalajack.mongo.USDemographic", "_id": "34", "address": {"_hint": "co.blocke.scalajack.mongo.UnknownAddress", "street": "123 Main", "city": "New York", "state": "NY", "postalCode": "39822"}}"""
      )
      val read = sj.read[Demographic](dbo)
      assertEquals(read, USDemographic("34", DefaultAddress("39822")))
      assertEquals(sj.render(read), dboDefault)
  }
