package co.blocke.scalajack
package mongo

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.bson._
import scala.jdk.CollectionConverters._

case class Something(name: String, stuff: Map[String, Any])

class AnySpec extends AnyFunSpec with Matchers {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement(
          "stuff",
          new BsonDocument(
            List(
              new BsonElement("a", new BsonInt32(1)),
              new BsonElement("b", new BsonBoolean(true))
            ).asJava
          )
        )
      ).asJava
    )

    val b = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement(
          "stuff",
          new BsonDocument(
            List(
              new BsonElement("a", new BsonInt32(1)),
              new BsonElement(
                "b",
                new BsonArray(
                  List(new BsonInt32(4), new BsonInt32(5), new BsonInt32(6)).asJava
                )
              )
            ).asJava
          )
        )
      ).asJava
    )

    val c = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement(
          "stuff",
          new BsonDocument(
            List(
              new BsonElement("a", new BsonInt32(1)),
              new BsonElement(
                "b",
                new BsonArray(
                  List(
                    new BsonDocument(
                      List(
                        new BsonElement("x", new BsonString("Fido")),
                        new BsonElement("y", new BsonBoolean(false))
                      ).asJava
                    ),
                    new BsonDocument(
                      List(
                        new BsonElement("x", new BsonString("Cat")),
                        new BsonElement("y", new BsonBoolean(true))
                      ).asJava
                    )
                  ).asJava
                )
              )
            ).asJava
          )
        )
      ).asJava
    )

    val e = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement(
          "stuff",
          new BsonDocument(
            List(
              new BsonElement("a", new BsonInt32(1)),
              new BsonElement(
                "b",
                new BsonArray(
                  List(
                    new BsonString("foo"),
                    new BsonNull(),
                    new BsonString("bar")
                  ).asJava
                )
              )
            ).asJava
          )
        )
      ).asJava
    )

    val f = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement(
          "stuff",
          new BsonDocument(
            List(
              new BsonElement("a", new BsonInt32(1)),
              new BsonElement("b", new BsonDouble(1.23))
            ).asJava
          )
        )
      ).asJava
    )

    val g = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement(
          "stuff",
          new BsonDocument(
            List(
              new BsonElement("a", new BsonInt32(1)),
              new BsonElement("b", new BsonInt64(25L))
            ).asJava
          )
        )
      ).asJava
    )
  }

  object ScalaMaster {
    val a = Something("Fred", Map("a" -> 1, "b" -> true))
    val b = Something("Fred", Map("a" -> 1, "b" -> List(4, 5, 6)))
    val c = Something(
      "Fred",
      Map(
        "a" -> 1,
        "b" -> List(
          Map("x" -> "Fido", "y" -> false),
          Map("x" -> "Cat", "y" -> true)
        )
      )
    )
    val e = Something("Fred", Map("a" -> 1, "b" -> List("foo", null, "bar")))
    val f = Something("Fred", Map("a" -> 1, "b" -> 1.23))
    val g = Something("Fred", Map("a" -> 1, "b" -> 25L))
  }

  describe(
    "-------------------------\n:  Any Tests (MongoDB)  :\n-------------------------"
  ) {
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
