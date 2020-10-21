package co.blocke.scalajack
package mongo

import TestUtil._
import munit._
import munit.internal.console
import org.bson._
import scala.jdk.CollectionConverters._

case class Something(name: String, stuff: Map[String, Any])

class AnySpec extends FunSuite:

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

  test("Any 1") {
    describe(
      "-------------------------\n:  Any Tests (MongoDB)  :\n-------------------------", Console.BLUE
    )
    describe("Render Tests")
    assertEquals(sjM.render(ScalaMaster.a), MongoMaster.a)
  }

  test("Any 2") {
    assertEquals(sjM.render(ScalaMaster.b), MongoMaster.b)
  }

  test("Any 3") {
    assertEquals(sjM.render(ScalaMaster.c), MongoMaster.c)
  }

  test("Any 4") {
    assertEquals(sjM.render(ScalaMaster.e), MongoMaster.e)
  }

  test("Any 5") {
    assertEquals(sjM.render(ScalaMaster.f), MongoMaster.f)
  }

  test("Any 6") {
    assertEquals(sjM.render(ScalaMaster.g), MongoMaster.g)
  }

  test("Any 1") {
    describe("Read Tests")
    assertEquals(sjM.read[Something](MongoMaster.a), ScalaMaster.a)
  }

  test("Any 2") {
    assertEquals(sjM.read[Something](MongoMaster.b), ScalaMaster.b)
  }

  test("Any 3") {
    assertEquals(sjM.read[Something](MongoMaster.c), ScalaMaster.c)
  }

  test("Any 4") {
    assertEquals(sjM.read[Something](MongoMaster.e), ScalaMaster.e)
  }

  test("Any 5") {
    assertEquals(sjM.read[Something](MongoMaster.f), ScalaMaster.f)
  }

  test("Any 6") {
    assertEquals(sjM.read[Something](MongoMaster.g), ScalaMaster.g)
  }
