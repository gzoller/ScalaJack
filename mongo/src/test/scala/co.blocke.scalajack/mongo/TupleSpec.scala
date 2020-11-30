package co.blocke.scalajack
package mongo

import TestUtil._
import munit._
import munit.internal.console
import org.bson._
import scala.jdk.CollectionConverters._

case class TT2(name: String, rec: Map[String, List[(String, Int, Boolean)]])

class TupleSpec extends FunSuite:

  val sjM = ScalaJack(MongoFlavor())

  object ScalaMaster {
    val r = List(("a", 1, true))
    val r2 = List(("x", 8, false), ("r", 3, true))
    val a = TT2("Larry", Map("foo" -> r, "hey" -> r2))
  }
  
  object MongoMaster {
    val a = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Larry")),
        new BsonElement(
          "rec",
          new BsonDocument(
            List(
              new BsonElement(
                "foo",
                new BsonArray(
                  List(
                    new BsonArray(
                      List(
                        new BsonString("a"),
                        new BsonInt32(1),
                        new BsonBoolean(true)
                      ).asJava
                    )
                  ).asJava
                )
              ),
              new BsonElement(
                "hey",
                new BsonArray(
                  List(
                    new BsonArray(
                      List(
                        new BsonString("x"),
                        new BsonInt32(8),
                        new BsonBoolean(false),
                      ).asJava
                    ),
                    new BsonArray(
                      List(
                        new BsonString("r"),
                        new BsonInt32(3),
                        new BsonBoolean(true)
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
  }

  test("Render Tests") {
    describe(
      "---------------------------\n:  Tuple Tests (MongoDB)  :\n---------------------------"
    )
    assertEquals(sjM.render(ScalaMaster.a), MongoMaster.a)
  }

  test("Read Tests") {
    assertEquals(sjM.read[TT2](MongoMaster.a), ScalaMaster.a)
  }