package co.blocke.scalajack
package mongo

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Matchers._
import org.bson._
import scala.collection.JavaConverters._

case class TT2(name: String, rec: Map[String, List[(String, Int, Boolean)]])

class TupleSpec extends AnyFunSpec {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = new BsonDocument(List(
      new BsonElement("name", new BsonString("Larry")),
      new BsonElement("rec", new BsonDocument(List(
        new BsonElement("foo", new BsonArray(List(
          new BsonArray(List(
            new BsonString("a"), new BsonInt32(1), new BsonBoolean(true)
          ).asJava)
        ).asJava)),
        new BsonElement("hey", new BsonArray(List(
          new BsonArray(List(
            new BsonString("x"), new BsonInt32(8), new BsonBoolean(false),
          ).asJava),
          new BsonArray(List(
            new BsonString("r"), new BsonInt32(3), new BsonBoolean(true)
          ).asJava)
        ).asJava))
      ).asJava))
    ).asJava)
  }

  object ScalaMaster {
    val r = List(("a", 1, true))
    val r2 = List(("x", 8, false), ("r", 3, true))
    val a = TT2("Larry", Map("foo" -> r, "hey" -> r2))
  }

  describe("---------------------------\n:  Tuple Tests (MongoDB)  :\n---------------------------") {
    it("Render Tests") {
      sjM.render(ScalaMaster.a) should be(MongoMaster.a)
    }
    it("Read Tests") {
      sjM.read[TT2](MongoMaster.a) should be(ScalaMaster.a)
    }
  }
}