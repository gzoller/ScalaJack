package co.blocke.scalajack
package mongo

import model._
import co.blocke.scalajack.util.Path
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Matchers._
import org.bson._
import compat.BsonBuilder

import scala.collection.JavaConverters._

case class MyNumbers(d: BigDecimal, n: Number)
case class NumberBoom(x: BigInt)

class LooseChange extends AnyFunSpec {
  val sjM = ScalaJack(MongoFlavor())

  object MongoMaster {
    val a = new BsonDocument(List(
      new BsonElement("name", new BsonString("Fred")),
      new BsonElement("stuff", new BsonDocument(List(
        new BsonElement("a", new BsonInt32(1)),
        new BsonElement("b", new BsonBoolean(true))).asJava))).asJava)

    val b = new BsonDocument(List(
      new BsonElement("name", new BsonString("Fred")),
      new BsonElement("stuff", new BsonDocument(List(
        new BsonElement("a", new BsonInt32(1)),
        new BsonElement("b", new BsonArray(List(
          new BsonInt32(4), new BsonInt32(5), new BsonInt32(6)).asJava))).asJava))).asJava)

    val c = new BsonDocument(List(
      new BsonElement("name", new BsonString("Fred")),
      new BsonElement("stuff", new BsonDocument(List(
        new BsonElement("a", new BsonInt32(1)),
        new BsonElement("b", new BsonArray(List(
          new BsonDocument(List(
            new BsonElement("x", new BsonString("Fido")),
            new BsonElement("y", new BsonBoolean(false))).asJava),
          new BsonDocument(List(
            new BsonElement("x", new BsonString("Cat")),
            new BsonElement("y", new BsonBoolean(true))).asJava)).asJava))).asJava))).asJava)

    val e = new BsonDocument(List(
      new BsonElement("name", new BsonString("Fred")),
      new BsonElement("stuff", new BsonDocument(List(
        new BsonElement("a", new BsonInt32(1)),
        new BsonElement("b", new BsonArray(List(
          new BsonString("foo"), new BsonNull(), new BsonString("bar")).asJava))).asJava))).asJava)

    val f = new BsonDocument(List(
      new BsonElement("name", new BsonString("Fred")),
      new BsonElement("stuff", new BsonDocument(List(
        new BsonElement("a", new BsonInt32(1)),
        new BsonElement("b", new BsonDouble(1.23))).asJava))).asJava)

    val g = new BsonDocument(List(
      new BsonElement("name", new BsonString("Fred")),
      new BsonElement("stuff", new BsonDocument(List(
        new BsonElement("a", new BsonInt32(1)),
        new BsonElement("b", new BsonInt64(25L))).asJava))).asJava)
  }

  object ScalaMaster {
    val a = Something("Fred", Map("a" -> 1, "b" -> true))
    val b = Something("Fred", Map("a" -> 1, "b" -> List(4, 5, 6)))
    val c = Something("Fred", Map("a" -> 1, "b" -> List(Map("x" -> "Fido", "y" -> false), Map("x" -> "Cat", "y" -> true))))
    val e = Something("Fred", Map("a" -> 1, "b" -> List("foo", null, "bar")))
    val f = Something("Fred", Map("a" -> 1, "b" -> 1.23))
    val g = Something("Fred", Map("a" -> 1, "b" -> 25L))
  }

  describe("----------------------------\n:  Loose Change (MongoDB) :\n----------------------------") {
    it("Handles null value") {
      val dbo = new BsonDocument(List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement("stuff", new BsonDocument(List(
          new BsonElement("a", new BsonInt32(1)),
          new BsonElement("b", new BsonInt32(15))).asJava))).asJava)
      sjM.read[Something](dbo) should be(Something("Fred", Map("a" -> 1, "b" -> 15)))
    }
    it("Should blow up for unsupported BSON type") {
      val dbo = new BsonDocument(List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement("stuff", new BsonDocument(List(
          new BsonElement("a", new BsonJavaScript("code here"))).asJava))).asJava)
      the[SJError] thrownBy sjM.read[Something](dbo) should have message """BSON type org.bson.BsonJavaScript is not currently supported in ScalaJack."""
    }
    it("Field name remapping must work") {
      val mfp = MapFactor("wonder", 25L, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.asDocument.toJson should be("""{"foo_bar": "wonder", "a_b": {"$numberLong": "25"}, "count": 3, "big_mac": "hungry"}""")
      sjM.read[MapFactor](dbo) should be(mfp)
    }
    it("Field name remapping on dbkey must work") {
      // val mfp = MapFactorId2("wonder", 25L, 1, 3)
      val mfp = MapFactorId("wonder", 25L, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.asDocument.toJson should be("""{"_id": "wonder", "a_b": {"$numberLong": "25"}, "count": 3, "big_mac": "hungry"}""")
      sjM.read[MapFactorId](dbo) should be(mfp)
    }
    it("Field name remapping on dbkey with multi-part keys must work") {
      val mfp = MapFactorId2("wonder", 25L, 1, 3, "hungry")
      val dbo = sjM.render(mfp)
      dbo.asDocument.toJson should be("""{"_id": {"foo_bar": "wonder", "a_b": {"$numberLong": "25"}, "hey": 1}, "count": 3, "big_mac": "hungry"}""")
      sjM.read[MapFactorId2](dbo) should be(mfp)
    }
    it("Number, Decimal, and BigInt handling") {
      val my = MyNumbers(BigDecimal(123.45), 15)
      val d = sjM.render(my)
      d.asDocument.toJson should be("""{"d": {"$numberDecimal": "123.45"}, "n": 15}""")
      sjM.read[MyNumbers](d) should be(my)

      val boom = NumberBoom(BigInt(5))
      the[SJError] thrownBy sjM.render(boom) should have message "BigInt is currently an unsupported datatype for MongoDB serialization"
    }
    it("Skip Object (read)") {
      val c = Carry("carry me", Wrap("my name", true, "info"))
      val reader = sjM.parse(sjM.render(c))
      reader.next // skip BeginObject
      reader.next // skip name label
      reader.next // skip name value
      reader.next // skip wrap label
      reader.skipObject(Path.Root)
      reader.head.tokenType should be(TokenType.EndObject)
      reader.next
      reader.head.tokenType should be(TokenType.End)
      reader.next
      reader.hasNext should be(false)

      reader.reset()
      reader.next
      reader.skipObject(Path.Root)
      reader.hasNext should be(true)
      reader.head.tokenType should be(TokenType.String)

      reader.reset()
      reader.back
      reader.head.tokenType should be(TokenType.BeginObject)
    }
    it("Nulls") {
      val prim = PrimitiveLists(null, null, null, null, null)
      sjM.read[PrimitiveLists](sjM.render(prim)) should be(prim)

      val os = OneSub2("foo", false, null)
      the[ReadMissingError] thrownBy sjM.read[PrimitiveLists](sjM.render(os)) should have message "[$]: Class PrimitiveLists missing field ints"

      val os2 = OneSub2("foo", false, Map(null.asInstanceOf[String] -> 5))
      the[SJError] thrownBy sjM.render(os2) should have message "Map keys cannot be null."

      val out = null
      sjM.render[OneSub2](out).isNull should be(true)

      //case class BagMap[Y](i: Int, items: Map[String, Y])
      val mapDoc = new BsonDocument(List(
        new BsonElement("i", new BsonInt32(5)),
        new BsonElement("items", new BsonNull())).asJava)
      sjM.read[BagMap[Int]](mapDoc) should be(BagMap(5, null))

      sjM.read[OneSub2](null) should be(null)

      //case class Times( offset: OffsetDateTime, zoned: ZonedDateTime )
      val times = Times(null, null)
      val d = sjM.render(times)
      d.asDocument().toJson should be("""{"offset": null, "zoned": null}""")
      sjM.read[Times](d) should be(times)
      val d2 = new BsonDocument(List(
        new BsonElement("offset", new BsonNull()),
        new BsonElement("zoned", new BsonNull())).asJava)
      sjM.read[Times](d2) should be(times)
    }
    it("Overrun tuple") {
      //case class Tupple( t: (String,Int))
      val d = new BsonDocument(List(
        new BsonElement("t", new BsonArray(List(
          new BsonString("foo"), new BsonInt32(5), new BsonBoolean(true)).asJava))).asJava)
      the[ReadUnexpectedError] thrownBy sjM.read[Tuple](d) should have message "[$.t]: Expected EndArray here but found Boolean"
    }
    it("Bad expected values") {
      val d = new BsonDocument(List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement("big", new BsonDouble(123.45))).asJava)
      the[ReadUnexpectedError] thrownBy sjM.read[OneSub1](d) should have message "[$.big]: Expected Number of kind Int64 here but found Number of kind Double"
    }
    it("Non-hint hint-labeled field") {
      val d = new BsonDocument(List(
        new BsonElement("num", new BsonInt32(3)),
        new BsonElement("s", new BsonDocument(List(
          new BsonElement("_hint", new BsonInt32(45)),
          new BsonElement("size", new BsonInt32(34))).asJava))).asJava)
      the[ReadInvalidError] thrownBy sjM.read[StrangeWrapper](d) should have message "[$.s._hint]: Couldn't find expected type hint '_hint' for trait co.blocke.scalajack.mongo.Strange"
    }
    it("Can't find trait hint") {
      val d = new BsonDocument(List(
        new BsonElement("num", new BsonInt32(3)),
        new BsonElement("s", new BsonDocument(List(
          new BsonElement("size", new BsonInt32(34))).asJava))).asJava)
      the[ReadInvalidError] thrownBy sjM.read[StrangeWrapper](d) should have message "[$.s._hint]: Couldn't find expected type hint '_hint' for trait co.blocke.scalajack.mongo.Strange"
    }
    it("Enums as ints work") {
      val sj = ScalaJack(MongoFlavor()).enumsAsInts()
      val n = Numy(5, Num.B)
      val d = sj.render(n)
      d.asDocument.toJson should be("""{"age": 5, "num": 1}""")
      sj.read[Numy](d) should be(n)
    }
    it("Failure due to empty BsonBuilder") {
      the[SJError] thrownBy BsonBuilder().result() should have message "No value set for internal mongo builder"
    }
  }
}