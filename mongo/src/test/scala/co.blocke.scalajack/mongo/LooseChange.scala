package co.blocke.scalajack
package mongo

import co.blocke.scalajack.model.JackFlavor
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.bson._
import compat.BsonBuilder
import JsonMatcher._
import java.time.{ OffsetDateTime, ZonedDateTime }

import scala.collection.JavaConverters._

case class MyNumbers(d: BigDecimal, n: Number)
case class NumberBoom(x: BigInt)

class LooseChange extends AnyFunSpec with Matchers {
  val sjM: JackFlavor[BsonValue] = ScalaJack(MongoFlavor())

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
    val a: Something = Something("Fred", Map("a" -> 1, "b" -> true))
    val b: Something = Something("Fred", Map("a" -> 1, "b" -> List(4, 5, 6)))
    val c: Something = Something(
      "Fred",
      Map(
        "a" -> 1,
        "b" -> List(
          Map("x" -> "Fido", "y" -> false),
          Map("x" -> "Cat", "y" -> true)
        )
      )
    )
    val e: Something =
      Something("Fred", Map("a" -> 1, "b" -> List("foo", null, "bar")))
    val f: Something = Something("Fred", Map("a" -> 1, "b" -> 1.23))
    val g: Something = Something("Fred", Map("a" -> 1, "b" -> 25L))
  }

  describe(
    "----------------------------\n:  Loose Change (MongoDB) :\n----------------------------"
  ) {
      it("Handles null value") {
        val dbo = new BsonDocument(
          List(
            new BsonElement("name", new BsonString("Fred")),
            new BsonElement(
              "stuff",
              new BsonDocument(
                List(
                  new BsonElement("a", new BsonInt32(1)),
                  new BsonElement("b", new BsonInt32(15))
                ).asJava
              )
            )
          ).asJava
        )
        sjM.read[Something](dbo) should be(
          Something("Fred", Map("a" -> 1, "b" -> 15))
        )
      }
      it("Should blow up for unsupported BSON type") {
        val dbo = new BsonDocument(
          List(
            new BsonElement("name", new BsonString("Fred")),
            new BsonElement(
              "stuff",
              new BsonDocument(
                List(new BsonElement("a", new BsonJavaScript("code here"))).asJava
              )
            )
          ).asJava
        )
        the[ScalaJackError] thrownBy sjM.read[Something](dbo) should have message """BSON type org.bson.BsonJavaScript is not currently supported in ScalaJack."""
      }
      it("Field name remapping must work") {
        val mfp = MapFactor("wonder", 25L, 3, "hungry")
        val dbo = sjM.render(mfp)
        dbo.asDocument.toJson should be(
          """{"foo_bar": "wonder", "a_b": {"$numberLong": "25"}, "count": 3, "big_mac": "hungry"}"""
        )
        sjM.read[MapFactor](dbo) should be(mfp)
      }
      it("Field name remapping on dbkey must work") {
        // val mfp = MapFactorId2("wonder", 25L, 1, 3)
        val mfp = MapFactorId("wonder", 25L, 3, "hungry")
        val dbo = sjM.render(mfp)
        dbo.asDocument.toJson should be(
          """{"_id": "wonder", "a_b": {"$numberLong": "25"}, "count": 3, "big_mac": "hungry"}"""
        )
        sjM.read[MapFactorId](dbo) should be(mfp)
      }
      it("Field name remapping on dbkey with multi-part keys must work") {
        val mfp = MapFactorId2("wonder", 25L, 1, 3, "hungry")
        val dbo = sjM.render(mfp)
        parseJValue(dbo.asDocument.toJson) should matchJson(
          parseJValue(
            """{"_id": {"foo_bar": "wonder", "a_b": {"$numberLong": "25"}, "hey": 1}, "count": 3, "big_mac": "hungry"}"""
          )
        )
        sjM.read[MapFactorId2](dbo) should be(mfp)
      }
      it("Number, Decimal, and BigInt handling") {
        val my = MyNumbers(BigDecimal(123.45), 15)
        val d = sjM.render(my)
        d.asDocument.toJson should be(
          """{"d": {"$numberDecimal": "123.45"}, "n": 15}"""
        )
        sjM.read[MyNumbers](d) should be(my)

        val boom = NumberBoom(BigInt(5))
        the[ScalaJackError] thrownBy sjM.render(boom) should have message "BigInt is currently an unsupported datatype for MongoDB serialization"
      }
      /*
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
     */
      it("Nulls") {
        val prim = PrimitiveLists(null, null, null, null, null)
        sjM.read[PrimitiveLists](sjM.render(prim)) should be(prim)

        val os = OneSub2("foo", flipflop = false, null)
        the[ScalaJackError] thrownBy sjM.read[PrimitiveLists](sjM.render(os)) should have message "Class PrimitiveLists missing required fields: bools, chars, doubles, ints, longs"

        val os2 =
          OneSub2("foo", flipflop = false, Map(null.asInstanceOf[String] -> 5))
        the[ScalaJackError] thrownBy sjM.render(os2) should have message "Map keys cannot be null."

        val out = null
        sjM.render[OneSub2](out).isNull should be(true)

        val mapDoc = new BsonDocument(
          List(
            new BsonElement("i", new BsonInt32(5)),
            new BsonElement("items", new BsonNull())
          ).asJava
        )
        sjM.read[BagMap[Int]](mapDoc) should be(BagMap(5, null))

        sjM.read[OneSub2](null) should be(null)

        val times = Times(null, null)
        val d = sjM.render(times)
        d.asDocument().toJson should be("""{"offset": null, "zoned": null}""")
        sjM.read[Times](d) should be(times)
        val d2 = new BsonDocument(
          List(
            new BsonElement("offset", new BsonNull()),
            new BsonElement("zoned", new BsonNull())
          ).asJava
        )
        sjM.read[Times](d2) should be(times)
      }
      it("Good time values") {
        val t = Times(
          OffsetDateTime.parse("2007-12-03T10:15:30+01:00"),
          ZonedDateTime.parse("2007-12-03T10:15:30Z[UTC]")
        )
        val d = sjM.render(t)
        val t2 = sjM.read[Times](d)
        // Extra gymnastics because the read/rendered OffsetDateTime, while the same actual instant, isn't the same value so comparison fails
        t2.offset.atZoneSameInstant(java.time.ZoneId.of("Europe/Paris")) == t.offset
          .atZoneSameInstant(java.time.ZoneId.of("Europe/Paris")) should be(true)
        t2.zoned == t.zoned should be(true)
      }
      it("Bad expected values") {
        val d = new BsonDocument(
          List(
            new BsonElement("name", new BsonString("Fred")),
            new BsonElement("big", new BsonDouble(123.45))
          ).asJava
        )
        the[java.lang.NumberFormatException] thrownBy sjM.read[OneSub1](d) should have message "For input string: \"123.45\""
      }
      it("Non-hint hint-labeled field") {
        val d = new BsonDocument(
          List(
            new BsonElement("num", new BsonInt32(3)),
            new BsonElement(
              "s",
              new BsonDocument(
                List(
                  new BsonElement("_hint", new BsonInt32(45)),
                  new BsonElement("size", new BsonInt32(34))
                ).asJava
              )
            )
          ).asJava
        )
        the[ScalaJackError] thrownBy sjM.read[StrangeWrapper](d) should have message "Hint value _hint must be a string value"
      }
      it("Can't find trait hint") {
        val d = new BsonDocument(
          List(
            new BsonElement("num", new BsonInt32(3)),
            new BsonElement(
              "s",
              new BsonDocument(
                List(new BsonElement("size", new BsonInt32(34))).asJava
              )
            )
          ).asJava
        )
        the[ScalaJackError] thrownBy sjM.read[StrangeWrapper](d) should have message "Type hint '_hint' not found"
      }
      it("Enums as ints work") {
        val sj = ScalaJack(MongoFlavor()).enumsAsInts()
        val n = Numy(5, Num.B)
        val d = sj.render(n)
        d.asDocument.toJson should be("""{"age": 5, "num": 1}""")
        sj.read[Numy](d) should be(n)
      }
      it("Failure due to empty BsonBuilder") {
        the[ScalaJackError] thrownBy BsonBuilder()
          .result() should have message "No value set for internal mongo builder"
      }
      it("Parse only") {
        val d = new BsonDocument(
          List(
            new BsonElement("num", new BsonInt32(3)),
            new BsonElement(
              "s",
              new BsonDocument(
                List(new BsonElement("size", new BsonInt32(34))).asJava
              )
            )
          ).asJava
        )
        val p = sjM.parse(d)
        p.isInstanceOf[BsonParser] should be(true)
      }
    }
}
