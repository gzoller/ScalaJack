package co.blocke.scalajack
package mongo

import co.blocke.scalajack.model.JackFlavor
import TestUtil._
import munit._
import munit.internal.console
import org.bson._
import JsonMatcher._
import java.time.{ OffsetDateTime, ZonedDateTime }

import scala.jdk.CollectionConverters._

case class MyNumbers(d: BigDecimal, n: Number)
case class NumberBoom(x: BigInt)

class LooseChange extends FunSuite:
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

  test("Handles null value") {
    describe(
      "----------------------------\n:  Loose Change (MongoDB) :\n----------------------------"
    )
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
    assertEquals(sjM.read[Something](dbo), 
      Something("Fred", Map("a" -> 1, "b" -> 15))
    )
  }

  test("Should blow up for unsupported BSON type") {
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
    interceptMessage[ScalaJackError]("""BSON type org.bson.BsonJavaScript is not currently supported in ScalaJack."""){
      sjM.read[Something](dbo)
    }
  }

  test("Field name remapping must work") {
    val mfp = MapFactor("wonder", 25L, 3, "hungry")
    val dbo = sjM.render(mfp)
    assertEquals(dbo.asDocument.toJson, 
      """{"foo_bar": "wonder", "a_b": {"$numberLong": "25"}, "count": 3, "big_mac": "hungry"}"""
    )
    assertEquals(sjM.read[MapFactor](dbo), mfp)
  }

  test("Field name remapping on dbkey must work") {
    // val mfp = MapFactorId2("wonder", 25L, 1, 3)
    val mfp = MapFactorId("wonder", 25L, 3, "hungry")
    val dbo = sjM.render(mfp)
    assertEquals(dbo.asDocument.toJson, 
      """{"_id": "wonder", "a_b": {"$numberLong": "25"}, "count": 3, "big_mac": "hungry"}"""
    )
    assertEquals(sjM.read[MapFactorId](dbo), mfp)
  }

  test("Field name remapping on dbkey with multi-part keys must work") {
    val mfp = MapFactorId2("wonder", 25L, 1, 3, "hungry")
    val dbo = sjM.render(mfp)
    assert(jsonMatches(
      dbo.asDocument.toJson.asInstanceOf[co.blocke.scalajack.json.JSON], 
      """{"_id": {"foo_bar": "wonder", "a_b": {"$numberLong": "25"}, "hey": 1}, "count": 3, "big_mac": "hungry"}""".asInstanceOf[co.blocke.scalajack.json.JSON]
      ))
    assertEquals(sjM.read[MapFactorId2](dbo), mfp)
  }

  test("Number, Decimal, and BigInt handling") {
    val my = MyNumbers(BigDecimal(123.45), 15)
    val d = sjM.render(my)
    assertEquals(d.asDocument.toJson, 
      """{"d": {"$numberDecimal": "123.45"}, "n": 15}"""
    )
    assertEquals(sjM.read[MyNumbers](d), my)

    val boom = NumberBoom(BigInt(5))
    interceptMessage[ScalaJackError]("BigInt is currently an unsupported datatype for MongoDB serialization"){
      sjM.render(boom)
    }
  }

  test("Nulls") {
    val prim = PrimitiveLists(null, null, null, null, null)
    assertEquals(sjM.read[PrimitiveLists](sjM.render(prim)), prim)

    val os = OneSub2("foo", flipflop = false, null)
    interceptMessage[ScalaJackError]("Class co.blocke.scalajack.mongo.PrimitiveLists missing required fields: bools, chars, doubles, ints, longs"){
      sjM.read[PrimitiveLists](sjM.render(os))
    }

    val os2 =
      OneSub2("foo", flipflop = false, Map(null.asInstanceOf[String] -> 5))
    interceptMessage[ScalaJackError]("Map keys cannot be null."){
      sjM.render(os2)
    }

    val out = null
    assertEquals(sjM.render[OneSub2](out).isNull, true)

    val mapDoc = new BsonDocument(
      List(
        new BsonElement("i", new BsonInt32(5)),
        new BsonElement("items", new BsonNull())
      ).asJava
    )
    assertEquals(sjM.read[BagMap[Int]](mapDoc), BagMap(5, null))

    assertEquals(sjM.read[OneSub2](null), null)

    val times = Times(null, null)
    val d = sjM.render(times)
    assertEquals(d.asDocument().toJson, """{"offset": null, "zoned": null}""")
    assertEquals(sjM.read[Times](d), times)
    val d2 = new BsonDocument(
      List(
        new BsonElement("offset", new BsonNull()),
        new BsonElement("zoned", new BsonNull())
      ).asJava
    )
    assertEquals(sjM.read[Times](d2), times)
  }

  test("Good time values") {
    val t = Times(
      OffsetDateTime.parse("2007-12-03T10:15:30+01:00"),
      ZonedDateTime.parse("2007-12-03T10:15:30Z[UTC]")
    )
    val d = sjM.render(t)
    val t2 = sjM.read[Times](d)
    // Extra gymnastics because the read/rendered OffsetDateTime, while the same actual instant, isn't the same value so comparison fails
    assert(t2.offset.atZoneSameInstant(java.time.ZoneId.of("Europe/Paris")) == t.offset.atZoneSameInstant(java.time.ZoneId.of("Europe/Paris")))
    assertEquals(t2.zoned == t.zoned, true)
  }

  test("Bad expected values") {
    val d = new BsonDocument(
      List(
        new BsonElement("name", new BsonString("Fred")),
        new BsonElement("big", new BsonDouble(123.45))
      ).asJava
    )
    interceptMessage[ScalaJackError]("Cannot parse an Long from value"){
      sjM.read[OneSub1](d)
    }
  }

  test("Non-hint hint-labeled field") {
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
    interceptMessage[ScalaJackError]("Hint value _hint must be a string value"){
      sjM.read[StrangeWrapper](d)
    }
  }

  test("Can't find trait hint") {
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
    interceptMessage[ScalaJackError]("Type hint '_hint' not found"){
      sjM.read[StrangeWrapper](d)
    }
  }

  test("Enums as ints work") {
    val sj = ScalaJack(MongoFlavor()).enumsAsInts()
    val n = Numy(5, Num.B)
    val d = sj.render(n)
    assertEquals(d.asDocument.toJson, """{"age": 5, "num": 1}""")
    assertEquals(sj.read[Numy](d), n)
  }

  test("Failure due to empty BsonBuilder") {
    interceptMessage[ScalaJackError]("No value set for internal mongo builder"){
      BsonBuilder().result()
    }
  }

  test("Parse only") {
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
    assertEquals(p.isInstanceOf[BsonParser], true)
  }