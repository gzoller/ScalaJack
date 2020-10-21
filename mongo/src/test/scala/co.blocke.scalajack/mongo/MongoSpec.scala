package co.blocke.scalajack
package mongo

import model._

import java.time._
import java.util.UUID

import org.bson._
import org.bson.types.ObjectId
import TestUtil._
import munit._
import munit.internal.console
import scala.jdk.CollectionConverters._
import co.blocke.scala_reflection.RType

class WrappedOffsetDateTime(val offsetDateTime: OffsetDateTime) extends AnyVal

class MongoSpec extends FunSuite:

  val TRUE = true
  val FALSE = false

  val data = One(
    "Greg",
    List("a", "b"),
    List(Two("x", FALSE), Two("y", TRUE)),
    Two("Nest!", TRUE),
    Some("wow"),
    Map("hey" -> 17, "you" -> 21),
    TRUE,
    99123986123L,
    Num.C,
    46
  )

  def mongoScalaJack: JackFlavor[BsonValue] = ScalaJack(MongoFlavor())

  test("Naked Map support") {
    describe(
      "---------------------------\n:  Mongo Tests (MongoDB)  :\n---------------------------", Console.BLUE
    )
    describe("Prinitives")
    val li = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val dbo: BsonValue = mongoScalaJack.render(li)
    assertEquals(dbo.asDocument.toJson, """{"a": 1, "b": 2, "c": 3}""")
    assertEquals(mongoScalaJack.read[Map[String, Int]](dbo), li)
  }

  test("UUID support") {
    val thing = UuidThing(
      "Foo",
      UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),
      List(
        UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),
        UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")
      ),
      Some(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"))
    )
    val dbo = mongoScalaJack.render(thing)
    assertEquals(dbo.asDocument.toJson, 
      """{"name": "Foo", "uuid": "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c", "many": ["1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c", "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"], "maybe": "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"}"""
    )
    val b = mongoScalaJack.read[UuidThing](dbo)
    assertEquals(b, thing)
  }

  test("Misc number primitives support") {
    val inst = Loose('A', 1.23F, 15.toShort, 3.toByte)
    val dbo = mongoScalaJack.render(inst)
    assertEquals(dbo.asDocument.toJson, 
      """{"a": "A", "b": 1.23, "c": 15, "d": 3}"""
    )
    assertEquals(mongoScalaJack.read[Loose](dbo), inst)
  }

  test("OffsetDateTime support") {
    val t = LocalDate
      .parse("1986-07-01")
      .atTime(OffsetTime.of(LocalTime.MIDNIGHT, ZoneOffset.UTC))
    val thing = JodaThing("Foo", t, List(t, t), Some(t))
    val dbo = mongoScalaJack.render(thing)
    assertEquals(dbo.asDocument.toJson, 
      """{"name": "Foo", "dt": {"$date": 520560000000}, "many": [{"$date": 520560000000}, {"$date": 520560000000}], "maybe": {"$date": 520560000000}}"""
    )
    val b = mongoScalaJack.read[JodaThing](dbo)
    assertEquals(b, thing)
  }

  test("ZonedDateTime must work") {
    val inst = SampleZonedDateTime(
      ZonedDateTime.parse("2007-12-03T10:15:30Z[UTC]"),
      ZonedDateTime.parse("2007-12-03T10:15:30Z[UTC]")
    )
    val dbo = mongoScalaJack.render(inst)
    assertEquals(dbo.asDocument.toJson, 
      """{"o1": {"$date": 1196676930000}, "o2": {"$date": 1196676930000}}"""
    )
    val b = mongoScalaJack.read[SampleZonedDateTime](dbo)
    assertEquals(b, inst)
  }

  test("Permissives work") {
    val bd = new BsonDocument()
    bd.append("name", new BsonString("Fido"))
    bd.append("legs", new BsonString("3"))
    val wPerm = mongoScalaJack.allowPermissivePrimitives()
    assertEquals(wPerm.read[Animal](bd), Animal("Fido", 3))
  }

  test(
    "Case class having List parameter - Foo[A](x:A) where A -> List of simple type"
  ) {
    describe("Basic Collection Support")
    val w = Carry("Trey", Wrap("Hobbies", List(true, true, false), "all"))
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Trey", "w": {"name": "Hobbies", "data": [true, true, false], "stuff": "all"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[List[Boolean]]](db), w)
  }

  test(
    "Case class having Map parameter - Foo[A](x:A) where A -> Map of simple type"
  ) {
      val w = Carry("Troy", Wrap("Articles", Map("OK" -> 59), "all"))
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.toJson, 
        """{"s": "Troy", "w": {"name": "Articles", "data": {"OK": 59}, "stuff": "all"}}"""
      )
      assertEquals(mongoScalaJack.read[Carry[Map[String, Int]]](db), w)
    }

  test(
    "Case class having Option parameter - Foo[A](x:A) where A -> Option of simple type"
  ) {
      val w = Carry(
        "Terri",
        Wrap("Hobbies", Some(17).asInstanceOf[Option[Int]], "all")
      )
      val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
      val db = mongoScalaJack.render(w)
      val db2 = mongoScalaJack.render(x)
      assertEquals(db.asDocument.toJson, 
        """{"s": "Terri", "w": {"name": "Hobbies", "data": 17, "stuff": "all"}}"""
      )
      assertEquals(db2.asDocument.toJson, 
        """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
      )
      assertEquals(mongoScalaJack.read[Carry[Option[Int]]](db), w)
      assertEquals(mongoScalaJack.read[Carry[Option[Int]]](db2), x)
    }

  test(
    "Case class having List of parameterized value - Foo[A](x:List[A]) - where A is a simple type"
  ) {
      val w = BagList("list", List(1, 2, 3))
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.toJson, 
        """{"s": "list", "many": [1, 2, 3]}"""
      )
      assertEquals(mongoScalaJack.read[BagList[Int]](db), w)
    }

  test(
    "Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B are simple types"
  ) {
      val w = BagMap(5, Map("one" -> true, "two" -> false))
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.toJson, 
        """{"i": 5, "items": {"one": true, "two": false}}"""
      )
      assertEquals(mongoScalaJack.read[BagMap[Boolean]](db), w)
    }

  test(
    "Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A is a simple type"
  ) {
      val w = BagOpt(1, Some("ok"))
      val x = BagOpt[String](1, None)
      val db = mongoScalaJack.render(w)
      val db2 = mongoScalaJack.render(x)
      assertEquals(db.asDocument.toJson, """{"i": 1, "maybe": "ok"}""")
      assertEquals(db2.asDocument.toJson, """{"i": 1}""")
      assertEquals(mongoScalaJack.read[BagOpt[String]](db), w)
      assertEquals(mongoScalaJack.read[BagOpt[String]](db2), x)
    }

  test(
      "Case class having List parameter - Foo[A](x:A) where A -> List of Bar[Int]"
    ) {
      describe(
        "Advanced Collection Support - collections of parameterized case class"
      )
        val w = Carry(
        "Trey",
        Wrap("Hobbies", List(Zoo("one", 1), Zoo("two", 2)), "all")
      )
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.toJson, 
        """{"s": "Trey", "w": {"name": "Hobbies", "data": [{"name": "one", "z": 1}, {"name": "two", "z": 2}], "stuff": "all"}}"""
      )
      assertEquals(mongoScalaJack.read[Carry[List[Zoo[Int]]]](db), w)
    }

  test(
      "Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[Int,String]"
    ) {
        val w =
          Carry("Troy", Wrap("Articles", Map("OK" -> Zoo("q", false)), "all"))
        val db = mongoScalaJack.render(w)
        assertEquals(db.asDocument.toJson, 
          """{"s": "Troy", "w": {"name": "Articles", "data": {"OK": {"name": "q", "z": false}}, "stuff": "all"}}"""
        )
        assertEquals(mongoScalaJack.read[Carry[Map[String, Zoo[Boolean]]]](db), 
          w
        )
      }

  test(
      "Case class having Option parameter - Foo[A](x:A) where A -> Option of Bar[Int]"
    ) {
        val w = Carry(
          "Terri",
          Wrap(
            "Hobbies",
            Some(Zoo("a", "b")).asInstanceOf[Option[Zoo[String]]],
            "all"
          )
        )
        val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
        val db = mongoScalaJack.render(w)
        val db2 = mongoScalaJack.render(x)
        assertEquals(db.asDocument.toJson, 
          """{"s": "Terri", "w": {"name": "Hobbies", "data": {"name": "a", "z": "b"}, "stuff": "all"}}"""
        )
        assertEquals(db2.asDocument.toJson, 
          """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
        )
        assertEquals(mongoScalaJack.read[Carry[Option[Zoo[String]]]](db), w)
        assert(mongoScalaJack.read[Carry[Option[Zoo[String]]]](db2) == x)
      }

  test(
      "Case class having List parameter - Foo[A](x:A) where A -> List of value class"
    ) {
        val w = Carry(
          "Trey",
          Wrap("Hobbies", List(new Wrapper(99), new Wrapper(100)), "all")
        )
        val db = mongoScalaJack.render(w)
        assertEquals(db.asDocument.toJson, 
          """{"s": "Trey", "w": {"name": "Hobbies", "data": [99, 100], "stuff": "all"}}"""
        )
        assertEquals(mongoScalaJack.read[Carry[List[Wrapper]]](db), w)
      }

  test(
      "Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[String,value class]"
    ) {
        val w =
          Carry("Troy", Wrap("Articles", Map("OK" -> new Wrapper(2)), "all"))
        val db = mongoScalaJack.render(w)
        assertEquals(db.asDocument.toJson, 
          """{"s": "Troy", "w": {"name": "Articles", "data": {"OK": 2}, "stuff": "all"}}"""
        )
        assertEquals(mongoScalaJack.read[Carry[Map[String, Wrapper]]](db), w)
      }

  test(
      "Case class having Option parameter - Foo[A](x:A) where A -> Option of value class"
    ) {
        val w = Carry(
          "Terri",
          Wrap(
            "Hobbies",
            Some(new Wrapper(-2)).asInstanceOf[Option[Wrapper]],
            "all"
          )
        )
        val x = Carry[Option[Wrapper]]("Terry", Wrap("Hobbies", None, "all"))
        val db = mongoScalaJack.render(w)
        val db2 = mongoScalaJack.render(x)
        assertEquals(db.asDocument.toJson, 
          """{"s": "Terri", "w": {"name": "Hobbies", "data": -2, "stuff": "all"}}"""
        )
        assertEquals(db2.asDocument.toJson, 
          """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
        )
        assertEquals(mongoScalaJack.read[Carry[Option[Wrapper]]](db), w)
        assertEquals(mongoScalaJack.read[Carry[Option[Wrapper]]](db2), x)
      }

  test(
      "Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> Bar[Int]"
    ) {
        val w = BagList("list", List(Zoo("a", 1), Zoo("b", 2)))
        val db = mongoScalaJack.render(w)
        assertEquals(db.asDocument.toJson, 
          """{"s": "list", "many": [{"name": "a", "z": 1}, {"name": "b", "z": 2}]}"""
        )
        assertEquals(mongoScalaJack.read[BagList[Zoo[Int]]](db), w)
      }

  test(
      "Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,Bar[Int]"
    ) {
        val w = BagMap(5, Map("one" -> Zoo("a", 1), "two" -> Zoo("b", 2)))
        val db = mongoScalaJack.render(w)
        assertEquals(db.asDocument.toJson, 
          """{"i": 5, "items": {"one": {"name": "a", "z": 1}, "two": {"name": "b", "z": 2}}}"""
        )
        assertEquals(mongoScalaJack.read[BagMap[Zoo[Int]]](db), w)
      }

  test(
      "Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> Bar[Int]"
    ) {
        val w = Carry(
          "Terri",
          Wrap(
            "Hobbies",
            Some(Truck(false, Two("aaa", true)))
              .asInstanceOf[Option[Truck[Boolean]]],
            "all"
          )
        )
        val x =
          Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
        val db = mongoScalaJack.render(w)
        val db2 = mongoScalaJack.render(x)
        assertEquals(db.asDocument.toJson, 
          """{"s": "Terri", "w": {"name": "Hobbies", "data": {"s": false, "t": {"foo": "aaa", "bar": true}}, "stuff": "all"}}"""
        )
        assertEquals(db2.asDocument.toJson, 
          """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
        )
        assertEquals(mongoScalaJack.read[Carry[Option[Truck[Boolean]]]](db), w)
        assertEquals(mongoScalaJack.read[Carry[Option[Truck[Boolean]]]](db2), x)
      }

  test(
      "Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> value class"
    ) {
        val w = BagList(
          "list",
          List(Zoo("a", new Wrapper(1)), Zoo("b", new Wrapper(2)))
        )
        val db = mongoScalaJack.render(w)
        assertEquals(db.asDocument.toJson, 
          """{"s": "list", "many": [{"name": "a", "z": 1}, {"name": "b", "z": 2}]}"""
        )
        assertEquals(mongoScalaJack.read[BagList[Zoo[Wrapper]]](db), w)
      }

  test(
      "Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,value class"
    ) {
        val w = BagMap(
          5,
          Map(
            "one" -> Zoo("a", new Wrapper(1)),
            "two" -> Zoo("b", new Wrapper(2))
          )
        )
        val db = mongoScalaJack.render(w)
        assertEquals(db.asDocument.toJson, 
          """{"i": 5, "items": {"one": {"name": "a", "z": 1}, "two": {"name": "b", "z": 2}}}"""
        )
        assertEquals(mongoScalaJack.read[BagMap[Zoo[Wrapper]]](db), w)
      }

  test(
      "Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> value class"
    ) {
        val w = Carry(
          "Terri",
          Wrap(
            "Hobbies",
            Some(Zoo("a", new Wrapper(12))).asInstanceOf[Option[Zoo[Wrapper]]],
            "all"
          )
        )
        val x =
          Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
        val db = mongoScalaJack.render(w)
        val db2 = mongoScalaJack.render(x)
        assertEquals(db.asDocument.toJson, 
          """{"s": "Terri", "w": {"name": "Hobbies", "data": {"name": "a", "z": 12}, "stuff": "all"}}"""
        )
        assertEquals(db2.asDocument.toJson, 
          """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
        )
        assertEquals(mongoScalaJack.read[Carry[Option[Zoo[Wrapper]]]](db), w)
        assert(mongoScalaJack.read[Carry[Option[Zoo[Wrapper]]]](db2) == x)
      }

  test("Parameter is a simple trait") {
    describe("Basic trait support")
    val w = Carry[Pop]("Surprise", Wrap("Yellow", Wow2("three", 3), "Done"))
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Surprise", "w": {"name": "Yellow", "data": {"_hint": "co.blocke.scalajack.mongo.Wow2", "x": "three", "y": 3}, "stuff": "Done"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Pop]](db), w)
  }

  test("Parameter is a simple trait with hint function value mappings") {
    val w = Carry[Pop]("Surprise", Wrap("Yellow", Wow2("three", 4), "Done"))
    val scalaJack = mongoScalaJack.withHintModifiers(
      RType.of[Pop] -> ClassNameHintModifier(
        hint => s"co.blocke.scalajack.mongo.$hint",
        fullName => fullName.split('.').last
      )
    )
    val db = scalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Surprise", "w": {"name": "Yellow", "data": {"_hint": "Wow2", "x": "three", "y": 4}, "stuff": "Done"}}"""
    )
    assertEquals(scalaJack.read[Carry[Pop]](db), w)
  }

  test("Hint modifier fails") {
    val w = Carry[Pop]("Surprise", Wrap("Yellow", Wow2("three", 4), "Done"))
    val scalaJack = mongoScalaJack.withHintModifiers(
      RType.of[Pop] -> ClassNameHintModifier(
        hint => throw new Exception("Boom"), // intentional hint mod failure
        fullName => fullName.split('.').last
      )
    )
    val db = scalaJack.render(w)
    interceptMessage[ScalaJackError]("Couldn't marshal class for Wow2"){
      scalaJack.read[Carry[Pop]](db)
    }
  }

  test("Type modifier works") {
    val scalaJack = ScalaJack(MongoFlavor()).withTypeValueModifier(
      ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.mongo." + hint,
        (cname: String) => cname.split('.').last
      )
    )
    val value: Envelope[Body] = Envelope("DEF", FancyBody("BOO"))
    val d = scalaJack.render[Envelope[Body]](value)
    assertEquals(d.asDocument.toJson, 
      """{"Giraffe": "FancyBody", "id": "DEF", "body": {"message": "BOO"}}"""
    )
    assertEquals(scalaJack.read[Envelope[Body]](d), value)
  }

  test("Parameter is List of trait") {
    val w = Carry[List[Pop]](
      "Surprise",
      Wrap("Yellow", List(Wow1("four", 4), Wow2("three", 3)), "Done")
    )
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Surprise", "w": {"name": "Yellow", "data": [{"_hint": "co.blocke.scalajack.mongo.Wow1", "a": "four", "b": 4}, {"_hint": "co.blocke.scalajack.mongo.Wow2", "x": "three", "y": 3}], "stuff": "Done"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[List[Pop]]](db), w)
  }

  test("Parameter is Map of String->trait") {
    val w = Carry[Map[String, Pop]](
      "Surprise",
      Wrap(
        "Yellow",
        Map("a" -> Wow1("four", 4), "b" -> Wow2("three", 3)),
        "Done"
      )
    )
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Surprise", "w": {"name": "Yellow", "data": {"a": {"_hint": "co.blocke.scalajack.mongo.Wow1", "a": "four", "b": 4}, "b": {"_hint": "co.blocke.scalajack.mongo.Wow2", "x": "three", "y": 3}}, "stuff": "Done"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Map[String, Pop]]](db), w)
  }

  test("Parameter is an Option of trait") {
    val w = Carry[Option[Pop]](
      "Terri",
      Wrap("Hobbies", Some(Wow1("ok", -99)), "all")
    )
    val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
    val db = mongoScalaJack.render(w)
    val db2 = mongoScalaJack.render(x)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Terri", "w": {"name": "Hobbies", "data": {"_hint": "co.blocke.scalajack.mongo.Wow1", "a": "ok", "b": -99}, "stuff": "all"}}"""
    )
    assertEquals(db2.asDocument.toJson, 
      """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Option[Pop]]](db), w)
    assertEquals(mongoScalaJack.read[Carry[Option[Pop]]](db2), x)
  }

  test("List of parameter, where parameter is a trait") {
    val w = BagList[Pop]("list", List(Wow1("A", 1), Wow1("B", 2)))
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "list", "many": [{"_hint": "co.blocke.scalajack.mongo.Wow1", "a": "A", "b": 1}, {"_hint": "co.blocke.scalajack.mongo.Wow1", "a": "B", "b": 2}]}"""
    )
    assertEquals(mongoScalaJack.read[BagList[Pop]](db), w)
  }

  test("Map of String->parameter, where parameter is a trait") {
    val w =
      BagMap[Pop](5, Map("one" -> Wow2("q", 7), "two" -> Wow1("r", 3)))
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"i": 5, "items": {"one": {"_hint": "co.blocke.scalajack.mongo.Wow2", "x": "q", "y": 7}, "two": {"_hint": "co.blocke.scalajack.mongo.Wow1", "a": "r", "b": 3}}}"""
    )
    assertEquals(mongoScalaJack.read[BagMap[Pop]](db), w)
  }

  test("Option of parameter, where parameter is a trait") {
    val w = Carry[Option[Pop]](
      "Terri",
      Wrap("Hobbies", Some(Wow2("finite", 1000)), "all")
    )
    val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
    val db = mongoScalaJack.render(w)
    val db2 = mongoScalaJack.render(x)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Terri", "w": {"name": "Hobbies", "data": {"_hint": "co.blocke.scalajack.mongo.Wow2", "x": "finite", "y": 1000}, "stuff": "all"}}"""
    )
    assertEquals(db2.asDocument.toJson, 
      """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Option[Pop]]](db), w)
    assertEquals(mongoScalaJack.read[Carry[Option[Pop]]](db2), x)
  }

  test("Case class having an embedded parameterized trait") {
    describe(
      "Advanced trait support -- parameters are traits, themselves having parameters"
    )
    val w = Breakfast(true, Toast(7, "Burnt"))
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"y": true, "bread": {"_hint": "co.blocke.scalajack.mongo.Toast", "g": 7, "yum": "Burnt"}}"""
    )
    assertEquals(mongoScalaJack.read[Breakfast[String]](db), w)
  }

  test(
    "Case class having an embedded parameterized trait, with the trait's parameter another case class"
  ) {
      val w = Breakfast(true, Toast(7, Two("two", true)))
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.toJson, 
        """{"y": true, "bread": {"_hint": "co.blocke.scalajack.mongo.Toast", "g": 7, "yum": {"foo": "two", "bar": true}}}"""
      )
      assertEquals(mongoScalaJack.read[Breakfast[Two]](db), w)
    }

  test(
    "Case class having an embedded parameterized trait, with the trait's parameter a value class"
  ) {
      val w = Breakfast(true, Toast(7, new Wrapper(-100)))
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.toJson, 
        """{"y": true, "bread": {"_hint": "co.blocke.scalajack.mongo.Toast", "g": 7, "yum": -100}}"""
      )
      assertEquals(mongoScalaJack.read[Breakfast[Wrapper]](db), w)
    }

  test("Parameter is a parameterized trait") { // I can't believe this one worked!
    val w = Carry[Tart[Soup[String]]](
      "Bill",
      Wrap("Betty", Bun(3, Cruton(8, "eight")), "ok")
    )
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Bill", "w": {"name": "Betty", "data": {"_hint": "co.blocke.scalajack.mongo.Bun", "g": 3, "yum": {"_hint": "co.blocke.scalajack.mongo.Cruton", "i": 8, "sweet": "eight"}}, "stuff": "ok"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Tart[Soup[String]]]](db), w)
  }

  test("Parameter is List of parameterized trait") {
    val w = Carry[List[Tart[Boolean]]](
      "Trey",
      Wrap("Hobbies", List(Bun(1, false), Toast(2, true)), "all")
    )
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Trey", "w": {"name": "Hobbies", "data": [{"_hint": "co.blocke.scalajack.mongo.Bun", "g": 1, "yum": false}, {"_hint": "co.blocke.scalajack.mongo.Toast", "g": 2, "yum": true}], "stuff": "all"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[List[Tart[Boolean]]]](db), w)
  }

  test("Parameter is Map of String->parameterized trait") {
    val w = Carry[Map[String, Tart[String]]](
      "Troy",
      Wrap("Articles", Map("OK" -> Bun(27, "Hot")), "all")
    )
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Troy", "w": {"name": "Articles", "data": {"OK": {"_hint": "co.blocke.scalajack.mongo.Bun", "g": 27, "yum": "Hot"}}, "stuff": "all"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Map[String, Tart[String]]]](db), 
      w
    )
  }

  test("Parameter is an Option of parameterized trait") {
    val w = Carry[Option[Tart[Int]]](
      "Terri",
      Wrap("Hobbies", Some(Toast(11, 12)), "all")
    )
    val x = Carry[Option[Tart[Int]]]("Terry", Wrap("Hobbies", None, "all"))
    val db = mongoScalaJack.render(w)
    val db2 = mongoScalaJack.render(x)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Terri", "w": {"name": "Hobbies", "data": {"_hint": "co.blocke.scalajack.mongo.Toast", "g": 11, "yum": 12}, "stuff": "all"}}"""
    )
    assertEquals(db2.asDocument.toJson, 
      """{"s": "Terry", "w": {"name": "Hobbies", "stuff": "all"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Option[Tart[Int]]]](db), w)
    assertEquals(mongoScalaJack.read[Carry[Option[Tart[Int]]]](db2), x)
  }

  test("List of parameter, where parameter is a parameterized trait") {
    val w =
      BagList[Tart[Boolean]]("list", List(Toast(1, true), Bun(2, false)))
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"s": "list", "many": [{"_hint": "co.blocke.scalajack.mongo.Toast", "g": 1, "yum": true}, {"_hint": "co.blocke.scalajack.mongo.Bun", "g": 2, "yum": false}]}"""
    )
    assertEquals(mongoScalaJack.read[BagList[Tart[Boolean]]](db), w)
  }

  test("Map of String->parameter, where parameter is a parameterized trait") {
    val w = BagMap[Tart[Boolean]](
      5,
      Map("one" -> Bun(1, true), "two" -> Toast(2, false))
    )
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"i": 5, "items": {"one": {"_hint": "co.blocke.scalajack.mongo.Bun", "g": 1, "yum": true}, "two": {"_hint": "co.blocke.scalajack.mongo.Toast", "g": 2, "yum": false}}}"""
    )
    assertEquals(mongoScalaJack.read[BagMap[Tart[Boolean]]](db), w)
  }

  test("Option of parameter, where parameter is a parameterized trait") {
    val w = BagOpt[Tart[String]](1, Some(Bun(6, "ok")))
    val x = BagOpt[Tart[String]](1, None)
    val db = mongoScalaJack.render(w)
    val db2 = mongoScalaJack.render(x)
    assertEquals(db.asDocument.toJson, 
      """{"i": 1, "maybe": {"_hint": "co.blocke.scalajack.mongo.Bun", "g": 6, "yum": "ok"}}"""
    )
    assertEquals(db2.asDocument.toJson, """{"i": 1}""")
    assertEquals(mongoScalaJack.read[BagOpt[Tart[String]]](db), w)
    assertEquals(mongoScalaJack.read[BagOpt[Tart[String]]](db2), x)
  }

  test("DBKey Annotation (_id field generation) - single key") {
    describe("Annotations (e.g. DBKey)")
    val five = Five("Fred", Two("blah", true))
    val dbo = mongoScalaJack.render(five)
    assertEquals(dbo.asDocument.toJson, 
      """{"_id": "Fred", "two": {"foo": "blah", "bar": true}}"""
    )
    assertEquals(mongoScalaJack.read[Five](dbo), five)
  }

  test(
    "DBKey Annotation (_id field generation) - single key -- Missing Non-Key Field"
  ) {
      val dbo = new BsonDocument(
        List(
          new BsonElement("_id", new BsonString("Fred")),
          new BsonElement(
            "two",
            new BsonDocument(
              List(new BsonElement("bar", new BsonBoolean(true))).asJava
            )
          )
        ).asJava
      )
      assertEquals(dbo.toJson, """{"_id": "Fred", "two": {"bar": true}}""")
      val msg = """Class co.blocke.scalajack.mongo.Two missing required fields: foo""".stripMargin
      interceptMessage[ScalaJackError](msg){
      mongoScalaJack.read[Five](dbo)
    }
    }

  test(
    "DBKey Annotation (_id field generation) - single key -- Missing Key Field"
  ) {
      val dbo = new BsonDocument(
        List(
          new BsonElement(
            "two",
            new BsonDocument(
              List(
                new BsonElement("foo", new BsonString("blah")),
                new BsonElement("bar", new BsonBoolean(true))
              ).asJava
            )
          )
        ).asJava
      )
      assertEquals(dbo.toJson, """{"two": {"foo": "blah", "bar": true}}""")
      val msg =
        """Missing key (_id) field, or a component of a compound key field"""
      interceptMessage[ScalaJackError](msg){
      mongoScalaJack.read[Five](dbo)
    }
    }

  test("DBKey Annotation (_id field generation) - compound key") {
    val six = Six("Fred", 12, Two("blah", true))
    val dbo = mongoScalaJack.render(six)
    assertEquals(dbo.asDocument.toJson, 
      """{"_id": {"name": "Fred", "num": 12}, "two": {"foo": "blah", "bar": true}}"""
    )
    assertEquals(mongoScalaJack.read[Six](dbo), six)
  }

  test(
    "DBKey Annotation (_id field generation) - compound key -- Missing Key Field component"
  ) {
    val js =
      """{"_id": {"name": "Fred"},"two": {"foo": "blah", "bar": true}}"""
    val dbo = BsonDocument.parse(js)
    val msg =
      """Class co.blocke.scalajack.mongo.Six missing required fields: num"""
    interceptMessage[ScalaJackError](msg){
      mongoScalaJack.read[Six](dbo)
    }
  }

  test("ObjectId support -- Mongo") {
    describe("Mongo ObjectID")
    // val oid = (new BsonObjectId()).getValue()
    val oid = new ObjectId()
    val seven = Seven(oid, Two("blah", true))
    val dbo = mongoScalaJack.render(seven)
    assertEquals(dbo.asDocument.toJson, 
      s"""{"_id": {"$$oid": "${oid.toString}"}, "two": {"foo": "blah", "bar": true}}"""
    )
    assertEquals(mongoScalaJack.read[Seven](dbo), seven)
  }

  test("ObjectId support (null) -- Mongo") {
    val seven = Seven(null, Two("blah", bar = true))
    val dbo = mongoScalaJack.render(seven)
    assertEquals(mongoScalaJack.read[Seven](dbo), seven)
  }

  test("Must handle a case class with default values - defaults specified") {
    describe("Basic Case Class Support")
    val wd = WithDefaults(
      "Greg",
      49,
      Some(5),
      Some(false),
      GrumpyPet(Cat("Fluffy"), "fish")
    )
    val dbo = mongoScalaJack.render(wd)
    assertEquals(dbo.asDocument.toJson, 
      """{"name": "Greg", "age": 49, "num": 5, "hasStuff": false, "pet": {"_hint": "co.blocke.scalajack.mongo.GrumpyPet", "kind": {"_hint": "co.blocke.scalajack.mongo.Cat", "name": "Fluffy"}, "food": "fish"}}"""
    )
    val b = mongoScalaJack.read[WithDefaults](dbo)
    assertEquals(b, wd)
  }

  test("Case class as value for Any parameter") {
    val f = Flexible("foo", Two("bar", bar = true))
    val d = mongoScalaJack.render(f)
    assertEquals(d.toString, 
      """{"name": "foo", "dunno": {"_hint": "co.blocke.scalajack.mongo.Two", "foo": "bar", "bar": true}}"""
    )
    assertEquals(mongoScalaJack.read[Flexible](d), f)
  }

  test(
    "Must handle a case class with default values - defaults not specified"
  ) {
      val wd = WithDefaults("Greg", 49, None)
      val dbo = mongoScalaJack.render(wd)
      assertEquals(dbo.asDocument.toJson, 
        """{"name": "Greg", "age": 49, "hasStuff": true, "pet": {"_hint": "co.blocke.scalajack.mongo.NicePet", "kind": {"_hint": "co.blocke.scalajack.mongo.Dog", "name": "Fido"}, "food": "bones"}}"""
      )
      val b = mongoScalaJack.read[WithDefaults](dbo)
      assertEquals(b, wd)
  }

  test("Simple parameters - Foo[A](x:A) where A -> simple type") {
    describe("Basic Parameterized Case Class")
    val w = Wrap("number", true, 15)
    val w2 = Wrap("number", true, "wow")
    val db = mongoScalaJack.render(w)
    val db2 = mongoScalaJack.render(w2)
    assertEquals(db.asDocument.toJson, 
      """{"name": "number", "data": true, "stuff": 15}"""
    )
    assertEquals(db2.asDocument.toJson, 
      """{"name": "number", "data": true, "stuff": "wow"}"""
    )
    assertEquals(mongoScalaJack.read[Wrap[Boolean, Int]](db), w)
    assertEquals(mongoScalaJack.read[Wrap[Boolean, String]](db2), w2)
  }

  test(
    "Non-parameter case clase as a field member - Foo[A](x:A, b:Bar) where A -> simple type"
  ) {
      val w = Truck(false, Two("z", true))
      val dbo = mongoScalaJack.render(w)
      assertEquals(dbo.asDocument.toJson, 
        """{"s": false, "t": {"foo": "z", "bar": true}}"""
      )
      assertEquals(mongoScalaJack.read[Truck[Boolean]](dbo), w)
    }

  test("Non-parameter case class as a parameter - Foo[A](x:A) where A -> Bar") {
    val w = Wrap("number", true, Two("a", false))
    val db = mongoScalaJack.render(w)
    assertEquals(db.asDocument.toJson, 
      """{"name": "number", "data": true, "stuff": {"foo": "a", "bar": false}}"""
    )
    assertEquals(mongoScalaJack.read[Wrap[Boolean, Two]](db), w)
  }

  test(
    "Parameterized case class as parameter - Foo[A](x:A) where A -> Bar[Int]"
  ) {
    describe("Advanced Parameterized Case Class")
    val w = Carry("Bob", Wrap("Mary", 3, "Available"))
    val x = Carry("Mary", Wrap("Greg", false, "Done"))
    val y = Carry("Fred", Wrap("Mike", Two("Steam", true), "OK"))
    val db = mongoScalaJack.render(w)
    val db2 = mongoScalaJack.render(x)
    val db3 = mongoScalaJack.render(y)
    assertEquals(db.asDocument.toJson, 
      """{"s": "Bob", "w": {"name": "Mary", "data": 3, "stuff": "Available"}}"""
    )
    assertEquals(db2.asDocument.toJson, 
      """{"s": "Mary", "w": {"name": "Greg", "data": false, "stuff": "Done"}}"""
    )
    assertEquals(db3.asDocument.toJson, 
      """{"s": "Fred", "w": {"name": "Mike", "data": {"foo": "Steam", "bar": true}, "stuff": "OK"}}"""
    )
    assertEquals(mongoScalaJack.read[Carry[Int]](db), w)
    assertEquals(mongoScalaJack.read[Carry[Boolean]](db2), x)
    assertEquals(mongoScalaJack.read[Carry[Two]](db3), y)
  }

  test(
    "Case class having value class parameter - Foo[A](x:A) where A -> value class (no value class handler)"
  ) {
      val w = Carry("Mike", Wrap("Sally", new Wrapper(15), "Fine"))
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.asDocument.toJson, 
        """{"s": "Mike", "w": {"name": "Sally", "data": 15, "stuff": "Fine"}}"""
      )
      assertEquals(mongoScalaJack.read[Carry[Wrapper]](db), w)
    }

  test(
    "Case class having value class parameter - Foo[A](x:A) where A -> value class (WITH value class handler)"
  ) {
      val offsetDateTime =
        OffsetDateTime.of(2015, 7, 1, 0, 0, 0, 0, ZoneOffset.UTC)
      val w = Carry(
        "Mike",
        Wrap("Sally", new WrappedOffsetDateTime(offsetDateTime), "Fine")
      )
      val db = mongoScalaJack.render(w)

      val timeval = offsetDateTime.toInstant.toEpochMilli
      assertEquals(db.asDocument.toJson, 
        s"""{"s": "Mike", "w": {"name": "Sally", "data": {"$$date": $timeval}, "stuff": "Fine"}}"""
      )
      assertEquals(mongoScalaJack.read[Carry[WrappedOffsetDateTime]](db), w)
    }

  test(
    "Case class having parameterized case class as a parameter: Foo[A](x:A) where A -> Bar[Blah[Long]]"
  ) {
      val w = Carry("Bill", Wrap("Betty", Zoo("dog", false), "ok"))
      val db = mongoScalaJack.render(w)
      assertEquals(db.asDocument.toJson, 
        """{"s": "Bill", "w": {"name": "Betty", "data": {"name": "dog", "z": false}, "stuff": "ok"}}"""
      )
      assertEquals(mongoScalaJack.read[Carry[Zoo[Boolean]]](db), w)
    }
