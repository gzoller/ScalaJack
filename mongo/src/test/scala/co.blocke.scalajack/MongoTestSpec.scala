package co.blocke.scalajack
package test

import java.util.UUID

import co.blocke.scalajack.json.JsonFlavor
import co.blocke.scalajack.mongo._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.mongodb.scala.bson._
import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.Matchers._
import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }

import scala.reflect.runtime.universe.typeOf
import scala.util._

class MongoTestSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val data = One("Greg", List("a", "b"), List(Two("x", false), Two("y", true)), Two("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)

  def mongoScalaJack = ScalaJack(MongoFlavor()).withAdapters(JodaDateTimeTypeAdapter, BsonDateTimeTypeAdapter)

  def jsonScalaJack = ScalaJack(JsonFlavor()).withAdapters(JodaDateTimeTypeAdapter, BsonDateTimeTypeAdapter)

  describe("=====================\n| -- Mongo Tests -- |\n=====================") {
    describe("MongoDB/Casbah Support") {
      it("MongoKey Annotation (_id field generation) - single key") {
        val five = Five("Fred", Two("blah", true))
        val dbo = mongoScalaJack.render(five)
        Document(dbo.asDocument).toJson should equal("""{ "_id" : "Fred", "two" : { "foo" : "blah", "bar" : true } }""")
        mongoScalaJack.read[Five](dbo) should equal(five)
      }
      it("MongoKey Annotation (_id field generation) - single key -- Missing Non-Key Field") {
        val five = Five("Fred", Two("blah", true))
        val dbo = BsonDocument("_id" -> BsonString("Fred"), "two" -> BsonDocument("bar" -> BsonBoolean(true)))
        dbo.toJson should equal("""{ "_id" : "Fred", "two" : { "bar" : true } }""")
        val result = Try(mongoScalaJack.read[Five](dbo) should equal(five)) match {
          case Success(x) => "fail"
          case Failure(x) if (x.getMessage == "Missing required field foo in BsonDocument") => "pass"
        }
        result should equal("pass")
      }
      it("MongoKey Annotation (_id field generation) - single key -- Missing Key Field") {
        val five = Five("Fred", Two("blah", true))
        val dbo = BsonDocument("two" -> BsonDocument("foo" -> BsonString("blah"), "bar" -> BsonBoolean(true)))
        dbo.toJson should equal("""{ "two" : { "foo" : "blah", "bar" : true } }""")
        val result = Try(mongoScalaJack.read[Five](dbo) should equal(five)) match {
          case Success(x) => "fail"
          case Failure(x) if (x.getMessage == "Missing required field name in BsonDocument") => "pass"
        }
        result should equal("pass")
      }
      it("MongoKey Annotation (_id field generation) - compound key") {
        val six = Six("Fred", 12, Two("blah", true))
        val dbo = mongoScalaJack.render(six)
        dbo.asDocument.toJson should equal("""{ "_id" : { "name" : "Fred", "num" : 12 }, "two" : { "foo" : "blah", "bar" : true } }""")
        mongoScalaJack.read[Six](dbo) should equal(six)
      }
      it("ObjectId support -- JSON") {
        val oid = new ObjectId(BsonObjectId())
        val seven = Seven(oid, Two("blah", true))
        val js = jsonScalaJack.render(seven)
        println(js)
        jsonScalaJack.read[Seven](js) should equal(seven)
      }
      it("ObjectId support -- Mongo") {
        val oid = new ObjectId(BsonObjectId())
        val seven = Seven(oid, Two("blah", true))
        val dbo = mongoScalaJack.render(seven)
        dbo.asDocument.toJson should equal(s"""{ "_id" : { "$$oid" : "${oid.bsonObjectId.getValue.toString}" }, "two" : { "foo" : "blah", "bar" : true } }""")
        mongoScalaJack.read[Seven](dbo) should equal(seven)
      }
      it("Naked Map support") {
        val li = Map("a" -> 1, "b" -> 2, "c" -> 3)
        val dbo = mongoScalaJack.render(li)
        dbo.asDocument.toJson should equal("""{ "a" : 1, "b" : 2, "c" : 3 }""")
        mongoScalaJack.read[Map[String, Int]](dbo) should equal(li)
      }
      it("UUID support") {
        val thing = UuidThing("Foo", UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), List(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")), Some(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")))
        val dbo = mongoScalaJack.render(thing)
        dbo.asDocument.toJson should equal("""{ "name" : "Foo", "uuid" : "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c", "many" : ["1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c", "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"], "maybe" : "1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c" }""")
        val b = mongoScalaJack.read[UuidThing](dbo)
        b should equal(thing)
      }
      it("DateTime support") {
        val pattern = "MM-dd-yy"
        val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
        val thing = JodaThing("Foo", t, List(t, t), Some(t))
        val dbo = mongoScalaJack.render(thing)
        dbo.asDocument.toJson should equal("""{ "name" : "Foo", "dt" : { "$date" : 520560000000 }, "many" : [{ "$date" : 520560000000 }, { "$date" : 520560000000 }], "maybe" : { "$date" : 520560000000 } }""")
        val b = mongoScalaJack.read[JodaThing](dbo)
        b should equal(thing)
      }
      it("Must handle a case class with default values - defaults specified") {
        val wd = WithDefaults("Greg", 49, Some(5), Some(false), GrumpyPet(Cat("Fluffy"), "fish"))
        val dbo = mongoScalaJack.render(wd)
        dbo.asDocument.toJson should equal("""{ "name" : "Greg", "age" : 49, "num" : 5, "hasStuff" : false, "pet" : { "_hint" : "co.blocke.scalajack.test.GrumpyPet", "kind" : { "_hint" : "co.blocke.scalajack.test.Cat", "name" : "Fluffy" }, "food" : "fish" } }""")
        val b = mongoScalaJack.read[WithDefaults](dbo)
        b should equal(wd)
      }
      it("Must handle a case class with default values - defaults not specified") {
        val wd = WithDefaults("Greg", 49, None)
        val dbo = mongoScalaJack.render(wd)
        dbo.asDocument.toJson should equal("""{ "name" : "Greg", "age" : 49, "hasStuff" : true, "pet" : { "_hint" : "co.blocke.scalajack.test.NicePet", "kind" : { "_hint" : "co.blocke.scalajack.test.Dog", "name" : "Fido" }, "food" : "bones" } }""")
        val b = mongoScalaJack.read[WithDefaults](dbo)
        b should equal(wd)
      }
    }
    describe("Parameterized Class Support") {
      describe("Basic Parameterized Case Class") {
        it("Simple parameters - Foo[A](x:A) where A -> simple type") {
          val w = Wrap("number", true, 15)
          val w2 = Wrap("number", true, "wow")
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(w2)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(w2)
          js should equal("""{"name":"number","data":true,"stuff":15}""")
          js2 should equal("""{"name":"number","data":true,"stuff":"wow"}""")
          db.asDocument.toJson should equal("""{ "name" : "number", "data" : true, "stuff" : 15 }""")
          db2.asDocument.toJson should equal("""{ "name" : "number", "data" : true, "stuff" : "wow" }""")
          jsonScalaJack.read[Wrap[Boolean, Int]](js) should equal(w)
          jsonScalaJack.read[Wrap[Boolean, String]](js2) should equal(w2)
          mongoScalaJack.read[Wrap[Boolean, Int]](db) should equal(w)
          mongoScalaJack.read[Wrap[Boolean, String]](db2) should equal(w2)
        }
        it("Non-parameter case clase as a field member - Foo[A](x:A, b:Bar) where A -> simple type") {
          val w = Truck(false, Two("z", true))
          val js = jsonScalaJack.render(w)
          val dbo = mongoScalaJack.render(w)
          js should equal("""{"s":false,"t":{"foo":"z","bar":true}}""")
          dbo.asDocument.toJson should equal("""{ "s" : false, "t" : { "foo" : "z", "bar" : true } }""")
          jsonScalaJack.read[Truck[Boolean]](js) should equal(w)
          mongoScalaJack.read[Truck[Boolean]](dbo) should equal(w)
        }
        it("Non-parameter case class as a parameter - Foo[A](x:A) where A -> Bar") {
          val w = Wrap("number", true, Two("a", false))
          val js = jsonScalaJack.render(w)
          js should equal("""{"name":"number","data":true,"stuff":{"foo":"a","bar":false}}""")
          val db = mongoScalaJack.render(w)
          db.asDocument.toJson should equal("""{ "name" : "number", "data" : true, "stuff" : { "foo" : "a", "bar" : false } }""")
          jsonScalaJack.read[Wrap[Boolean, Two]](js) should equal(w)
          mongoScalaJack.read[Wrap[Boolean, Two]](db) should equal(w)
        }
      }
      describe("Advanced Parameterized Case Class") {
        it("Parameterized case class as parameter - Foo[A](x:A) where A -> Bar[Int]") {
          val w = Carry("Bob", Wrap("Mary", 3, "Available"))
          val x = Carry("Mary", Wrap("Greg", false, "Done"))
          val y = Carry("Fred", Wrap("Mike", Two("Steam", true), "OK"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val js3 = jsonScalaJack.render(y)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          val db3 = mongoScalaJack.render(y)
          js should equal("""{"s":"Bob","w":{"name":"Mary","data":3,"stuff":"Available"}}""")
          js2 should equal("""{"s":"Mary","w":{"name":"Greg","data":false,"stuff":"Done"}}""")
          js3 should equal("""{"s":"Fred","w":{"name":"Mike","data":{"foo":"Steam","bar":true},"stuff":"OK"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Bob", "w" : { "name" : "Mary", "data" : 3, "stuff" : "Available" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Mary", "w" : { "name" : "Greg", "data" : false, "stuff" : "Done" } }""")
          db3.asDocument.toJson should equal("""{ "s" : "Fred", "w" : { "name" : "Mike", "data" : { "foo" : "Steam", "bar" : true }, "stuff" : "OK" } }""")
          jsonScalaJack.read[Carry[Int]](js) should equal(w)
          jsonScalaJack.read[Carry[Boolean]](js2) should equal(x)
          jsonScalaJack.read[Carry[Two]](js3) should equal(y)
          mongoScalaJack.read[Carry[Int]](db) should equal(w)
          mongoScalaJack.read[Carry[Boolean]](db2) should equal(x)
          mongoScalaJack.read[Carry[Two]](db3) should equal(y)
        }
        it("Case class having value class parameter - Foo[A](x:A) where A -> value class (no value class handler)") {
          val w = Carry("Mike", Wrap("Sally", new Wrapper(15), "Fine"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Mike","w":{"name":"Sally","data":15,"stuff":"Fine"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Mike", "w" : { "name" : "Sally", "data" : 15, "stuff" : "Fine" } }""")
          jsonScalaJack.read[Carry[Wrapper]](js) should equal(w)
          mongoScalaJack.read[Carry[Wrapper]](db) should equal(w)
        }
        it("Case class having value class parameter - Foo[A](x:A) where A -> value class (WITH value class handler)") {
          val w = Carry("Mike", Wrap("Sally", new CustomVC(new DateTime(2015, 7, 1, 0, 0)), "Fine"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)

          val timeval = (new DateTime(2015, 7, 1, 0, 0)).toDate.getTime
          db.asDocument.toJson should equal(s"""{ "s" : "Mike", "w" : { "name" : "Sally", "data" : { "$$date" : $timeval }, "stuff" : "Fine" } }""")
          jsonScalaJack.read[Carry[CustomVC]](js) should equal(w)
          mongoScalaJack.read[Carry[CustomVC]](db) should equal(w)
        }
        it("Case class having parameterized case class as a parameter: Foo[A](x:A) where A -> Bar[Blah[Long]]") {
          val w = Carry("Bill", Wrap("Betty", Zoo("dog", false), "ok"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Bill","w":{"name":"Betty","data":{"name":"dog","z":false},"stuff":"ok"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Bill", "w" : { "name" : "Betty", "data" : { "name" : "dog", "z" : false }, "stuff" : "ok" } }""")
          jsonScalaJack.read[Carry[Zoo[Boolean]]](js) should equal(w)
          mongoScalaJack.read[Carry[Zoo[Boolean]]](db) should equal(w)
        }
      }
      describe("Basic Collection Support") {
        it("Case class having List parameter - Foo[A](x:A) where A -> List of simple type") {
          val w = Carry("Trey", Wrap("Hobbies", List(true, true, false), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[true,true,false],"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Trey", "w" : { "name" : "Hobbies", "data" : [true, true, false], "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[List[Boolean]]](js) should equal(w)
          mongoScalaJack.read[Carry[List[Boolean]]](db) should equal(w)
        }
        it("Case class having Map parameter - Foo[A](x:A) where A -> Map of simple type") {
          val w = Carry("Troy", Wrap("Articles", Map("OK" -> 59), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":59},"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Troy", "w" : { "name" : "Articles", "data" : { "OK" : 59 }, "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Map[String, Int]]](js) should equal(w)
          mongoScalaJack.read[Carry[Map[String, Int]]](db) should equal(w)
        }
        it("Case class having Option parameter - Foo[A](x:A) where A -> Option of simple type") {
          val w = Carry("Terri", Wrap("Hobbies", Some(17).asInstanceOf[Option[Int]], "all"))
          val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":17,"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : 17, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Int]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Int]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Int]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Int]]](db2) should equal(x)
        }
        it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A is a simple type") {
          val w = BagList("list", List(1, 2, 3))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"list","many":[1,2,3]}""")
          db.asDocument.toJson should equal("""{ "s" : "list", "many" : [1, 2, 3] }""")
          jsonScalaJack.read[BagList[Int]](js) should equal(w)
          mongoScalaJack.read[BagList[Int]](db) should equal(w)
        }
        it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B are simple types") {
          val w = BagMap(5, Map("one" -> true, "two" -> false))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"i":5,"items":{"one":true,"two":false}}""")
          db.asDocument.toJson should equal("""{ "i" : 5, "items" : { "one" : true, "two" : false } }""")
          jsonScalaJack.read[BagMap[Boolean]](js) should equal(w)
          mongoScalaJack.read[BagMap[Boolean]](db) should equal(w)
        }
        it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A is a simple type") {
          val w = BagOpt(1, Some("ok"))
          val x = BagOpt[String](1, None)
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"i":1,"maybe":"ok"}""")
          js2 should equal("""{"i":1}""")
          db.asDocument.toJson should equal("""{ "i" : 1, "maybe" : "ok" }""")
          db2.asDocument.toJson should equal("""{ "i" : 1 }""")
          jsonScalaJack.read[BagOpt[String]](js) should equal(w)
          jsonScalaJack.read[BagOpt[String]](js2) should equal(x)
          mongoScalaJack.read[BagOpt[String]](db) should equal(w)
          mongoScalaJack.read[BagOpt[String]](db2) should equal(x)
        }
      }
      describe("Advanced Collection Support - collections of parameterized case class") {
        it("Case class having List parameter - Foo[A](x:A) where A -> List of Bar[Int]") {
          val w = Carry("Trey", Wrap("Hobbies", List(Zoo("one", 1), Zoo("two", 2)), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[{"name":"one","z":1},{"name":"two","z":2}],"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Trey", "w" : { "name" : "Hobbies", "data" : [{ "name" : "one", "z" : 1 }, { "name" : "two", "z" : 2 }], "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[List[Zoo[Int]]]](js) should equal(w)
          mongoScalaJack.read[Carry[List[Zoo[Int]]]](db) should equal(w)
        }
        it("Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[Int,String]") {
          val w = Carry("Troy", Wrap("Articles", Map("OK" -> Zoo("q", false)), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":{"name":"q","z":false}},"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Troy", "w" : { "name" : "Articles", "data" : { "OK" : { "name" : "q", "z" : false } }, "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Map[String, Zoo[Boolean]]]](js) should equal(w)
          mongoScalaJack.read[Carry[Map[String, Zoo[Boolean]]]](db) should equal(w)
        }
        it("Case class having Option parameter - Foo[A](x:A) where A -> Option of Bar[Int]") {
          val w = Carry("Terri", Wrap("Hobbies", Some(Zoo("a", "b")).asInstanceOf[Option[Zoo[String]]], "all"))
          val x = Carry[Option[Int]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"name":"a","z":"b"},"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : { "name" : "a", "z" : "b" }, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Zoo[String]]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Zoo[String]]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Zoo[String]]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Zoo[String]]]](db2) should equal(x)
        }
        it("Case class having List parameter - Foo[A](x:A) where A -> List of value class") {
          val w = Carry("Trey", Wrap("Hobbies", List(new Wrapper(99), new Wrapper(100)), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[99,100],"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Trey", "w" : { "name" : "Hobbies", "data" : [99, 100], "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[List[Wrapper]]](js) should equal(w)
          mongoScalaJack.read[Carry[List[Wrapper]]](db) should equal(w)
        }
        it("Case class having Map parameter - Foo[A](x:A) where A -> Map of Bar[String,value class]") {
          val w = Carry("Troy", Wrap("Articles", Map("OK" -> new Wrapper(2)), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":2},"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Troy", "w" : { "name" : "Articles", "data" : { "OK" : 2 }, "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Map[String, Wrapper]]](js) should equal(w)
          mongoScalaJack.read[Carry[Map[String, Wrapper]]](db) should equal(w)
        }
        it("Case class having Option parameter - Foo[A](x:A) where A -> Option of value class") {
          val w = Carry("Terri", Wrap("Hobbies", Some(new Wrapper(-2)).asInstanceOf[Option[Wrapper]], "all"))
          val x = Carry[Option[Wrapper]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":-2,"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : -2, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Wrapper]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Wrapper]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Wrapper]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Wrapper]]](db2) should equal(x)
        }
        it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> Bar[Int]") {
          val w = BagList("list", List(Zoo("a", 1), Zoo("b", 2)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"list","many":[{"name":"a","z":1},{"name":"b","z":2}]}""")
          db.asDocument.toJson should equal("""{ "s" : "list", "many" : [{ "name" : "a", "z" : 1 }, { "name" : "b", "z" : 2 }] }""")
          jsonScalaJack.read[BagList[Zoo[Int]]](js) should equal(w)
          mongoScalaJack.read[BagList[Zoo[Int]]](db) should equal(w)
        }
        it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,Bar[Int]") {
          val w = BagMap(5, Map("one" -> Zoo("a", 1), "two" -> Zoo("b", 2)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"i":5,"items":{"one":{"name":"a","z":1},"two":{"name":"b","z":2}}}""")
          db.asDocument.toJson should equal("""{ "i" : 5, "items" : { "one" : { "name" : "a", "z" : 1 }, "two" : { "name" : "b", "z" : 2 } } }""")
          jsonScalaJack.read[BagMap[Zoo[Int]]](js) should equal(w)
          mongoScalaJack.read[BagMap[Zoo[Int]]](db) should equal(w)
        }
        it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> Bar[Int]") {
          val w = Carry("Terri", Wrap("Hobbies", Some(Truck(false, Two("aaa", true))).asInstanceOf[Option[Truck[Boolean]]], "all"))
          val x = Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"s":false,"t":{"foo":"aaa","bar":true}},"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : { "s" : false, "t" : { "foo" : "aaa", "bar" : true } }, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Truck[Boolean]]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Truck[Boolean]]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Truck[Boolean]]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Truck[Boolean]]]](db2) should equal(x)
        }
        it("Case class having List of parameterized value - Foo[A](x:List[A]) - where A -> value class") {
          val w = BagList("list", List(Zoo("a", new Wrapper(1)), Zoo("b", new Wrapper(2))))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"list","many":[{"name":"a","z":1},{"name":"b","z":2}]}""")
          db.asDocument.toJson should equal("""{ "s" : "list", "many" : [{ "name" : "a", "z" : 1 }, { "name" : "b", "z" : 2 }] }""")
          jsonScalaJack.read[BagList[Zoo[Wrapper]]](js) should equal(w)
          mongoScalaJack.read[BagList[Zoo[Wrapper]]](db) should equal(w)
        }
        it("Case class having Map of parameterized value - Foo[A,B](x:Map[A,B]) - where A,B -> String,value class") {
          val w = BagMap(5, Map("one" -> Zoo("a", new Wrapper(1)), "two" -> Zoo("b", new Wrapper(2))))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"i":5,"items":{"one":{"name":"a","z":1},"two":{"name":"b","z":2}}}""")
          db.asDocument.toJson should equal("""{ "i" : 5, "items" : { "one" : { "name" : "a", "z" : 1 }, "two" : { "name" : "b", "z" : 2 } } }""")
          jsonScalaJack.read[BagMap[Zoo[Wrapper]]](js) should equal(w)
          mongoScalaJack.read[BagMap[Zoo[Wrapper]]](db) should equal(w)
        }
        it("Case class having Option of parameterized value - Foo[A](x:Option[A]) - where A -> value class") {
          val w = Carry("Terri", Wrap("Hobbies", Some(Zoo("a", new Wrapper(12))).asInstanceOf[Option[Zoo[Wrapper]]], "all"))
          val x = Carry[Option[Truck[Boolean]]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"name":"a","z":12},"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : { "name" : "a", "z" : 12 }, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Zoo[Wrapper]]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Zoo[Wrapper]]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Zoo[Wrapper]]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Zoo[Wrapper]]]](db2) should equal(x)
        }
      }
      describe("Basic trait support") {
        it("Parameter is a simple trait") {
          val w = Carry[Pop]("Surprise", Wrap("Yellow", Wow2("three", 3), "Done"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":{"_hint":"co.blocke.scalajack.test.Wow2","x":"three","y":3},"stuff":"Done"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Surprise", "w" : { "name" : "Yellow", "data" : { "_hint" : "co.blocke.scalajack.test.Wow2", "x" : "three", "y" : 3 }, "stuff" : "Done" } }""")
          jsonScalaJack.read[Carry[Pop]](js) should equal(w)
          mongoScalaJack.read[Carry[Pop]](db) should equal(w)
        }
        it("Parameter is a simple trait with hint function value mappings") {
          val w = Carry[Pop]("Surprise", Wrap("Yellow", Wow2("three", 4), "Done"))
          val scalaJack = mongoScalaJack.withHintModifiers(
            typeOf[Pop] -> ClassNameHintModifier(
              hint => s"co.blocke.scalajack.test.$hint",
              fullName => fullName.split('.').last
            )
          )
          val db = scalaJack.render(w)
          db.asDocument.toJson should equal("""{ "s" : "Surprise", "w" : { "name" : "Yellow", "data" : { "_hint" : "Wow2", "x" : "three", "y" : 4 }, "stuff" : "Done" } }""")
          scalaJack.read[Carry[Pop]](db) should equal(w)
        }
        it("Parameter is List of trait") {
          val w = Carry[List[Pop]]("Surprise", Wrap("Yellow", List(Wow1("four", 4), Wow2("three", 3)), "Done"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":[{"_hint":"co.blocke.scalajack.test.Wow1","a":"four","b":4},{"_hint":"co.blocke.scalajack.test.Wow2","x":"three","y":3}],"stuff":"Done"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Surprise", "w" : { "name" : "Yellow", "data" : [{ "_hint" : "co.blocke.scalajack.test.Wow1", "a" : "four", "b" : 4 }, { "_hint" : "co.blocke.scalajack.test.Wow2", "x" : "three", "y" : 3 }], "stuff" : "Done" } }""")
          jsonScalaJack.read[Carry[List[Pop]]](js) should equal(w)
          mongoScalaJack.read[Carry[List[Pop]]](db) should equal(w)
        }
        it("Parameter is Map of String->trait") {
          val w = Carry[Map[String, Pop]]("Surprise", Wrap("Yellow", Map("a" -> Wow1("four", 4), "b" -> Wow2("three", 3)), "Done"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Surprise","w":{"name":"Yellow","data":{"a":{"_hint":"co.blocke.scalajack.test.Wow1","a":"four","b":4},"b":{"_hint":"co.blocke.scalajack.test.Wow2","x":"three","y":3}},"stuff":"Done"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Surprise", "w" : { "name" : "Yellow", "data" : { "a" : { "_hint" : "co.blocke.scalajack.test.Wow1", "a" : "four", "b" : 4 }, "b" : { "_hint" : "co.blocke.scalajack.test.Wow2", "x" : "three", "y" : 3 } }, "stuff" : "Done" } }""")
          jsonScalaJack.read[Carry[Map[String, Pop]]](js) should equal(w)
          mongoScalaJack.read[Carry[Map[String, Pop]]](db) should equal(w)
        }
        it("Parameter is an Option of trait") {
          val w = Carry[Option[Pop]]("Terri", Wrap("Hobbies", Some(Wow1("ok", -99)), "all"))
          val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.Wow1","a":"ok","b":-99},"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : { "_hint" : "co.blocke.scalajack.test.Wow1", "a" : "ok", "b" : -99 }, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Pop]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Pop]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Pop]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Pop]]](db2) should equal(x)
        }
        it("List of parameter, where parameter is a trait") {
          val w = BagList[Pop]("list", List(Wow1("A", 1), Wow1("B", 2)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"list","many":[{"_hint":"co.blocke.scalajack.test.Wow1","a":"A","b":1},{"_hint":"co.blocke.scalajack.test.Wow1","a":"B","b":2}]}""")
          db.asDocument.toJson should equal("""{ "s" : "list", "many" : [{ "_hint" : "co.blocke.scalajack.test.Wow1", "a" : "A", "b" : 1 }, { "_hint" : "co.blocke.scalajack.test.Wow1", "a" : "B", "b" : 2 }] }""")
          jsonScalaJack.read[BagList[Pop]](js) should equal(w)
          mongoScalaJack.read[BagList[Pop]](db) should equal(w)
        }
        it("Map of String->parameter, where parameter is a trait") {
          val w = BagMap[Pop](5, Map("one" -> Wow2("q", 7), "two" -> Wow1("r", 3)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"i":5,"items":{"one":{"_hint":"co.blocke.scalajack.test.Wow2","x":"q","y":7},"two":{"_hint":"co.blocke.scalajack.test.Wow1","a":"r","b":3}}}""")
          db.asDocument.toJson should equal("""{ "i" : 5, "items" : { "one" : { "_hint" : "co.blocke.scalajack.test.Wow2", "x" : "q", "y" : 7 }, "two" : { "_hint" : "co.blocke.scalajack.test.Wow1", "a" : "r", "b" : 3 } } }""")
          jsonScalaJack.read[BagMap[Pop]](js) should equal(w)
          mongoScalaJack.read[BagMap[Pop]](db) should equal(w)
        }
        it("Option of parameter, where parameter is a trait") {
          val w = Carry[Option[Pop]]("Terri", Wrap("Hobbies", Some(Wow2("finite", 1000)), "all"))
          val x = Carry[Option[Pop]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.Wow2","x":"finite","y":1000},"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : { "_hint" : "co.blocke.scalajack.test.Wow2", "x" : "finite", "y" : 1000 }, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Pop]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Pop]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Pop]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Pop]]](db2) should equal(x)
        }
      }
      describe("Advanced trait support -- parameters are traits, themselves having parameters") {
        it("Case class having an embedded parameterized trait") {
          val w = Breakfast(true, Toast(7, "Burnt"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":"Burnt"}}""")
          db.asDocument.toJson should equal("""{ "y" : true, "bread" : { "_hint" : "co.blocke.scalajack.test.Toast", "g" : 7, "yum" : "Burnt" } }""")
          jsonScalaJack.read[Breakfast[String]](js) should equal(w)
          mongoScalaJack.read[Breakfast[String]](db) should equal(w)
        }
        it("Case class having an embedded parameterized trait, with the trait's parameter another case class") {
          val w = Breakfast(true, Toast(7, Two("two", true)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":{"foo":"two","bar":true}}}""")
          db.asDocument.toJson should equal("""{ "y" : true, "bread" : { "_hint" : "co.blocke.scalajack.test.Toast", "g" : 7, "yum" : { "foo" : "two", "bar" : true } } }""")
          jsonScalaJack.read[Breakfast[Two]](js) should equal(w)
          mongoScalaJack.read[Breakfast[Two]](db) should equal(w)
        }
        it("Case class having an embedded parameterized trait, with the trait's parameter a value class") {
          val w = Breakfast(true, Toast(7, new Wrapper(-100)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":-100}}""")
          db.asDocument.toJson should equal("""{ "y" : true, "bread" : { "_hint" : "co.blocke.scalajack.test.Toast", "g" : 7, "yum" : -100 } }""")
          jsonScalaJack.read[Breakfast[Wrapper]](js) should equal(w)
          mongoScalaJack.read[Breakfast[Wrapper]](db) should equal(w)
        }
        it("Parameter is a parameterized trait") { // I can't believe this one worked!
          val w = Carry[Tart[Soup[String]]]("Bill", Wrap("Betty", Bun(3, Cruton(8, "eight")), "ok"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Bill","w":{"name":"Betty","data":{"_hint":"co.blocke.scalajack.test.Bun","g":3,"yum":{"_hint":"co.blocke.scalajack.test.Cruton","i":8,"sweet":"eight"}},"stuff":"ok"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Bill", "w" : { "name" : "Betty", "data" : { "_hint" : "co.blocke.scalajack.test.Bun", "g" : 3, "yum" : { "_hint" : "co.blocke.scalajack.test.Cruton", "i" : 8, "sweet" : "eight" } }, "stuff" : "ok" } }""")
          jsonScalaJack.read[Carry[Tart[Soup[String]]]](js) should equal(w)
          mongoScalaJack.read[Carry[Tart[Soup[String]]]](db) should equal(w)
        }
        it("Parameter is List of parameterized trait") {
          val w = Carry[List[Tart[Boolean]]]("Trey", Wrap("Hobbies", List(Bun(1, false), Toast(2, true)), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Trey","w":{"name":"Hobbies","data":[{"_hint":"co.blocke.scalajack.test.Bun","g":1,"yum":false},{"_hint":"co.blocke.scalajack.test.Toast","g":2,"yum":true}],"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Trey", "w" : { "name" : "Hobbies", "data" : [{ "_hint" : "co.blocke.scalajack.test.Bun", "g" : 1, "yum" : false }, { "_hint" : "co.blocke.scalajack.test.Toast", "g" : 2, "yum" : true }], "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[List[Tart[Boolean]]]](js) should equal(w)
          mongoScalaJack.read[Carry[List[Tart[Boolean]]]](db) should equal(w)
        }
        it("Parameter is Map of String->parameterized trait") {
          val w = Carry[Map[String, Tart[String]]]("Troy", Wrap("Articles", Map("OK" -> Bun(27, "Hot")), "all"))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"Troy","w":{"name":"Articles","data":{"OK":{"_hint":"co.blocke.scalajack.test.Bun","g":27,"yum":"Hot"}},"stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Troy", "w" : { "name" : "Articles", "data" : { "OK" : { "_hint" : "co.blocke.scalajack.test.Bun", "g" : 27, "yum" : "Hot" } }, "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Map[String, Tart[String]]]](js) should equal(w)
          mongoScalaJack.read[Carry[Map[String, Tart[String]]]](db) should equal(w)
        }
        it("Parameter is an Option of parameterized trait") {
          val w = Carry[Option[Tart[Int]]]("Terri", Wrap("Hobbies", Some(Toast(11, 12)), "all"))
          val x = Carry[Option[Tart[Int]]]("Terry", Wrap("Hobbies", None, "all"))
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"s":"Terri","w":{"name":"Hobbies","data":{"_hint":"co.blocke.scalajack.test.Toast","g":11,"yum":12},"stuff":"all"}}""")
          js2 should equal("""{"s":"Terry","w":{"name":"Hobbies","stuff":"all"}}""")
          db.asDocument.toJson should equal("""{ "s" : "Terri", "w" : { "name" : "Hobbies", "data" : { "_hint" : "co.blocke.scalajack.test.Toast", "g" : 11, "yum" : 12 }, "stuff" : "all" } }""")
          db2.asDocument.toJson should equal("""{ "s" : "Terry", "w" : { "name" : "Hobbies", "stuff" : "all" } }""")
          jsonScalaJack.read[Carry[Option[Tart[Int]]]](js) should equal(w)
          jsonScalaJack.read[Carry[Option[Tart[Int]]]](js2) should equal(x)
          mongoScalaJack.read[Carry[Option[Tart[Int]]]](db) should equal(w)
          mongoScalaJack.read[Carry[Option[Tart[Int]]]](db2) should equal(x)
        }
        it("List of parameter, where parameter is a parameterized trait") {
          val w = BagList[Tart[Boolean]]("list", List(Toast(1, true), Bun(2, false)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"s":"list","many":[{"_hint":"co.blocke.scalajack.test.Toast","g":1,"yum":true},{"_hint":"co.blocke.scalajack.test.Bun","g":2,"yum":false}]}""")
          db.asDocument.toJson should equal("""{ "s" : "list", "many" : [{ "_hint" : "co.blocke.scalajack.test.Toast", "g" : 1, "yum" : true }, { "_hint" : "co.blocke.scalajack.test.Bun", "g" : 2, "yum" : false }] }""")
          jsonScalaJack.read[BagList[Tart[Boolean]]](js) should equal(w)
          mongoScalaJack.read[BagList[Tart[Boolean]]](db) should equal(w)
        }
        it("Map of String->parameter, where parameter is a parameterized trait") {
          val w = BagMap[Tart[Boolean]](5, Map("one" -> Bun(1, true), "two" -> Toast(2, false)))
          val js = jsonScalaJack.render(w)
          val db = mongoScalaJack.render(w)
          js should equal("""{"i":5,"items":{"one":{"_hint":"co.blocke.scalajack.test.Bun","g":1,"yum":true},"two":{"_hint":"co.blocke.scalajack.test.Toast","g":2,"yum":false}}}""")
          db.asDocument.toJson should equal("""{ "i" : 5, "items" : { "one" : { "_hint" : "co.blocke.scalajack.test.Bun", "g" : 1, "yum" : true }, "two" : { "_hint" : "co.blocke.scalajack.test.Toast", "g" : 2, "yum" : false } } }""")
          jsonScalaJack.read[BagMap[Tart[Boolean]]](js) should equal(w)
          mongoScalaJack.read[BagMap[Tart[Boolean]]](db) should equal(w)
        }
        it("Option of parameter, where parameter is a parameterized trait") {
          val w = BagOpt[Tart[String]](1, Some(Bun(6, "ok")))
          val x = BagOpt[Tart[String]](1, None)
          val js = jsonScalaJack.render(w)
          val js2 = jsonScalaJack.render(x)
          val db = mongoScalaJack.render(w)
          val db2 = mongoScalaJack.render(x)
          js should equal("""{"i":1,"maybe":{"_hint":"co.blocke.scalajack.test.Bun","g":6,"yum":"ok"}}""")
          js2 should equal("""{"i":1}""")
          db.asDocument.toJson should equal("""{ "i" : 1, "maybe" : { "_hint" : "co.blocke.scalajack.test.Bun", "g" : 6, "yum" : "ok" } }""")
          db2.asDocument.toJson should equal("""{ "i" : 1 }""")
          jsonScalaJack.read[BagOpt[Tart[String]]](js) should equal(w)
          jsonScalaJack.read[BagOpt[Tart[String]]](js2) should equal(x)
          mongoScalaJack.read[BagOpt[Tart[String]]](db) should equal(w)
          mongoScalaJack.read[BagOpt[Tart[String]]](db2) should equal(x)
        }
      }
    }
    describe("Thread Safety Test") {
      it("Should not crash when multiple threads access Analyzer (Scala 2.10.x reflection bug)") {
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration._
        import scala.concurrent.{ Await, Future }
        import scala.language.postfixOps
        val doit = () =>
          Try {
            val js = jsonScalaJack.render(Foo("Greg", List("a", "b", "c")))
            jsonScalaJack.read[Foo](js)
          }.toOption.isDefined
        val z = List(
          Future(doit()),
          Future(doit()),
          Future(doit()),
          Future(doit())
        )
        val res = Await.result(Future.sequence(z), 3 seconds).reduce((a, b) => a && b)
        res should be(true)
      }
    }
  }
}
