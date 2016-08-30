package co.blocke.scalajack
package test

import mongo._
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import org.mongodb.scala._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson._
import scala.util.Try
import java.util.UUID
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat

// Just some "bonus" parser read/render tests--not mongo specific.  Could go into core but they evolved here.

class ExtraSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val data = One("Greg", List("a", "b"), List(Two("x", false), Two("y", true)), Two("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)

  val sjM = ScalaJack(MongoFlavor())

  describe("=====================\n| -- Extra Tests -- |\n=====================") {
    describe("Basic Render/Read") {
      it("Serialize simple object to JSON -- all supported data types") {
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""")
      }
      it("De-serialize simple object from JSON -- all supported data types") {
        val js = ScalaJack().render(data)
        js should equal("""{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""")
        val b = ScalaJack().read[One](js)
        b should equal(data)
      }
      it("Should handle UUID types") {
        val thing = UuidThing("Foo", UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), List(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")), Some(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")))
        val js = ScalaJack().render(thing)
        js should equal("""{"name":"Foo","uuid":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","many":["1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"],"maybe":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"}""")
        val b = ScalaJack().read[UuidThing](js)
        b should equal(thing)
      }
      it("Should handle DateTime types") {
        val pattern = "MM-dd-yy"
        val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
        val thing = JodaThing("Foo", t, List(t, t), Some(t))
        val js = ScalaJack().render(thing)
        js should equal("""{"name":"Foo","dt":520560000000,"many":[520560000000,520560000000],"maybe":520560000000}""")
        val b = ScalaJack().read[JodaThing](js)
        b should equal(thing)
      }
      it("Should ignore extra fields in the inbound JSON") {
        val js = """{"foo":"hey","bogus":19,"bar":true}"""
        ScalaJack().read[Two](js) should equal(Two("hey", true))
      }
      it("Should ignore extra fields in the inbound database objects") {
        val dbo = Document(BsonDocument("foo" -> "hey", "bogus" -> 19, "bar" -> true))
        sjM.read[Two](dbo) should equal(Two("hey", true))
      }
      it("Handle empty Lists & Maps") {
        val four = Four(List[String](), Map[String, Int]())
        val js = ScalaJack().render(four)
        js should equal("""{"stuff":[],"things":{}}""")
        ScalaJack().read[Four](js) should equal(four)
      }
      it("Primitive Lists") {
        val pl = PrimitiveLists(List(1, 2, 3), List(3L, 4L), List(true, false), List('a', 'b', 'c'), List(1.2, 3.4))
        val js = ScalaJack().render(pl)
        js should equal("""{"ints":[1,2,3],"longs":[3,4],"bools":[true,false],"chars":["a","b","c"],"doubles":[1.2,3.4]}""")
        ScalaJack().read[PrimitiveLists](js) should equal(pl)
      }
      it("Naked Lists of string") {
        val stuff = List("a", "b", "c")
        val js = ScalaJack().render(stuff)
        js should equal("""["a","b","c"]""")
        ScalaJack().read[List[String]](js) should equal(stuff)
      }
      it("Naked Lists of UUID") {
        val stuff = List(UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"), UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"))
        val js = ScalaJack().render(stuff)
        js should equal("""["1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"]""")
        ScalaJack().read[List[UUID]](js) should equal(stuff)
      }
      it("Naked Lists of Joda") {
        val pattern = "MM-dd-yy"
        val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
        val stuff = List(t, t)
        val js = ScalaJack().render(stuff)
        js should equal("""[520560000000,520560000000]""")
        ScalaJack().read[List[DateTime]](js) should equal(stuff)
      }
      it("Naked Lists of objects") {
        val stuff = List(Three("three", Num.A, Wow1("foo", 17)), Three("four", Num.B, Wow1("bar", 18)))
        val js = ScalaJack().render(stuff)
        js should equal("""[{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}},{"name":"four","two":"B","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"bar","b":18}}]""")
        ScalaJack().read[List[Three]](js) should equal(stuff)
      }
      it("Naked Lists of Boolean") {
        val stuff = List(true, false) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""[true,false]""")
        ScalaJack().read[List[Boolean]](js) should equal(stuff)
      }
      it("Naked Lists of Int") {
        val stuff = List(5, 6) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""[5,6]""")
        ScalaJack().read[List[Int]](js) should equal(stuff)
      }
      it("Naked Lists of Long") {
        val stuff = List(5L, 6L) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""[5,6]""")
        ScalaJack().read[List[Long]](js) should equal(stuff)
      }
      it("Naked Lists of Double") {
        val stuff = List(5.1, 6.2) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""[5.1,6.2]""")
        ScalaJack().read[List[Double]](js) should equal(stuff)
      }
      it("Naked Lists of Char") {
        val stuff = List('a', 'b') // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""["a","b"]""")
        ScalaJack().read[List[Char]](js) should equal(stuff)
      }
      it("Naked Maps of string") {
        val stuff = Map("a" -> "b", "c" -> "d")
        val js = ScalaJack().render(stuff)
        js should equal("""{"a":"b","c":"d"}""")
        ScalaJack().read[Map[String, String]](js) should equal(stuff)
      }
      it("Naked Maps of objects") {
        val stuff = Map("a" -> Three("three", Num.A, Wow1("foo", 17)), "b" -> Three("four", Num.B, Wow1("bar", 18)))
        val js = ScalaJack().render(stuff)
        js should equal("""{"a":{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}},"b":{"name":"four","two":"B","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"bar","b":18}}}""")
        ScalaJack().read[Map[String, Three]](js) should equal(stuff)
      }
      it("Naked Maps of Boolean") {
        val stuff = Map("a" -> true, "b" -> false) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""{"a":true,"b":false}""")
        ScalaJack().read[Map[String, Boolean]](js) should equal(stuff)
      }
      it("Naked Maps of Int") {
        val stuff = Map("a" -> 5, "b" -> 6) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""{"a":5,"b":6}""")
        ScalaJack().read[Map[String, Int]](js) should equal(stuff)
      }
      it("Naked Maps of Long") {
        val stuff = Map("a" -> 5L, "b" -> 6L) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""{"a":5,"b":6}""")
        ScalaJack().read[Map[String, Long]](js) should equal(stuff)
      }
      it("Naked Maps of Double") {
        val stuff = Map("a" -> 5.1, "b" -> 6.2) // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""{"a":5.1,"b":6.2}""")
        ScalaJack().read[Map[String, Double]](js) should equal(stuff)
      }
      it("Naked Maps of Char") {
        val stuff = Map("a" -> 'b', "c" -> 'd') // int, boolean, long, double, char
        val js = ScalaJack().render(stuff)
        js should equal("""{"a":"b","c":"d"}""")
        ScalaJack().read[Map[String, Char]](js) should equal(stuff)
      }
      it("Renders and reads strings with embedded chars (newlines, quotes, etc.)") {
        val w = Two("This is a test\nOf the \"Emergency Broadcast \tSystem\"", true)
        val js = ScalaJack().render(w)
        js should equal("""{"foo":"This is a test\nOf the \"Emergency Broadcast \tSystem\"","bar":true}""")
        ScalaJack().read[Two](js) should equal(w)
      }
      it("Handles a case class of all-optional values, that happen to be None") {
        val ao = AllOpt(None, None, None)
        val js = ScalaJack().render(ao)
        js should equal("""{}""")
        ScalaJack().read[AllOpt](js) should equal(ao)
      }
      it("Handles null values - Double") {
        val js = """{"a":5.1,"b":null}"""
        val o = ScalaJack().read[Map[String, Double]](js)
        o should contain allOf (("a" -> 5.1), ("b" -> null))
      }
      it("Handles null values - Boolean") {
        val js = """{"a":true,"b":null}"""
        val o = ScalaJack().read[Map[String, Boolean]](js)
        o should contain allOf (("a" -> true), ("b" -> null))
      }
      it("Handles null values - String") {
        val js = """{"a":"wow","b":null}"""
        val o = ScalaJack().read[Map[String, String]](js)
        o should contain allOf (("a" -> "wow"), ("b" -> null))
      }
      it("Handles null values - Int") {
        val js = """{"a":5,"b":null}"""
        val o = ScalaJack().read[Map[String, Int]](js)
        o should contain allOf (("a" -> 5), ("b" -> null))
      }
      it("Handles null values - Long") {
        val js = """{"a":5,"b":null}"""
        val o = ScalaJack().read[Map[String, Long]](js)
        o should contain allOf (("a" -> 5L), ("b" -> null))
      }
      it("Handles null values - UUID") {
        val js = """{"a":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","b":null}"""
        val o = ScalaJack().read[Map[String, UUID]](js)
        o should contain allOf (("a" -> UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")), ("b" -> null))
      }
      it("Handles null values - DateTime") {
        val js = """{"a":520560000000,"b":null}"""
        val o = ScalaJack().read[Map[String, DateTime]](js)
        val pattern = "MM-dd-yy"
        val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
        o should contain allOf (("a" -> t), ("b" -> null))
      }
    }
    describe("Trait Support") {
      it("Traits with subclasses") {
        val t = Three("three", Num.A, Wow1("foo", 17))
        val js2 = ScalaJack().render(t)
        js2 should equal("""{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}}""")
        // Change order so hint isn't first in list
        val js3 = """{"name":"three","two":"A","pp":{"a":"foo","_hint":"co.blocke.scalajack.test.Wow1","b":17}}"""
        val u = ScalaJack().read[Three](js3)
        u should equal(t)
      }
      it("Support changing type hint") {
        val t = Three("three", Num.A, Wow1("foo", 17))
        val vc = VisitorContext().withDefaultHint("hey")
        val js2 = ScalaJack().render(t, vc)
        js2 should equal("""{"name":"three","two":"A","pp":{"hey":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}}""")
        val u = ScalaJack().read[Three](js2, vc)
        u should equal(t)
      }
      it("Top-level trait") {
        val w = Wow1("hey", 99)
        val js = ScalaJack().render[Pop](w)
        js should equal("""{"_hint":"co.blocke.scalajack.test.Wow1","a":"hey","b":99}""")
        ScalaJack().read[Pop](js) should equal(w)
      }
    }
    describe("Value Classes") {
      describe("Without custom JSON support") {
        it("Simple value class support") {
          val stuff = ValSupport("foo", new Wrapper(42), false)
          val js = ScalaJack().render(stuff)
          js should equal("""{"name":"foo","wrap":42,"more":false}""")
          ScalaJack().read[ValSupport](js) should equal(stuff)
        }
        it("List of value class without custom JSON support") {
          val stuff = ListValSupport("bar", List(new Wrapper(99), new Wrapper(100)), true)
          val js = ScalaJack().render(stuff)
          js should equal("""{"name":"bar","wrap":[99,100],"more":true}""")
          ScalaJack().read[ListValSupport](js) should equal(stuff)
        }
        it("Option of value class without custom JSON support") {
          val stuff = OptValSupport("hey", Some(new Wrapper(2)))
          val stuff2 = OptValSupport("hey", None)
          val js1 = ScalaJack().render(stuff)
          val js2 = ScalaJack().render(stuff2)
          js1 should equal("""{"name":"hey","wrap":2}""")
          js2 should equal("""{"name":"hey"}""")
          ScalaJack().read[OptValSupport](js1) should equal(stuff)
          ScalaJack().read[OptValSupport](js2) should equal(stuff2)
        }
        it("Map of value class without custom JSON support") {
          val stuff = MapValSupport("hey", Map("blah" -> new Wrapper(2), "wow" -> new Wrapper(3)))
          val js2 = ScalaJack().render(stuff)
          js2 should equal("""{"name":"hey","wrap":{"blah":2,"wow":3}}""")
          ScalaJack().read[MapValSupport](js2) should equal(stuff)
        }
        it("Must read & render custom JSON for value class") {
          val sj = ScalaJack()
          val ss = SomethingSpecial("hey", new CustomVC(new DateTime(2015, 7, 1, 0, 0)))
          val js = sj.render(ss)
          js should equal("""{"what":"hey","when":"July, 2015"}""")
          (sj.read[SomethingSpecial](js.toString) == ss) should be(true)
        }
      }
    }
    describe("Nested Constructs") {
      describe("With Lists") {
        it("List of lists of case classes") {
          val ln = ListList("Fred", List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4))))
          val js = ScalaJack().render(ln)
          js should equal("""{"name":"Fred","stuff":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""")
          ScalaJack().read[ListList](js) should equal(ln)
        }
        it("List of lists of lists of case classes") {
          val ln = ListListList(
            "Fred",
            List(
              List(
                List(
                  Animal("mouse", 4),
                  Animal("bug", 6)
                ),
                List(
                  Animal("whale", 0),
                  Animal("elephant", 4)
                )
              ),
              List(
                List(
                  Animal("millipede", 1000),
                  Animal("slug", 0)
                ),
                List(
                  Animal("bird", 2),
                  Animal("tiger", 4)
                )
              )
            )
          )
          val js = ScalaJack().render(ln)
          js should equal("""{"name":"Fred","stuff":[[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]],[[{"name":"millipede","legs":1000},{"name":"slug","legs":0}],[{"name":"bird","legs":2},{"name":"tiger","legs":4}]]]}""")
          ScalaJack().read[ListListList](js) should equal(ln)
        }
        // NOTE: If your list has a None it it, this will be lost upon re-marshal from JSON as JSON has no representation
        //       for a None (it's simply missing from the list).
        it("List of option of case classes") {
          val lop = ListOpt("Jenny", List(Some(Animal("mouse", 4)), None, Some(Animal("whale", 0))))
          val js = ScalaJack().render(lop)
          js should equal("""{"name":"Jenny","stuff":[{"name":"mouse","legs":4},{"name":"whale","legs":0}]}""")
          ScalaJack().read[ListOpt](js) should equal(lop.copy(stuff = lop.stuff.filter(_.isDefined)))
        }
        it("List of map of case classes") {
          val lm = ListMap("Jenny", List(Map("a" -> Animal("mouse", 4)), Map("b" -> Animal("whale", 0))))
          val js = ScalaJack().render(lm)
          js should equal("""{"name":"Jenny","stuff":[{"a":{"name":"mouse","legs":4}},{"b":{"name":"whale","legs":0}}]}""")
          ScalaJack().read[ListMap](js) should equal(lm)
        }
      }
      describe("With Option") {
        it("Option of list of case classes") {
          val oln = OpList("Wow", Some(List(Animal("mouse", 4), Animal("bug", 6))))
          val js = ScalaJack().render(oln)
          js should equal("""{"name":"Wow","opList":[{"name":"mouse","legs":4},{"name":"bug","legs":6}]}""")
          ScalaJack().read[OpList](js) should equal(oln)
        }
        it("Option of nested list of case classes") {
          val oln = OpListList("Yay", Some(List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
          val js = ScalaJack().render(oln)
          js should equal("""{"name":"Yay","opListList":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""")
          ScalaJack().read[OpListList](js) should equal(oln)
        }
        it("Option of map of case classes") {
          val om = OpMap("Wow", Some(Map("hello" -> (Animal("mouse", 4)))))
          val js = ScalaJack().render(om)
          js should equal("""{"name":"Wow","opMap":{"hello":{"name":"mouse","legs":4}}}""")
          ScalaJack().read[OpMap](js) should equal(om)
          val om2 = OpMap("Wow", None)
          val js2 = ScalaJack().render(om2)
          js2 should equal("""{"name":"Wow"}""")
          ScalaJack().read[OpMap](js2) should equal(om2)
        }
        it("Nested Option of case classes") {
          val oop = OpOp("Oops", Some(Some(Animal("mouse", 4))))
          val js = ScalaJack().render(oop)
          js should equal("""{"name":"Oops","opts":{"name":"mouse","legs":4}}""")
          ScalaJack().read[OpOp](js) should equal(oop)
          val oop2 = OpOp("Oops", None)
          val js2 = ScalaJack().render(oop2)
          js2 should equal("""{"name":"Oops"}""")
          ScalaJack().read[OpOp](js2) should equal(oop2)
        }
      }
      describe("With Map") {
        it("Map of list of case classes") {
          val mln = MapList("Bob", Map("Mike" -> List(Animal("mouse", 4), Animal("bug", 6)), "Sally" -> List(Animal("whale", 0), Animal("elephant", 4))))
          val js = ScalaJack().render(mln)
          js should equal("""{"name":"Bob","mapList":{"Mike":[{"name":"mouse","legs":4},{"name":"bug","legs":6}],"Sally":[{"name":"whale","legs":0},{"name":"elephant","legs":4}]}}""")
          ScalaJack().read[MapList](js) should equal(mln)
        }
        it("Map of nested lists of case classes") {
          val mln = MapListList("Bob", Map("Everyone" -> List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
          val js = ScalaJack().render(mln)
          js should equal("""{"name":"Bob","mapList":{"Everyone":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}}""")
          ScalaJack().read[MapListList](js) should equal(mln)
        }
        it("Map of option of case classes") {
          val a: Option[Animal] = None
          val mln = MapOpt("Bob", Map("things" -> Some(Animal("mouse", 4)), "otherthings" -> a))
          val js = ScalaJack().render(mln)
          js should equal("""{"name":"Bob","mapOpt":{"things":{"name":"mouse","legs":4}}}""")
          ScalaJack().read[MapOpt](js) should equal(mln.copy(mapOpt = mln.mapOpt.filter({ case (k, v) => v.isDefined })))
        }
        it("Map of map of case classes") {
          val mm = MapMap("Bob", Map("things" -> Map("a" -> Animal("mouse", 4), "b" -> Animal("horse", 4)), "stuff" -> Map("c" -> Animal("sloth", 2))))
          val js = ScalaJack().render(mm)
          js should equal("""{"name":"Bob","mapmap":{"things":{"a":{"name":"mouse","legs":4},"b":{"name":"horse","legs":4}},"stuff":{"c":{"name":"sloth","legs":2}}}}""")
          ScalaJack().read[MapMap](js) should equal(mm)
        }
      }
    }
  }
}
