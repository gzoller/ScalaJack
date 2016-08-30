package co.blocke.scalajack
package test.v3

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat

class TestSpec1 extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val data = One("Greg", List("a", "b"), List(Two("x", false), Two("y", true)), Two("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
  val ms = "520560000000"

  describe("=========================\n| -- V3 Tests Part 1 -- |\n=========================") {
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
        js should equal(s"""{"name":"Foo","dt":$ms,"many":[$ms,$ms],"maybe":$ms}""")
        val b = ScalaJack().read[JodaThing](js)
        b should equal(thing)
      }
      it("Should ignore extra fields in the inbound JSON") {
        val js = """{"foo":"hey","bogus":19,"bar":true}"""
        ScalaJack().read[Two](js) should equal(Two("hey", true))
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
        js should equal(s"""[$ms,$ms]""")
        ScalaJack().read[List[DateTime]](js) should equal(stuff)
      }
      it("Naked Lists of objects") {
        val stuff = List(Three("three", Num.A, Wow1("foo", 17)), Three("four", Num.B, Wow1("bar", 18)))
        val js = ScalaJack().render(stuff)
        js should equal("""[{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"foo","b":17}},{"name":"four","two":"B","pp":{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"bar","b":18}}]""")
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
        js should equal("""{"a":{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"foo","b":17}},"b":{"name":"four","two":"B","pp":{"_hint":"co.blocke.scalajack.test.v3.Wow1","a":"bar","b":18}}}""")
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
        o should contain only (("a" -> 5.1), ("b" -> null))
      }
      it("Handles null values - Boolean") {
        val js = """{"a":true,"b":null}"""
        val o = ScalaJack().read[Map[String, Boolean]](js)
        o should contain only (("a" -> true), ("b" -> null))
      }
      it("Handles null values - String") {
        val js = """{"a":"wow","b":null}"""
        val o = ScalaJack().read[Map[String, String]](js)
        o should contain only (("a" -> "wow"), ("b" -> null))
      }
      it("Handles null values - Int") {
        val js = """{"a":5,"b":null}"""
        val o = ScalaJack().read[Map[String, Int]](js)
        o should contain only (("a" -> 5), ("b" -> null))
      }
      it("Handles null values - Long") {
        val js = """{"a":5,"b":null}"""
        val o = ScalaJack().read[Map[String, Long]](js)
        o should contain only (("a" -> 5L), ("b" -> null))
      }
      it("Handles null values - UUID") {
        val js = """{"a":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","b":null}"""
        val o = ScalaJack().read[Map[String, UUID]](js)
        o should contain only (("a" -> UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c")), ("b" -> null))
      }
      it("Handles null values - DateTime") {
        val js = s"""{"a":$ms,"b":null}"""
        val o = ScalaJack().read[Map[String, DateTime]](js)
        val pattern = "MM-dd-yy"
        val t = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
        o should contain only (("a" -> t), ("b" -> null))
      }
    }
  }
}
