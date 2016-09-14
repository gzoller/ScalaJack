package co.blocke.scalajack
package test.v4

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{ DateTime, DateTimeZone }
import org.joda.time.format.DateTimeFormat

class SimpleTestSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
  val sjJS = ScalaJack()

  describe("==================\n| -- V4 Tests -- |\n==================") {
    describe("Render Tests") {
      it("Must render JSON") {
        sjJS.render(Foo("John", 24)) should equal("""{"name":"John","age":24}""")
      }
      it("Must render naked collections") {
        sjJS.render(List(1, 2, 3)) should equal("""[1,2,3]""")
        sjJS.render(Map("a" -> false, "b" -> true)) should equal("""{"a":false,"b":true}""")
        sjJS.render(Set(1, 2, 3)) should equal("""[1,2,3]""")
      }
      it("Must render all primitives") {
        val pattern = "MM-dd-yy"
        val dt = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
        val all = All(
          5,
          new java.lang.Integer(17),
          false,
          new java.lang.String("hey"),
          "you",
          1.2 toFloat,
          1.2 toDouble,
          9223372036854775800L,
          'Z',
          null,
          -14 toByte,
          2 toShort,
          java.util.UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),
          dt
        )
        // println(sjJS.render(all))
        sjJS.render(all) should equal("""{"a":5,"b":17,"c":false,"d":"hey","e":"you","f":1.2,"g":1.2,"h":9223372036854775800,"i":"Z","j":null,"k":-14,"l":2,"m":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","n":520560000000}""")
      }
      it("Must render all collections (non-nested & non-canonical)") {
        val all = AllColl(
          List(1, 2),
          List(Foo("one", 1), Foo("two", 2)),
          None,
          Some("Me"),
          List(Some(1), None, Some(3), None),
          Map("a" -> 1),
          Map(Foo("a", 1) -> Some(WithType(5)), Foo("b", 2) -> Some(WithType(7)), Foo("c", 3) -> None)
        )
        // println(sjJS.render(all,VisitorContext("_hint",true)))
        sjJS.render(all, VisitorContext(isCanonical = false)) should equal("""{"a":[1,2],"b":[{"name":"one","age":1},{"name":"two","age":2}],"d":"Me","e":[1,3],"f":{"a":1},"g":{{"name":"a","age":1}:{"me":5},{"name":"b","age":2}:{"me":7}}}""")
      }
      it("Must render traits") {
        val t = Stuff("wow", Foo("me", 9))
        sjJS.render(t) should equal("""{"item":"wow","other":{"_hint":"co.blocke.scalajack.test.v4.Foo","name":"me","age":9}}""")
      }
      it("Must render traits with hint function value mappings") {
        val t = Stuff("wow", Foo("me", 9))
        val vcx = VisitorContext().copy(hintValueRender = Map("co.blocke.scalajack.test.v4.Blah" -> { (x: String) => x.split('.').last }))
        sjJS.render(t, vcx) should equal("""{"item":"wow","other":{"_hint":"Foo","name":"me","age":9}}""")
      }
      it("Must render typed classes") {
        val a1 = WithType("hey")
        val a2 = WithType(Foo("boom", 9))
        val a3 = WithType(Set(List("a", "b"), List("c")))
        sjJS.render(a1) should equal("""{"me":"hey"}""")
        sjJS.render(a2) should equal("""{"me":{"name":"boom","age":9}}""")
        sjJS.render(a3) should equal("""{"me":[["a","b"],["c"]]}""")
      }
      it("Must render Enumerations") {
        val all = EnumExer(Colors.Red, Formats.JSON)
        sjJS.render(all) should equal("""{"a":"Red","b":"JSON"}""")
      }
      it("Must render value classes") {
        val a1 = new Wrapper("test")
        val a2 = new Wrapper2(7)
        val a3 = Wrapped(new Wrapper("foo"), 1)
        val a4 = Wrapped2(new Wrapper2(true), 9)
        sjJS.render(a1) should equal("\"test\"")
        sjJS.render(a2) should equal("7")
        sjJS.render(a3) should equal("""{"hey":"foo","you":1}""")
        sjJS.render(a4) should equal("""{"hey":true,"you":9}""")
      }
      it("Must support unicode") {
        val a = WithType("następujących")
        sjJS.render(a) should equal("{\"me\":\"nast\\u0119puj\\u0105cych\"}")
      }
      it("Must handle a case class with default values - defaults specified") {
        val wd = WithDefaults("Greg", 49, Some(5), Some(false), GrumpyPet(Cat("Fluffy"), "fish"))
        sjJS.render(wd) should equal("""{"name":"Greg","age":49,"num":5,"hasStuff":false,"pet":{"_hint":"co.blocke.scalajack.test.v4.GrumpyPet","kind":{"_hint":"co.blocke.scalajack.test.v4.Cat","name":"Fluffy"},"food":"fish"}}""")
      }
      it("Must handle a case class with default values - defaults not specified") {
        val wd = WithDefaults("Greg", 49, Some(5))
        sjJS.render(wd) should equal("""{"name":"Greg","age":49,"num":5,"hasStuff":true,"pet":{"_hint":"co.blocke.scalajack.test.v4.NicePet","kind":{"_hint":"co.blocke.scalajack.test.v4.Dog","name":"Fido"},"food":"bones"}}""")
      }
    }

    describe("Read Tests") {
      it("Must read simple JSON") {
        val js = """{"name":"Fred","age":29,"bogus":false,"addr":{"street":"123 Main","zip":29384}}"""
        val z = sjJS.read[Pristine](js, VisitorContext().copy(isValidating = true))
        (z == Pristine("Fred", 29, None, Address("123 Main", 29384))) should be(true)
      }
      it("Must read all primitive types") {
        val js = """{"a":5,"b":17,"c":false,"d":"hey","e":"you","f":1.2,"g":1.2,"h":9223372036854775800,"i":"Z","j":null,"k":-14,"l":2,"m":"1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c","n":520560000000,"o":null}"""
        val z = sjJS.read[All](js, VisitorContext().copy(isValidating = true))
        val pattern = "MM-dd-yy"
        val dt = DateTime.parse("07-01-86", DateTimeFormat.forPattern(pattern).withZoneUTC())
        val all = All(
          5,
          new java.lang.Integer(17),
          false,
          new java.lang.String("hey"),
          "you",
          1.2 toFloat,
          1.2 toDouble,
          9223372036854775800L,
          'Z',
          null,
          -14 toByte,
          2 toShort,
          java.util.UUID.fromString("1e6c2b31-4dfe-4bf6-a0a0-882caaff0e9c"),
          dt
        )
        all should equal(z)
      }
      it("Must read naked collections") {
        sjJS.read[List[Int]]("""[1,2,3]""") should equal(List(1, 2, 3))
        sjJS.read[List[Map[Int, Boolean]]]("""[{5:false,6:true},{10:false,11:true}]""", VisitorContext().copy(isValidating = true, isCanonical = false)) should equal(List(Map(5 -> false, 6 -> true), Map(11 -> true, 10 -> false)))
        sjJS.read[Map[String, Boolean]]("""{"a":false,"b":true}""") should equal(Map("a" -> false, "b" -> true))
        sjJS.read[Set[Int]]("""[1,2,3]""") should equal(Set(1, 2, 3))
      }
      it("Must read all collection types") {
        val all = AllColl(
          List(1, 2),
          List(Foo("one", 1), Foo("two", 2)),
          None,
          Some("Me"),
          List(Some(1), None, Some(3), None),
          Map("a" -> 1),
          Map(Foo("a", 1) -> Some(WithType(5)), Foo("b", 2) -> Some(WithType(7)), Foo("c", 3) -> None)
        )
        val js = """{"a":[1,2],"b":[{"name":"one","age":1},{"name":"two","age":2}],"d":"Me","e":[1,3],"f":{"a":1},"g":{{"name":"a","age":1}:{"me":5},{"name":"b","age":2}:{"me":7}}}"""
        val z = sjJS.read[AllColl](js, VisitorContext().copy(isValidating = true, isCanonical = false))
        // filter out the None values for comparison
        (all.copy(e = all.e.filter(_.isDefined), g = all.g.filter(_._2.isDefined)) == z) should be(true)
      }
      it("Must read traits") {
        val js = """{"item":"wow","other":{"name":"me","_hint":"co.blocke.scalajack.test.v4.Foo","age":9}}"""
        val z = sjJS.read[Stuff](js, VisitorContext().copy(isValidating = true))
        val t = Stuff("wow", Foo("me", 9))
        (z == t) should equal(true)
      }
      it("Must read traits with hint function value mappings") {
        val js = """{"item":"wow","other":{"_hint":"Foo","name":"me","age":9}}"""
        val vcx = VisitorContext().copy(hintValueRead = Map("co.blocke.scalajack.test.v4.Blah" -> { (x: String) => "co.blocke.scalajack.test.v4." + x }))
        sjJS.read[Stuff](js, vcx) should equal(Stuff("wow", Foo("me", 9)))
      }
      it("Must read parameterized classes - Basic") {
        val a1 = WithType("hey")
        val a2 = WithType(Foo("boom", 9))
        val a3 = WithType(Set(List("a", "b"), List("c")))
        sjJS.read[WithType[String]]("""{"me":"hey"}""") should equal(a1)
        sjJS.read[WithType[Foo]]("""{"me":{"name":"boom","age":9}}""") should equal(a2)
        sjJS.read[WithType[Set[List[String]]]]("""{"me":[["a","b"],["c"]]}""") should equal(a3)
      }
      it("Must read parameterized classes - Case class having a defined parameterized type") {
        val c1 = Case_1("Greg", List(WithType(1), WithType(2)))
        val jsC1 = """{"name":"Greg","other":[{"me":1},{"me":2}]}"""
        (sjJS.read[Case_1](jsC1) == c1) should be(true)
      }
      it("Must read parameterized classes - Parameterized case class having parameterized type") {
        val c2 = Case_2("Greg", List(WithType(1), WithType(2)))
        val jsC2 = """{"name":"Greg","other":[{"me":1},{"me":2}]}"""
        (sjJS.read[Case_2[Int]](jsC2) == c2) should be(true)
      }
      it("Must read parameterized classes - Parameterized case class having 2 param types--one specified, one not") {
        val c3 = Case_3("Greg", List(TwoP(4, true), TwoP(3, true)))
        val jsC3 = """{"name":"Greg","other":[{"a":4,"b":true},{"a":3,"b":true}]}"""
        (sjJS.read[Case_3[Int]](jsC3) == c3) should be(true)
      }
      it("Must read parameterized classes - Naked collection (nested) of parameterized type") {
        val c4 = List(List(WithType("foo")))
        val jsC4 = """[[{"me":"foo"}]]"""
        (sjJS.read[List[List[WithType[String]]]](jsC4) == c4) should be(true)
      }
      it("Must read parameterized classes - Multiple parameters, consumed out-of-order") {
        val c5 = Case_5("Greg", List(TwoP(5, "five"), TwoP(6, "six")))
        val jsC5 = """{"name":"Greg","other":[{"a":5,"b":"five"},{"a":6,"b":"six"}]}"""
        (sjJS.read[Case_5[String, Int]](jsC5) == c5) should be(true)
      }
      it("Must read parameterized classes - Parameterized trait") {
        val t1 = Ex1("hey", 99)
        val jsT1 = """{"_hint":"co.blocke.scalajack.test.v4.Ex1","a":"hey","b":99}"""
        (sjJS.read[Excite[Int, String]](jsT1) == t1) should be(true)
      }
      it("Must read parameterized classes - Nested parameterized trait") {
        val t2 = PairOfMeals(Lunch('a'), Lunch(false))
        val jsT2 = """{"_hint":"co.blocke.scalajack.test.v4.PairOfMeals","a":{"_hint":"co.blocke.scalajack.test.v4.Lunch","drink":"a"},"b":{"_hint":"co.blocke.scalajack.test.v4.Lunch","drink":false}}"""
        sjJS.read[PairOfValues[Meal[Char], Meal[Boolean]]](jsT2) should be(t2)
      }
      it("Must read parameterized classes - Nested parameterized trait, partially applied") {
        val t3 = Ex3("wow", false)
        val jsT3 = """{"_hint":"co.blocke.scalajack.test.v4.Ex3","a":"wow","b":false}"""
        (sjJS.read[PairOfValues[String, Boolean]](jsT3) == t3) should be(true)
      }
      it("Must read Enumerations") {
        val js = """{"a":"Red","b":"JSON"}"""
        val z = sjJS.read[EnumExer](js, VisitorContext().copy(isValidating = true))
        val all = EnumExer(Colors.Red, Formats.JSON)
        (all == z) should be(true)
      }
      it("Must read value classes") {
        val a1 = new Wrapper("test")
        val a2 = new Wrapper2(7)
        val a3 = Wrapped(new Wrapper("foo"), 1)
        val a4 = Wrapped2(new Wrapper2(true), 9)
        val js1 = "\"test\""
        val js2 = "7"
        val js3 = """{"hey":"foo","you":1}"""
        val js4 = """{"hey":true,"you":9}"""
        val js5 = """["a","b","c"]"""
        sjJS.read[Wrapper](js1) should equal(a1)
        // (sjJS.read[Wrapper2[Int]](js2) == a2) should be( true )  <-- Can't parse "7"
        sjJS.read[Wrapped](js3) should equal(a3)
        sjJS.read[Wrapped2[Boolean]](js4) should equal(a4)
      }
      it("Must read unicode") {
        val js = "{\"me\":\"nast\\u0119puj\\u0105cych\"}"
        val a = WithType("następujących")
        sjJS.read[WithType[String]](js) should equal(a)
      }
      it("Must handle a case class with default values - defaults specified") {
        val wd = WithDefaults("Greg", 49, Some(5), Some(false), GrumpyPet(Cat("Fluffy"), "fish"))
        val js = """{"name":"Greg","age":49,"num":5,"hasStuff":false,"pet":{"_hint":"co.blocke.scalajack.test.v4.GrumpyPet","kind":{"_hint":"co.blocke.scalajack.test.v4.Cat","name":"Fluffy"},"food":"fish"}}"""
        sjJS.read[WithDefaults](js) should equal(wd)
      }
      it("Must handle a case class with default values - defaults not specified") {
        val wd = WithDefaults("Greg", 49, Some(5))
        val js = """{"name":"Greg","age":49,"num":5,"hasStuff":true,"pet":{"_hint":"co.blocke.scalajack.test.v4.NicePet","kind":{"_hint":"co.blocke.scalajack.test.v4.Dog","name":"Fido"},"food":"bones"}}"""
        sjJS.read[WithDefaults](js) should equal(wd)
      }
    }
    describe("Annotation Tests") {
      it("Must analyze annotations for Collection and DBKey") {
        (pending)
        // val t = Analyzer.inspectByName[Decorated]("co.blocke.scalajack.test.v4.Decorated").asInstanceOf[CCType]
        // t.collAnno should equal(Some("mystuff"))
        // t.members.values.map(_._1.isDbKey) should equal(List(true, false, false))
      }
    }
    describe("Support Custom JSON for Value Class") {
      it("Must read & render custom JSON for value class") {
        val vc = VisitorContext().withAdapter(SpecialAdapter)
        val ss = SomethingSpecial("hey", new CustomVC(new DateTime(2015, 7, 1, 0, 0)))
        val js = sjJS.render(ss,vc)
        js should equal("""{"what":"hey","when":"July, 2015"}""")
        (sjJS.read[SomethingSpecial](js.toString,vc) == ss) should be(true)
      }
    }
    describe("Type Hint Handling") {
      it("Must handle changing default type hint") {
        val pets = List(Dog("Fido"), Cat("Meow"))
        sjJS.render[List[Animal]](pets, VisitorContext(hintMap = Map("default" -> "kind"))) should equal("""[{"kind":"co.blocke.scalajack.test.v4.Dog","name":"Fido"},{"kind":"co.blocke.scalajack.test.v4.Cat","name":"Meow"}]""")
      }
      it("Must handle per-trait (trait specific) type hinting") {
        val pets = List(NicePet(Dog("Fido"), "kibbles"), GrumpyPet(Cat("Meow"), "fish"))
        val js = sjJS.render[List[Pet]](pets, VisitorContext(hintMap = Map(
          "default" -> "_hint",
          "co.blocke.scalajack.test.v4.Pet" -> "_happy", "co.blocke.scalajack.test.v4.Animal" -> "_kind"
        )))
        js should equal("""[{"_happy":"co.blocke.scalajack.test.v4.NicePet","kind":{"_kind":"co.blocke.scalajack.test.v4.Dog","name":"Fido"},"food":"kibbles"},{"_happy":"co.blocke.scalajack.test.v4.GrumpyPet","kind":{"_kind":"co.blocke.scalajack.test.v4.Cat","name":"Meow"},"food":"fish"}]""")
        sjJS.read[List[Pet]](js, VisitorContext(hintMap = Map(
          "default" -> "_hint",
          "co.blocke.scalajack.test.v4.Pet" -> "_happy", "co.blocke.scalajack.test.v4.Animal" -> "_kind"
        ))) should equal(pets)
      }
    }
    describe("View/Splice Tests") {
      it("Must process view") {
        (pending)
        // val data = One("Greg", List("a", "b"), List(Two("x", false), Two("y", true)), Two("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
        // ScalaJack.view[OneSub2](data) should equal(OneSub2("Greg", true, Map("hey" -> 17, "you" -> 21)))
      }
      it("Must spliceWith") {
        (pending)
        // val data = One("Greg", List("a", "b"), List(Two("x", false), Two("y", true)), Two("Nest!", true), Some("wow"), Map("hey" -> 17, "you" -> 21), true, 99123986123L, Num.C, 46)
        // val x = ScalaJack.view[OneSub1](data)
        // val y: One = ScalaJack.spliceInto(x.copy(name = "Fred", big = 2L), data)
        // y should equal(data.copy(name = "Fred", big = 2L))
      }
    }
  }
}
