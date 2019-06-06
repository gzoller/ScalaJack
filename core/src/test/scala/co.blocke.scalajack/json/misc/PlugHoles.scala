package co.blocke.scalajack
package json.misc

import org.scalatest.Matchers
import org.scalatest.funspec.AnyFunSpec
import util.TypeTags

case class Bogus(num: Int, unneeded: Option[Boolean], t: Option[(Int, Boolean)] = Some((5, true)))

trait TypeTrait {
  val thing: String
}
case class AThing(thing: String) extends TypeTrait
case class WithType[+T](a: T)

case class Falling[T](id: T) {
  type foo = T
}

class PlugHoles() extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe("----------------------------\n:  Plug Holes in Coverage  :\n----------------------------") {
    it("Tokenizatino") {
      val js =
        """{ "name"  :   "Fred",
            |"age" : 12}""".stripMargin
      assertResult(Map("name" -> "Fred", "age" -> 12)) {
        sj.read[Map[String, Any]](js)
      }

      val js2 = """{"thing":"blather on about some stupid subject that no one really cares about"n}"""
      val msg =
        """[<tokenizing>]: Unexpected character 'n' at position 78
          |ome stupid subject that no one really cares about"n}
          |--------------------------------------------------^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Map[String, String]](js2) should have message msg

      val js3 = """{"thing":"blather"n}"""
      val msg2 =
        """[<tokenizing>]: Unexpected character 'n' at position 18
          |{"thing":"blather"n}
          |------------------^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Map[String, String]](js3) should have message msg2
    }
    it("String must break") {
      val js = """{"s1":something,"s2":-19,"s3":null}"""
      val msg = """[<tokenizing>]: Unexpected character s at position 6
                  |{"s1":something,"s2":-19,"s3":null}
                  |------^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Map[String, Any]](js) should have message msg
    }
    it("Bad n char") {
      val js = """{"sx":new}"""
      val msg = """[<tokenizing>]: Unexpected character 'n' at position 6
                  |{"sx":new}
                  |------^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Map[String, Any]](js) should have message msg
    }
    it("Bad t char") {
      val js = """{"sx":test}"""
      val msg = """[<tokenizing>]: Unexpected character 't' at position 6
                  |{"sx":test}
                  |------^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Map[String, Any]](js) should have message msg
    }
    it("Long json error") {
      val js = """["a""In the dark night when the wolves roam wild did the little rabbit dispair of life itself."]"""
      val msg = """[$[1]]: Expected Comma here but found String
                  |...wild did the little rabbit dispair of life itself."]
                  |----------------------------------------------------^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[List[String]](js) should have message msg
    }
    it("ScalaJack") {
      val sj = ScalaJack.apply(json.JsonFlavor())
      sj.read[Int]("15") should be(15)
    }
    it("StringBuilder") {
      val s = new compat.StringBuilder()
      s += "Greg"
      s.clear()
      s.result() should be("")
    }
    it("End of TA chain") {
      the[IllegalArgumentException] thrownBy model.DefaultTypeAdapterFactory.typeAdapterOf[Any](null)(sj.context, TypeTags.of(typeOf[Any])) should have message """Unable to find a type adapter for Any (may be abstract or a dependency of an abstract class)"""
    }
    it("Map reading (json)") {
      val js = """{"a":5"""
      val msg =
        """[$]: Expected Comma here but found End
          |{"a":5
          |------^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[Map[String, Int]](js) should have message msg
      val js2 = """{"a""""
      val msg2 =
        """[$.a]: Expected Colon here but found End
          |{"a"
          |----^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[Map[String, Int]](js2) should have message msg2
    }
    it("Object reading (json)") {
      val js = """{5:5}"""
      val msg =
        """[$]: Expected String here but found Number
          |{5:5}
          |-^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[Bogus](js) should have message msg
    }
    it("Tuple reading (json)") {
      val js = """[12"""
      val msg =
        """[$]: Expected Comma here but found End
          |[12
          |---^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[(Int, Int)](js) should have message msg
      sj.read[(Int, Int)]("null") should be(null)
      val msg2 =
        """[$]: Expected BeginArray here but found Number
                |123
                |--^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[(Int, Int)]("123") should have message msg2
      sj.read[(Int, Int)]("null") should be(null)
    }
  }
  it("JsonWriter") {
    val thing: List[BigInt] = List(BigInt(1), null, BigInt(2))
    assertResult("""[1,null,2]""") { sj.render(thing) }
    val nullList: List[Int] = null
    assertResult("null") { sj.render(nullList) }
    val nullMap: Map[String, Int] = null
    assertResult("null") { sj.render(nullMap) }
    val strSlash = """This\that"""
    assertResult("\"This\\\\that\"") { sj.render(strSlash) }
    val nullObj: Bogus = null
    assertResult("null") { sj.render(nullObj) }
    val nullBigInt: BigInt = null
    assertResult("null") { sj.render(nullBigInt) }
  }
  it("JsonReader") {
    val jsNull = "null"
    assertResult(null) { sj.read[List[Int]](jsNull) }
    val jsNotAnArray = """"Fred""""
    val msg = """[$]: Expected BeginArray here but found String
                |"Fred"
                |----^""".stripMargin
    the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[List[Int]](jsNotAnArray) should have message msg
    assertResult(null) { sj.read[Map[String, Int]](jsNull) }
    val msg2 = """[$]: Expected BeginObject here but found String
                |"Fred"
                |----^""".stripMargin
    the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Map[String, Int]](jsNotAnArray) should have message msg2
    val badNumber = "12.34.56"
    val msg3 = """[$]: Unable to read value (e.g. bad number format)
                |12.34.56
                |-------^""".stripMargin
    the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[BigDecimal](badNumber) should have message msg3
    val msg4 = """[$]: Unable to read value (e.g. bad number format)
                |12.34.56
                |-------^""".stripMargin
    the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[BigInt](badNumber) should have message msg4
    assertResult(null) { sj.read[Bogus](jsNull) }
    val msg5 = """[$]: Expected BeginObject here but found String
                |"Fred"
                |----^""".stripMargin
    the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Bogus](jsNotAnArray) should have message msg5
    val strSlash = "\"This\\\\that\""
    assertResult("""This\that""") { sj.read[String](strSlash) }
    val js = """{"_hint":33,"thing":"hah"}"""
    val msg6 =
      """[$._hint]: Couldn't find expected type hint '_hint' for trait co.blocke.scalajack.json.misc.TypeTrait
        |{"_hint":33,"thing":"hah"}
        |-------------------------^""".stripMargin
    the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[TypeTrait](js) should have message msg6
  }
  describe("TypeAdapters") {
    it("Any") {
      val js = """[1,2,3]"""
      assertResult(List(1, 2, 3)) {
        sj.read[Any](js)
      }
      val js2 = """{"_hint":"co.blocke.scalajack.json.misc.SimpleHasDefaults","name":"Fred"}"""
      assertResult(SimpleHasDefaults("Fred", 5)) {
        sj.read[Any](js2)
      }
      val js3 = """{"name":"Fred"}"""
      assertResult(Map("name" -> "Fred")) {
        sj.read[Any](js3)
      }

      val js4 = """["5","true","Fred"]"""
      val sjp = ScalaJack().allowPermissivePrimitives()
      val inst1 = sjp.read[List[Any]](js4)
      inst1 should be(List(BigInt(5), true, "Fred"))
      val inst2 = sj.read[List[Any]](js4)
      inst2 should be(List("5", "true", "Fred"))

      sj.render[Map[Any, Int]](Map(Map("a" -> 3) -> 5)) should be("""{"{\"a\":3}":5}""")
      sj.render[Map[Any, Int]](Map(Map(Some("a") -> 3) -> 5)) should be("""{"{\"a\":3}":5}""")
      sj.render[Map[Any, Int]](Map(Map(Some(List(1, 2, 3)) -> 3) -> 5)) should be("""{"{\"[1,2,3]\":3}":5}""")
      sj.render[Map[Any, Int]](Map(Map(Map(1 -> 2) -> 3) -> 5)) should be("""{"{\"{\\\"1\\\":2}\":3}":5}""")
      sj.render[Map[Any, Any]](Map(Map(None -> 3) -> None)) should be("""{}""")
      sj.render[Map[Int, Any]](Map(1 -> Some(3), 2 -> None)) should be("""{"1":3}""")

      sj.read[Any]("""{"_hint":"bogus","a":3,"b":2}""") should be(Map("_hint" -> "bogus", "a" -> 3, "b" -> 2))
    }
    it("Fallback") {
      val sjf = ScalaJack().parseOrElse(typeOf[Falling[Int]] -> typeOf[String])
      assertResult(true) { sjf.read[Falling[Int]]("""{"foo":"scala.Int", "id":5}""") == Falling(5) }
      sjf.read[Falling[Int]]("\"wow\"") should be("wow")
    }
    it("Tuples") {
      val jsNull = "null"
      assertResult(null) { sj.read[(Int, Boolean)](jsNull) }
    }
    it("Options") {
      val js = """{"num":11}"""
      //case class Bogus(num: Int, unneeded: Option[Boolean], t: Option[(Int, Boolean)] = Some((5, true)))
      assertResult(Bogus(11, None, Some((5, true)))) {
        sj.read[Bogus](js)
      }
      //      val inst: Option[_] = None
      //      sj.render(inst)
    }
    it("Map") {
      val m: Map[Any, Any] = sj.read[Map[Any, Any]]("""{"1":"5"}""")
      m should be(Map(1 -> "5"))

      val m2: Map[Any, Any] = Map(1 -> 3)
      sj.render(m2) should equal("""{"1":3}""")
    }
    it("Extra chars in JSON") {
      val js = """[1,2,3]]"""
      val msg = """[$]: Extra input after read
                  |[1,2,3]]
                  |-------^""".stripMargin
      the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[List[Int]](js) should have message msg
    }
    it("Classes") {
      val msg = """[$]: Class Bogus missing field num
                  |{}
                  |-^""".stripMargin
      the[co.blocke.scalajack.model.ReadMissingError] thrownBy sj.read[Bogus]("{}") should have message msg
    }
    it("Case class defaults and Option") {
      val js = """{"num":5}"""
      val inst = sj.read[Bogus](js)
      inst should be(Bogus(5, None, Some((5, true))))
    }
    it("Traits") {
      val js = "15"
      the[model.ReadUnexpectedError] thrownBy sj.read[Pet](js) should have message
        """[$]: Expected start of an object but read token Number
          |15
          |-^""".stripMargin
    }
    it("Type") {
      val t = sj.read[Type]("\"scala.collection.immutable.List\"")
      sj.render(t) should be("\"scala.collection.immutable.List\"")
    }
  }
}