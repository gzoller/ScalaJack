package co.blocke.scalajack
package json.misc

import org.scalatest.{ FunSpec, Matchers }

case class Bogus(num: Int, unneeded: Option[Boolean], t: Option[(Int, Boolean)] = Some((5, true)))

trait TypeTrait {
  val thing: String
}
case class AThing(thing: String) extends TypeTrait
case class WithType[+T](a: T)

class PlugHoles() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("----------------------------\n:  Plug Holes in Coverage  :\n----------------------------") {
    it("Tokenizatino") {
      val js =
        """{ "name"  :   "Fred",
            |"age" : 12}""".stripMargin
      assertResult(Map("name" -> "Fred", "age" -> 12)) {
        sj.read[Map[String, Any]](js)
      }
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
      val msg = """[$[1]]: Expected comma here.
                  |["a""In the dark night when the wolves roam wild did th
                  |-----^""".stripMargin
      the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[List[String]](js) should have message msg
    }
    it("ScalaJack") {
      val sj = ScalaJack.apply(json.JsonFlavorMaker)
      sj.read[Int]("15") should be(15)
    }
    it("End of TA chain") {
      the[IllegalArgumentException] thrownBy model.DefaultTypeAdapterFactory.typeAdapterOf[Any](null)(sj.context, TypeTags.of(typeOf[Any])) should have message """Unable to find a type adapter for Any"""
    }
    it("Map reading (json)") {
      val js = """{"a":5"""
      val msg =
        """[$]: Expected comma here.
          |{"a":5
          |------^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[Map[String, Int]](js) should have message msg
      val js2 = """{"a""""
      val msg2 =
        """[$.a]: Expected a colon here
          |{"a"
          |----^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[Map[String, Int]](js2) should have message msg2
    }
    it("Object reading (json)") {
      val js = """{5:5}"""
      val msg =
        """[$]: Expected a JSON string here
          |{5:5}
          |-^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[Bogus](js) should have message msg
    }
    it("Tuple reading (json)") {
      val js = """[12"""
      val msg =
        """[$]: Expected comma here.
          |[12
          |---^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[(Int, Int)](js) should have message msg
      sj.read[(Int, Int)]("null") should be(null)
      val msg2 =
        """[$]: Expected an Tuple (Array) but parsed Number
          |123
          |^""".stripMargin
      the[model.ReadUnexpectedError] thrownBy sj.read[(Int, Int)]("123") should have message msg2
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
    val msg = """[$]: Expected an Array but parsed String
                |"Fred"
                |-^""".stripMargin
    the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[List[Int]](jsNotAnArray) should have message msg
    assertResult(null) { sj.read[Map[String, Int]](jsNull) }
    val msg2 = """[$]: Expected a Map but parsed String
                |"Fred"
                |-^""".stripMargin
    the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Map[String, Int]](jsNotAnArray) should have message msg2
    val badNumber = "12.34.56"
    val msg3 = """[$]: Failed to create BigDecimal value from parsed text 12.34.56
                |12.34.56
                |^""".stripMargin
    the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[BigDecimal](badNumber) should have message msg3
    val msg4 = """[$]: Failed to create BigInt value from parsed text 12.34.56
                |12.34.56
                |^""".stripMargin
    the[co.blocke.scalajack.model.ReadMalformedError] thrownBy sj.read[BigInt](badNumber) should have message msg4
    assertResult(null) { sj.read[Bogus](jsNull) }
    val msg5 = """[$]: Expected an Object (map with String keys) but parsed String
                |"Fred"
                |-^""".stripMargin
    the[co.blocke.scalajack.model.ReadUnexpectedError] thrownBy sj.read[Bogus](jsNotAnArray) should have message msg5
    val strSlash = "\"This\\\\that\""
    assertResult("""This\that""") { sj.read[String](strSlash) }
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
      val js4 = """true"""
      val inst = sj.read[Any](js4)
      (inst == true) should be(true)
      sj.render[Map[Any, Int]](Map(Map("a" -> 3) -> 5)) should be("""{"{\"a\":3}":5}""")
      sj.render[Map[Any, Int]](Map(Map(Some("a") -> 3) -> 5)) should be("""{"{\"a\":3}":5}""")
      sj.render[Map[Any, Int]](Map(Map(Some(List(1, 2, 3)) -> 3) -> 5)) should be("""{"{\"[1,2,3]\":3}":5}""")
      sj.render[Map[Any, Int]](Map(Map(Map(1 -> 2) -> 3) -> 5)) should be("""{"{\"{\\\"1\\\":2}\":3}":5}""")
      sj.render[Map[Any, Any]](Map(Map(None -> 3) -> None)) should be("""{}""")
      sj.render[Map[Int, Any]](Map(1 -> Some(3), 2 -> None)) should be("""{"1":3}""")
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
    it("Extra chars in JSON") {
      val js = """[1,2,3]]"""
      val msg = """[$]: Extra input after read.
                  |[1,2,3]]
                  |-------^""".stripMargin
      the[co.blocke.scalajack.model.ReadInvalidError] thrownBy sj.read[List[Int]](js) should have message msg
    }
    it("Classes") {
      val msg = """[$]: Class Bogus missing field num
                  |{}
                  |--^""".stripMargin
      the[co.blocke.scalajack.model.ReadMissingError] thrownBy sj.read[Bogus]("{}") should have message msg
    }
    it("Case class defaults and Option") {
      val js = """{"num":5}"""
      val inst = sj.read[Bogus](js)
      inst should be(Bogus(5, None, Some((5, true))))
    }
    it("Traits") {
      val js = "15"
      the[model.ReadUnexpectedError] thrownBy sj.read[Pet](js) should have message """[$]: Expected start of an object but read token Number"""
    }
    it("Type") {
      val t = sj.read[Type]("\"scala.collection.immutable.List\"")
      sj.render(t) should be("\"scala.collection.immutable.List\"")
    }
  }
}