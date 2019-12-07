package co.blocke.scalajack
package json.misc

import co.blocke.scalajack.compat.StringBuilder

import scala.reflect.runtime.universe._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import util.TypeTags

case class Bogus(num: Int,
                 unneeded: Option[Boolean],
                 t: Option[(Int, Boolean)] = Some((5, true)))

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

  describe(
    "----------------------------\n:  Plug Holes in Coverage  :\n----------------------------"
  ) {
    it("Tokenizatino") {
      val js =
        """{ "name"  :   "Fred",
          |"age" : 12}""".stripMargin
      assertResult(Map("name" -> "Fred", "age" -> 12)) {
        sj.read[Map[String, Any]](js)
      }

      val js2 =
        """{"thing":"blather on about some stupid subject that no one really cares about"n}"""
      val msg =
        """Expected comma here
          |...me stupid subject that no one really cares about"n}
          |----------------------------------------------------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Map[String, String]](js2) should have message msg

      val js3 = """{"thing":"blather"n}"""
      val msg2 =
        """Expected comma here
          |{"thing":"blather"n}
          |------------------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Map[String, String]](js3) should have message msg2
    }
    it("Missing end of list bracket") {
      val js = """[12,5"""
      val msg =
        """Expected end of list here
          |[12,5
          |-----^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[List[Int]](js) should have message msg
    }
    it("Expected colon in Map") {
      val js = """{"a",true}"""
      val msg =
        """Expected colon here
          |{"a",true}
          |----^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Map[String, Boolean]](js) should have message msg
    }
    it("Missing ending brace in Map") {
      val js = """{"a":true"""
      val msg =
        """Expected end of object here
          |{"a":true
          |---------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Map[String, Boolean]](js) should have message msg
    }
    it("Missing ending brace in Object") {
      val js = """{"name":"Mike","age":35"""
      val msg =
        """Expected end of object here
          |{"name":"Mike","age":35
          |-----------------------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Person](js) should have message msg
    }
    it("Missing : on hint scan") {
      val js =
        """{"name","_hint":"co.blocke.scalajack.json.misc.Dog","notneeded":[[1,2],[3,4]],"kind":15}"""
      val msg =
        """Expected ':' here
          |{"name","_hint":"co.blocke.scalajack.json.misc.Dog","notneeded":[[1,2],[3,4]]...
          |-------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Pet](js) should have message msg
    }
    it("Unknown character on hint scan") {
      val js =
        """{"name":"Fido"Z,"_hint":"co.blocke.scalajack.json.misc.Dog","notneeded":[[1,2],[3,4]],"kind":15}"""
      val msg =
        """Unexpected character found
          |{"name":"Fido"Z,"_hint":"co.blocke.scalajack.json.misc.Dog","notneeded":[[1,2...
          |--------------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Pet](js) should have message msg
    }
    it("Missing : while resolving type members") {
      val js =
        """{"kind""co.blocke.scalajack.json.misc.Dog","payload":{"name":"Fido","kind":15}}"""
      val msg =
        """Expected ':' here
            |{"kind""co.blocke.scalajack.json.misc.Dog","payload":{"name":"Fido","kind":15}}
            |-------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[PetHolder[Pet]](js) should have message msg
    }
    it("Unknown character on type member resolution") {
      val js =
        """{"kind":"co.blocke.scalajack.json.misc.Dog"Z,"payload":{"name":"Fido","kind":15}}"""
      val msg =
        """Unexpected character found
          |{"kind":"co.blocke.scalajack.json.misc.Dog"Z,"payload":{"name":"Fido","kind":...
          |-------------------------------------------^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[PetHolder[Pet]](js) should have message msg
    }
    it("Long json error") {
      val js =
        """["a""In the dark night when the wolves roam wild did the little rabbit dispair of life itself."]"""
      val msg =
        """Expected comma here
                  |["a""In the dark night when the wolves roam wild did the little rabbit dispai...
                  |----^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[List[String]](js) should have message msg
    }
    it("ScalaJack") {
      val sj = ScalaJack.apply(json.JsonFlavor())
      sj.read[Int]("15") should be(15)
    }
    it("StringBuilder") {
      val s = StringBuilder()
      s += "Greg"
      s.clear()
      s.result() should be("")
    }
    it("End of TA chain") {
      the[IllegalArgumentException] thrownBy model.DefaultTypeAdapterFactory
        .typeAdapterOf[Any](null)(sj.taCache, TypeTags.of(typeOf[Any])) should have message """Unable to find a type adapter for Any (may be abstract or a dependency of an abstract class)"""
    }
    it("Object reading (json)") {
      val js = """{5:5}"""
      val msg =
        """Expected a String here
          |{5:5}
          |-^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Bogus](js) should have message msg
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
    val msg = """Expected start of list here
                |"Fred"
                |^""".stripMargin
    the[ScalaJackError] thrownBy sj.read[List[Int]](jsNotAnArray) should have message msg
    assertResult(null) { sj.read[Map[String, Int]](jsNull) }
    val msg2 = """Expected start of object here
                 |"Fred"
                 |^""".stripMargin
    the[ScalaJackError] thrownBy sj.read[Map[String, Int]](jsNotAnArray) should have message msg2
    val badNumber = "12.34.56"
    the[java.lang.NumberFormatException] thrownBy sj
      .read[scala.math.BigDecimal](badNumber) should have message "Character array contains more than one decimal point."
    the[java.lang.NumberFormatException] thrownBy sj.read[scala.math.BigInt](
      badNumber
    ) should have message "For input string: \"12.34.56\""
    assertResult(null) { sj.read[Bogus](jsNull) }
    val msg5 = """Expected start of object here
                 |"Fred"
                 |^""".stripMargin
    the[ScalaJackError] thrownBy sj.read[Bogus](jsNotAnArray) should have message msg5
    val strSlash = "\"This\\\\that\""
    assertResult("""This\that""") { sj.read[String](strSlash) }
    val js = """{"_hint":33,"thing":"hah"}"""
    val msg6 =
      """Expected a String here
        |{"_hint":33,"thing":"hah"}
        |---------^""".stripMargin
    the[ScalaJackError] thrownBy sj.read[TypeTrait](js) should have message msg6
  }
  describe("TypeAdapters") {
    it("Any") {
      val js = """[1,2,3]"""
      assertResult(List(1, 2, 3)) {
        sj.read[Any](js)
      }
      val js2 =
        """{"_hint":"co.blocke.scalajack.json.misc.SimpleHasDefaults","name":"Fred"}"""
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

      sj.render[Map[Any, Int]](Map(Map("a" -> 3) -> 5)) should be(
        """{"{\"a\":3}":5}"""
      )
      sj.render[Map[Any, Int]](Map(Map(Some("a") -> 3) -> 5)) should be(
        """{"{\"a\":3}":5}"""
      )
      sj.render[Map[Any, Int]](Map(Map(Some(List(1, 2, 3)) -> 3) -> 5)) should be(
        """{"{\"[1,2,3]\":3}":5}"""
      )
      sj.render[Map[Any, Int]](Map(Map(Map(1 -> 2) -> 3) -> 5)) should be(
        """{"{\"{\\\"1\\\":2}\":3}":5}"""
      )
      sj.render[Map[Any, Any]](Map(Map(None -> 3) -> None)) should be("""{}""")
      sj.render[Map[Int, Any]](Map(1 -> Some(3), 2 -> None)) should be(
        """{"1":3}"""
      )

      sj.read[Any]("""{"_hint":"bogus","a":3,"b":2}""") should be(
        Map("_hint" -> "bogus", "a" -> 3, "b" -> 2)
      )
    }
    it("Fallback") {
      val sjf = ScalaJack().parseOrElse(typeOf[Falling[Int]] -> typeOf[String])
      assertResult(true) {
        sjf.read[Falling[Int]]("""{"foo":"scala.Int", "id":5}""") == Falling(5)
      }
      sjf.read[Falling[Int]]("\"wow\"") should be("wow")
    }
    it("Tuples") {
      val jsNull = "null"
      assertResult(null) { sj.read[(Int, Boolean)](jsNull) }
    }
    it("Options") {
      val js = """{"num":11}"""
      assertResult(Bogus(11, None, Some((5, true)))) {
        sj.read[Bogus](js)
      }
    }
    it("Map") {
      val m: Map[Any, Any] = sj.read[Map[Any, Any]]("""{"1":"5"}""")
      m should be(Map(1 -> "5"))

      val m2: Map[Any, Any] = Map(1 -> 3)
      sj.render(m2) should equal("""{"1":3}""")
    }
    /* TODO:  Is this needed?
    it("Extra chars in JSON") {
      val js = """[1,2,3]]"""
      sj.read[List[Int]](js)
      //      val msg = """[$]: Extra input after read
      //                  |[1,2,3]]
      //                  |-------^""".stripMargin
      //      the[ScalaJackError] thrownBy sj.read[List[Int]](js) should have message msg
    }
     */
    it("Classes") {
      val msg = """Class Bogus missing required fields: num
                  |{}
                  |-^""".stripMargin
      the[ScalaJackError] thrownBy sj.read[Bogus]("{}") should have message msg
    }
    it("Case class defaults and Option") {
      val js = """{"num":5}"""
      val inst = sj.read[Bogus](js)
      inst should be(Bogus(5, None, Some((5, true))))
    }
    it("Traits") {
      val js = "15"
      the[ScalaJackError] thrownBy sj.read[Pet](js) should have message
        """Expected start of object here
          |15
          |^""".stripMargin
    }
    it("Type") {
      pending
      /*  TODO: Do we really need a TypeTypeAdapter???
      val t = sj.read[Type]("\"scala.collection.immutable.List\"")
      sj.render(t) should be("\"scala.collection.immutable.List\"")
     */
    }
  }
}
