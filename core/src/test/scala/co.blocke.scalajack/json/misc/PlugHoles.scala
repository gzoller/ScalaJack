package co.blocke.scalajack
package json.test.custom

import co.blocke.scalajack.TestUtil._
import co.blocke.scalajack.util.Path
import org.scalatest.{ FunSpec, Matchers }

case class Bogus(num: Int)

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
      assert(expectUnexpected(() => sj.read[Map[String, Any]](js), Path.Tokenizing, List("s", "6")))
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
  }
  it("JsonReader") {
    val jsNull = "null"
    assertResult(null) { sj.read[List[Int]](jsNull) }
    val jsNotAnArray = """"Fred""""
    assert(expectUnexpected(() => sj.read[List[Int]](jsNotAnArray), Path.Root, List("String")))
    assertResult(null) { sj.read[Map[String, Int]](jsNull) }
    assert(expectUnexpected(() => sj.read[Map[String, Int]](jsNotAnArray), Path.Root, List("String")))
    val badNumber = "12.34.56"
    assert(expectMalformed[NumberFormatException](() => sj.read[BigDecimal](badNumber), Path.Root, List.empty[String]))
    assert(expectMalformed[NumberFormatException](() => sj.read[BigInt](badNumber), Path.Root, List.empty[String]))
    assertResult(null) { sj.read[Bogus](jsNull) }
    assert(expectUnexpected(() => sj.read[Bogus](jsNotAnArray), Path.Root, List("String")))
    val strSlash = "\"This\\\\that\""
    assertResult("""This\that""") { sj.read[String](strSlash) }
  }
}