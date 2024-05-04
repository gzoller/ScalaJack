package co.blocke.scalajack
package json
package misc

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import TestUtil.*

import java.util.UUID

class AliasSpec() extends AnyFunSpec with JsonMatchers:
  opaque type Count = Int
  opaque type CountX = Option[Int]
  type CountY = String
  type CountZ = Option[String]

  describe(colorString("-------------------------------\n:         Alias Tests         :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Type aliases (opaque types) must be dereferenced") {
        val inst = AliasHolder[Count](5, List(1, 2, 3), Map(1 -> "wow"), Map("wow" -> 2))
        val sj = sjCodecOf[AliasHolder[Count]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5,"b":[1,2,3],"c":{"1":"wow"},"d":{"wow":2}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Type aliases (opaque types) must be dereferenced (with Option)") {
        val inst = AliasHolder2[CountX](Some(5), List(Some(1), None, Some(3)), Map("wow" -> None))
        val sj = sjCodecOf[AliasHolder2[CountX]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5,"b":[1,3],"c":{}}""")
        sj.fromJson(js) shouldEqual (AliasHolder2[CountX](Some(5), List(Some(1), Some(3)), Map.empty[String, CountX]))
      }
      it("Type aliases (opaque types) must be dereferenced (with Option, noneAsNull)") {
        val inst = AliasHolder2[CountX](Some(5), List(Some(1), None, Some(3)), Map("wow" -> None))
        val sj = sjCodecOf[AliasHolder2[CountX]](SJConfig.withNoneAsNull())
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5,"b":[1,null,3],"c":{"wow":null}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Type aliases (non-opaque types) must be dereferenced") {
        val inst = AliasHolder[CountY]("q", List("r", "s", "t"), Map("u" -> "wow"), Map("wow" -> "v"))
        val sj = sjCodecOf[AliasHolder[CountY]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":"q","b":["r","s","t"],"c":{"u":"wow"},"d":{"wow":"v"}}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Type aliases (non-opaque types) must be dereferenced (with Option)") {
        val inst = AliasHolder2[CountZ](Some("q"), List(Some("r"), None, Some("t")), Map("wow" -> None))
        val sj = sjCodecOf[AliasHolder2[CountZ]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":"q","b":["r","t"],"c":{}}""")
        sj.fromJson(js) shouldEqual (AliasHolder2[CountZ](Some("q"), List(Some("r"), Some("t")), Map.empty[String, CountZ]))
      }
    }
  }
