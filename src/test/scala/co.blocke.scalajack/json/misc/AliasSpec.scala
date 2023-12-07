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
        val js = sj[AliasHolder[Count]].toJson(inst)
        js should matchJson("""{"a":5,"b":[1,2,3],"c":{"1":"wow"},"d":{"wow":2}}""")
      }
      it("Type aliases (opaque types) must be dereferenced (with Option)") {
        val inst = AliasHolder2[CountX](Some(5), List(Some(1), None, Some(3)), Map("wow" -> None))
        val js = sj[AliasHolder2[CountX]].toJson(inst)
        js should matchJson("""{"a":5,"b":[1,3],"c":{}}""")
      }
      it("Type aliases (non-opaque types) must be dereferenced") {
        val inst = AliasHolder[CountY]("q", List("r", "s", "t"), Map("u" -> "wow"), Map("wow" -> "v"))
        val js = sj[AliasHolder[CountY]].toJson(inst)
        js should matchJson("""{"a":"q","b":["r","s","t"],"c":{"u":"wow"},"d":{"wow":"v"}}""")
      }
      it("Type aliases (non-opaque types) must be dereferenced (with Option)") {
        val inst = AliasHolder2[CountZ](Some("q"), List(Some("r"), None, Some("t")), Map("wow" -> None))
        val js = sj[AliasHolder2[CountZ]].toJson(inst)
        js should matchJson("""{"a":"q","b":["r","t"],"c":{}}""")
      }
    }
  }
