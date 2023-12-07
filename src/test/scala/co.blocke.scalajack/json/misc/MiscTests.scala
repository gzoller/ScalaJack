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

class MiscSpec() extends AnyFunSpec with JsonMatchers:
  opaque type Count = Int
  opaque type CountX = Option[Int]
  type CountY = String
  type CountZ = Option[String]

  describe(colorString("-------------------------------\n:          Misc Tests         :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("String escaping must work") {
        val inst = StringHolder("""This is a "strange" test\non another level.""")
        val js = sj[StringHolder].toJson(inst)
        js should matchJson("""{"a":"This is a \"strange\" test\\non another level."}""")
      }
      it("String without escaping must work (bad JSON, but proves escape can be turned off)") {
        val inst = StringHolder("""This is a "strange" test\non another level.""")
        val js = sj[StringHolder](JsonConfig.withEscapeStrings(false)).toJson(inst)
        js should equal("""{"a":"This is a "strange" test\non another level."}""")
      }
    }
  }
