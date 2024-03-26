package co.blocke.scalajack
package json
package collections

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

import java.util.UUID

class TupleSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:         Tuple Tests         :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Tuple is null must work") {
        val inst = TupleHolder[Int, String, Boolean](null)
        val js = sj[TupleHolder[Int, String, Boolean]].toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Tuple of simple types must work") {
        val inst = TupleHolder[Int, String, Boolean]((15, "wow", true))
        val js = sj[TupleHolder[Int, String, Boolean]].toJson(inst)
        js should matchJson("""{"a":[15,"wow",true]}""")
      }
      it("Tuple of collecitons (including another tuple) must work") {
        val inst = TupleHolder[Seq[Int], Map[String, Long], (Double, Char, Boolean)]((List(1, 2), Map("a" -> 3L, "b" -> 4L), (1.23d, 'X', true)))
        val js = sj[TupleHolder[Seq[Int], Map[String, Long], (Double, Char, Boolean)]].toJson(inst)
        js should matchJson("""{"a":[[1,2],{"a":3,"b":4},[1.23,"X",true]]}""")
      }
    }
  }
