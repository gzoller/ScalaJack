package co.blocke.scalajack
package json
package collections

import ScalaJack.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

class TupleSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:         Tuple Tests         :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Tuple is null must work") {
        val inst = TupleHolder[Int, String, Boolean](null)
        val sj = sjCodecOf[TupleHolder[Int, String, Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual inst
      }
      it("Tuple of simple types must work") {
        val inst = TupleHolder[Int, String, Boolean]((15, "wow", true))
        val sj = sjCodecOf[TupleHolder[Int, String, Boolean]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[15,"wow",true]}""")
        sj.fromJson(js) shouldEqual inst
      }
      it("Tuple of collecitons (including another tuple) must work") {
        val inst = TupleHolder[Seq[Int], Map[String, Long], (Double, Char, Boolean)]((List(1, 2), Map("a" -> 3L, "b" -> 4L), (1.23d, 'X', true)))
        val sj = sjCodecOf[TupleHolder[Seq[Int], Map[String, Long], (Double, Char, Boolean)]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[[1,2],{"a":3,"b":4},[1.23,"X",true]]}""")
        sj.fromJson(js) shouldEqual inst
      }
      it("Tuple of one element must work") {
        val inst = TupleOneHolder[Int](Tuple1(15))
        val sj = sjCodecOf[TupleOneHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[15]}""")
        sj.fromJson(js) shouldEqual inst
      }
    }

    describe(colorString("--- Negative Tests ---")) {
      it("Wrong number of elements in a tuple") {
        val js = """{"a":[15,"wow",true,12.34]}"""
        val msg =
          """Expected ']' here at position [19]
          |{"a":[15,"wow",true,12.34]}
          |-------------------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[TupleHolder[Int, String, Boolean]].fromJson(js))
        ex.show shouldEqual msg
      }
      it("Wrong type of elements in tuple") {
        val js = """{"a":[15,true,true]}"""
        val msg =
          """Expected a String value but got 't' at position [9]
          |{"a":[15,true,true]}
          |---------^""".stripMargin
        val ex = intercept[JsonParseError](sjCodecOf[TupleHolder[Int, String, Boolean]].fromJson(js))
        ex.show shouldEqual msg
      }
    }
  }
