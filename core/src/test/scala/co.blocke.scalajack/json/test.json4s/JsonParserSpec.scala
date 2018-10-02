package co.blocke.scalajack
package json
package test.json4s

import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JDouble, JInt, JLong, JNothing, JNull, JObject, JString }
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class JsonParserSpec extends FunSpec {

  val sj = ScalaJack()

  describe("Numbers") {
    it("should parse a simple integer") {
      sj.parse("""1234""") should be(JLong(1234))
      sj.parse(Long.MaxValue.toString) should be(JLong(Long.MaxValue))
    }
    it("should parse a negative integer") {
      sj.parse("""-1234""") should be(JLong(-1234))
    }
    it("should parse a double") {
      sj.parse("123.456D") should be(JDecimal(123.456))
    }
    it("should parse a null") {
      sj.parse("") should be(JNull)
    }
    it("should parse a string") {
      sj.parse("\"blather\"") should be(JString("blather"))
    }
    it("should parse a array") {
      sj.parse("[1,2,3]") should be(JArray(List(JLong(1), JLong(2), JLong(3))))
    }
    it("should parse an object") {
      sj.parse("""{"a":3,"b":4}""") should be(JObject(List(("a", JLong(3)), ("b", JLong(4)))))
    }
    it("should parse a boolean") {
      sj.parse("true") should be(JBool(true))
    }
  }

}