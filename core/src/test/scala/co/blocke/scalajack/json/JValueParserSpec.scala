package co.blocke.scalajack.json

import org.json4s.JsonAST.{ JInt, JLong }
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class JValueParserSpec extends FunSpec {

  describe("Numbers") {
    it("should parse a small integer") {
      JValueParser.parse("""1234""") should be(JLong(1234))
    }
    it("should parse Long.MaxValue as a JLong") {
      JValueParser.parse(Long.MaxValue.toString) should be(JLong(Long.MaxValue))
    }
    it("should parse (Long.MaxValue + 1) as a JInt") {
      JValueParser.parse((BigInt(Long.MaxValue) + 1).toString) should be(JInt(BigInt(Long.MaxValue) + 1))
    }
  }

}
