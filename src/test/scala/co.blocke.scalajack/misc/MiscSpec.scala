package co.blocke.scalajack
package json
package misc

import ScalaJack.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import Raw.*
import Raw.given

import scala.util.*
import TestUtil.*
import reading.JsonSource
import writing.JsonOutput

import scala.util.matching.Regex

class MiscSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:          Misc Tests         :\n-------------------------------", Console.YELLOW)) {
    it("String escaping must work (proves escape can be turned off)") {
      val inst = StringHolder("""This is a "strange" test
on another level.""")
      val sj1 = sjCodecOf[StringHolder]
      val js1 = sj1.toJson(inst)
      val sj2 = sjCodecOf[StringHolder](SJConfig.suppressEscapedStrings)
      val js2 = sj2.toJson(inst)
      js1 should equal("""{"a":"This is a \"strange\" test\non another level."}""")
      js2 should equal("""{"a":"This is a "strange" test
on another level."}""")
      sj1.fromJson(js1) shouldEqual (inst)
      val msg =
        """Expected ',' or '}' but found 's' at position [17]
            |{"a":"This is a "strange" test~on another level."}
            |-----------------^""".stripMargin
      val ex = intercept[JsonParseError](sj2.fromJson(js2))
      ex.show shouldEqual msg
    }
    it("NeoType integration must work") {
      val inst = Validated(NonEmptyString("Mike"), XList(List("x", "y", "z")), List(EmptyString(""), EmptyString(""), EmptyString("")))
      val sj = sjCodecOf[Validated]
      val js = sj.toJson(inst)
      js should equal("""{"name":"Mike","xspot":["x","y","z"],"nada":["","",""]}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("NeoType validation must work (test failure)") {
      val sj = sjCodecOf[Validated]
      val js = """{"name":"","xspot":["x","y","z"],"nada":["","",""]}"""
      val msg =
        """NeoType validation for NonEmptyString failed at position [9]
            |{"name":"","xspot":["x","y","z"],"nada":["","",""]}
            |---------^""".stripMargin
      val ex = intercept[JsonParseError](sj.fromJson(js))
      ex.show shouldEqual msg
    }
    it("Any type must work (non-exhaustive test)") {
      val inst = AnyHolder(
        Some(List(1, 2, 3)),
        None,
        TryHolder(Success(-5)),
        Success(99),
        Failure(new Exception("oops")),
        Map("a" -> 1, "b" -> 2),
        Right(3),
        Left("nope"),
        (Some('a'), None, Some('b'))
      )
      val sj = sjCodecOf[AnyHolder]
      val js = sj.toJson(inst)
      js should equal("""{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]}""")
      sj.fromJson(js) shouldEqual (AnyHolder(List(1, 2, 3), null, Map("a" -> -5), 99, null, Map("a" -> 1, "b" -> 2), 3, "nope", List("a", null, "b")))
    }
    it("Any type must work (none as null)") {
      val inst = AnyHolder(
        Some(List(1, 2, 3)),
        None,
        TryHolder(Success(-5)),
        Success(99),
        Failure(new Exception("oops")),
        Map("a" -> 1, "b" -> 2),
        Right(3),
        Left("nope"),
        (Some('a'), None, Some('b'))
      )
      val sj = sjCodecOf[AnyHolder](
        SJConfig.withNoneAsNull
          .withEitherLeftHandling(EitherLeftPolicy.ERR_MSG_STRING)
          .withTryFailureHandling(TryPolicy.ERR_MSG_STRING)
      )
      val js = sj.toJson(inst)
      js should equal("""{"maybe":[1,2,3],"maybeNot":null,"itried":{"a":-5},"itried2":99,"ifailed":"Try Failure with msg: oops","anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"Left Error: nope","bunch":["a",null,"b"]}""")
      sj.fromJson(js) shouldEqual (AnyHolder(List(1, 2, 3), null, Map("a" -> -5), 99, "Try Failure with msg: oops", Map("a" -> 1, "b" -> 2), 3, "Left Error: nope", List("a", null, "b")))
    }
    it("User-supplied 'given' JsonCodec overrides must work") {
      // case class PhoneNumber(countryCode: Int, areaCode: Int, prefix: Int, rest: Int)
      given JsonCodec[PhoneNumber] = new JsonCodec[PhoneNumber] {
        val phoneRegex: Regex = raw"\+(\d) \((\d{3})\) (\d{3})-(\d{4})".r
        def encodeValue(in: PhoneNumber, out: JsonOutput): Unit = out.value(s"+${in.countryCode} (${in.areaCode}) ${in.prefix}-${in.rest}")
        def decodeValue(in: JsonSource): PhoneNumber =
          in.expectString() match {
            case phoneRegex(country, area, prefix, line) => PhoneNumber(country.toInt, area.toInt, prefix.toInt, line.toInt)
            case _                                       => throw new Exception("boom")
          }
      }
      val sj = sjCodecOf[PhoneHolder]
      val inst = PhoneHolder("Dude", PhoneNumber(1, 123, 456, 7890))
      val js = sj.toJson(inst)
      js should equal("""{"me":"Dude","phone":"+1 (123) 456-7890"}""")
      sj.fromJson(js) shouldEqual inst
    }
    it("JsonRaw must work") {
      val sj = sjCodecOf[RawHolder]
      val payload: JsonRaw = Raw("""{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]}""")
      val inst = RawHolder("aaa", 3, payload, List(payload, payload))
      val js = sj.toJson(inst)
      js should equal(
        """{"id":"aaa","count":3,"oneBlob":{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]},"items":[{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]},{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]}]}"""
      )
      sj.fromJson(js) shouldEqual inst
    }
    it("ScalaJackSyntax must work") {
      import ScalaJackSyntax.*
      given ScalaJack[RawHolder] = ScalaJack.sjCodecOf[RawHolder]
      val payload: JsonRaw = Raw("""{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]}""")
      val inst = RawHolder("aaa", 3, payload, List(payload, payload))
      val js = inst.toJson
      js should equal(
        """{"id":"aaa","count":3,"oneBlob":{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]},"items":[{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]},{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"ifailed":null,"anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"nope","bunch":["a",null,"b"]}]}"""
      )
      js.fromJson[RawHolder] shouldEqual inst
    }
  }
