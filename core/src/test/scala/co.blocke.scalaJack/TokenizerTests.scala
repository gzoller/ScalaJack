package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import java.io._

import json._

class TokenizerSpec extends FunSpec {

  // val fastT = FastTokenizer(128)
  // val validTC = ValidTokenizer() // canonical JSON
  // val validTNC = ValidTokenizer(false) // non-canonical JSON (non-string object keys)

  describe("=========================\n| -- Tokenizer Tests -- |\n=========================") {
    describe("::: Validating Tokenizer (canonical)") {
      it("Must tokenize good json") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[1,2],"d":{"one":1},"e":null}"""
        // val z = validTC.tokenize(js.toCharArray)
        // z.tokPos.slice(0, z.tokCount) should equal(Array(0, 2, 5, 8, 11, 17, 20, 21, 23, 24, 27, 30, 32, 37, 38, 41, 44, 48))
        // z.tokLen.slice(0, z.tokCount) should equal(Array(1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 4, 1))
        // z.tokType.slice(0, z.tokCount) should equal(Array(JSobjStart, JSstringObjKey, JSnumber, JSstringObjKey, JStrue, JSstringObjKey, JSlistStart, JSnumberInList, JSnumberInList, JSlistEnd, JSstringObjKey, JSobjStart, JSstringObjKey, JSnumber, JSobjEnd, JSstringObjKey, JSnull, JSobjEnd))
      }
      it("Must catch errors in json - open top-level object") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[1,2],"d":{"one":1}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // val err = thrown.getMessage + " @ " + thrown.pos
        // err should equal("""Incomplete (open) object or list in JSON.  Forgot closing } or ]? @ 39""")
      }
      it("Must catch errors in json - missing comma") {
        (pending)
        // val js = """{"a":1"b":true,"c":[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JScomma JSobjEnd JSobjEndInList but saw JSstringObjKey at position 6""")
      }
      it("Must catch errors in json - missing colon") {
        (pending)
        // val js = """{"a":1,"b"true,"c":[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JSstringObjKey but saw JSstring at position 7""")
      }
      it("Must catch errors in json - invalid field") {
        (pending)
        // val js = """{"a":1,"b":bogus,"c":[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""Character out of place (bad JSON) at position 11""")
      }
      it("Must catch errors in json - non-string object key") {
        (pending)
        // val js = """{"a":1,{"fred":1,"wilma":2}:true,"c":[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JSstringObjKey but saw JSobjStart at position 7""")
      }
      it("Must catch errors in json - missing quotes for object key") {
        (pending)
        // val js = """{"a":1,"b":true,c:[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""Character out of place (bad JSON) at position 16""")
      }
      it("Must catch errors in json - open quotes for object key") {
        (pending)
        // val js = """{"a":1,"b":true,"c:[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""Character out of place (bad JSON) at position 26""")
      }
      it("Must catch errors in json - open list") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[1,2,"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JSobjStart JSlistStart JStrueInList JSfalseInList JSnullInList JSstringInList JSnumberInList but saw JSstringObjKey at position 25""")
      }
      it("Must catch errors in json - open object in list") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[{"fred":1,"wilma":2},{"barney":3,"betty":4],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JSobjStart JSlistStart JStrue JSfalse JSnull JSstring JSnumber but saw JSnumberInList at position 62""")
      }
    }
    describe("::: Validating Tokenizer (non-canonical)") {
      it("Must tokenize good json") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[1,2],"d":{"one":1}}"""
        // val z = validTNC.tokenize(js.toCharArray)
        // z.tokPos.slice(0, z.tokCount) should equal(Array(0, 2, 5, 8, 11, 17, 20, 21, 23, 24, 27, 30, 32, 37, 38, 39))
        // z.tokLen.slice(0, z.tokCount) should equal(Array(1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1))
        // z.tokType.slice(0, z.tokCount) should equal(Array(JSobjStart, JSstringObjKey, JSnumber, JSstringObjKey, JStrue, JSstringObjKey, JSlistStart, JSnumberInList, JSnumberInList, JSlistEnd, JSstringObjKey, JSobjStart, JSstringObjKey, JSnumber, JSobjEnd, JSobjEnd))
      }
      it("Must catch errors in json - open top-level object") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[1,2],"d":{"one":1}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // val err = thrown.getMessage + " @ " + thrown.pos
        // err should equal("""Incomplete (open) object or list in JSON.  Forgot closing } or ]? @ 39""")
      }
      it("Must catch errors in json - missing comma") {
        (pending)
        // val js = """{"a":1"b":true,"c":[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JScomma JSobjEnd JSobjEndInList but saw JSstringObjKey at position 6""")
      }
      it("Must catch errors in json - missing colon") {
        (pending)
        // val js = """{"a":1,"b"true,"c":[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JSstringObjKey but saw JSstring at position 7""")
      }
      it("Must catch errors in json - invalid field") {
        (pending)
        // val js = """{"a":1,"b":bogus,"c":[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""Character out of place (bad JSON) at position 11""")
      }
      it("Must *not* catch errors in json for non-string object key") {
        (pending)
        // val js = """{"a":1,{"fred":1,"wilma":2}:true,"c":[1,2],"d":{"one":1}}"""
        // val z = validTNC.tokenize(js.toCharArray)
        // z.tokPos.slice(0, z.tokCount) should equal(Array(0, 2, 5, 7, 9, 15, 18, 25, 26, 28, 34, 37, 38, 40, 41, 44, 47, 49, 54, 55, 56))
        // z.tokLen.slice(0, z.tokCount) should equal(Array(1, 1, 1, 1, 4, 1, 5, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1))
        // z.tokType.slice(0, z.tokCount) should equal(Array(JSobjStart, JSstringObjKey, JSnumber, JSobjStart, JSstringObjKey, JSnumber, JSstringObjKey, JSnumber, JSobjEndObjKey, JStrue, JSstringObjKey, JSlistStart, JSnumberInList, JSnumberInList, JSlistEnd, JSstringObjKey, JSobjStart, JSstringObjKey, JSnumber, JSobjEnd, JSobjEnd))
      }
      it("Must catch errors in json - missing quotes for object key") {
        (pending)
        // val js = """{"a":1,"b":true,c:[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""Character out of place (bad JSON) at position 16""")
      }
      it("Must catch errors in json - open quotes for object key") {
        (pending)
        // val js = """{"a":1,"b":true,"c:[1,2],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""Character out of place (bad JSON) at position 26""")
      }
      it("Must catch errors in json - open list") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[1,2,"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JSobjStart JSlistStart JStrueInList JSfalseInList JSnullInList JSstringInList JSnumberInList but saw JSstringObjKey at position 25""")
      }
      it("Must catch errors in json - open object in list") {
        (pending)
        // val js = """{"a":1,"b":true,"c":[{"fred":1,"wilma":2},{"barney":3,"betty":4],"d":{"one":1}}"""
        // val thrown = the[JsonParseException] thrownBy validTC.tokenize(js.toCharArray)
        // // Not sure this is right... pos 62 -> ']' (after 4)
        // thrown.getMessage should equal("""JSON parse error.  Expected one of JSobjStart JSlistStart JStrue JSfalse JSnull JSstring JSnumber but saw JSnumberInList at position 62""")
      }
    }
    describe("::: Streaming Adapter Tests") {
      it("Must stream list of objects, managin buffer as it goes. - Small Buffer") {
        (pending)
        /*
        val js = """[{"a":1,"b":true,"c":[1,2],"d":{"one":1}},{"a":1,"b":true,"c":[1,2],"d":{"two":2}},{"a":1,"b":true,"c":[1,2],"d":{"three":3}}]"""
        implicit val tokenizer = fastT
        // Buffer smaller than an object -- impossible
        Try {
          val streamer = StreamingAdapter(new InputStreamReader(new ByteArrayInputStream(js.getBytes("UTF-8"))), 10)
          streamer.nextObject()
          // should throw an exception
        }.toOption == None should be(true)
        */
      }
      it("Must stream list of objects, managin buffer as it goes. - Medium Buffer") {
        (pending)
        /*
        val js = """[{"a":1,"b":true,"c":[1,2],"d":{"one":1}},{"a":1,"b":true,"c":[1,2],"d":{"two":2}},{"a":1,"b":true,"c":[1,2],"d":{"three":3}}]"""
        implicit val tokenizer = fastT
        val streamer = StreamingAdapter(new InputStreamReader(new ByteArrayInputStream(js.getBytes("UTF-8"))), 50)
        streamer.nextObject().isDefined should be(true)
        streamer.nextObject().isDefined should be(true)
        streamer.nextObject().isDefined should be(true)
        streamer.nextObject().isDefined should be(false)
        */
      }
      it("Must stream list of objects, managin buffer as it goes. - Large Buffer") {
        (pending)
        /*
        val js = """[{"a":1,"b":true,"c":[1,2],"d":{"one":1}},{"a":1,"b":true,"c":[1,2],"d":{"two":2}},{"a":1,"b":true,"c":[1,2],"d":{"three":3}}]"""
        implicit val tokenizer = fastT
        val streamer = StreamingAdapter(new InputStreamReader(new ByteArrayInputStream(js.getBytes("UTF-8"))))
        streamer.nextObject().isDefined should be(true)
        streamer.nextObject().isDefined should be(true)
        streamer.nextObject().isDefined should be(true)
        streamer.nextObject().isDefined should be(false)
        */
      }
    }
  }
}
