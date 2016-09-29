package co.blocke.scalajack
package test
package tokenizer

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import java.io._
import java.lang.IllegalArgumentException

import json._
import TokenType._

class TokenizerSpec extends FunSpec {

  val validTC = new Tokenizer()

  describe("---------------------\n:  Tokenizer Tests  :\n---------------------") {
    it("Must tokenize good json") {
      val js = """{"a":1,"b":true,"c":[1,2],"d":{"one":1},"e":null}"""
      assertResult(List(BeginObject, String, Number, String, True, String, BeginArray, Number, Number, EndArray, String, BeginObject, String, Number, EndObject, String, Null, EndObject, End)) {
        validTC.tokenize(js.toCharArray, 0, js.length).getTokens()
      }
    }
    it("Must tokenize empty strings for map key values") {
      val js = """{"a":1,"":2,"c":3}"""
      assertResult(List(BeginObject, String, Number, String, Number, String, Number, EndObject, End)) {
        validTC.tokenize(js.toCharArray, 0, js.length).getTokens()
      }
    }
    it("Must catch errors in json - open top-level object") {
      val js = """{"a":1,"b":true,"c":[1,2],"d":{"one":1}"""
      val msg = """Unterminated object
        |{"a":1,"b":true,"c":[1,2],"d":{"one":1}
        |---------------------------------------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
    it("Must catch errors in json - missing comma") {
      val js = """{"a":1"b":true,"c":[1,2],"d":{"one":1}}"""
      val msg = """Character out of place. String not expected here.
        |{"a":1"b":true,"c":[1,2],"d":{"one":1}}
        |------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
    it("Must catch errors in json - missing colon") {
      val js = """{"a":1,"b"true,"c":[1,2],"d":{"one":1}}"""
      val msg = """Character out of place. Un-quoted literal not expected here.  (Possile un-terminated string earlier in your JSON.)
        |{"a":1,"b"true,"c":[1,2],"d":{"one":1}}
        |----------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
    it("Must catch errors in json - unknown literal") {
      val js = """{"a":1,"b":bogus,"c":[1,2],"d":{"one":1}}"""
      assertResult(List(BeginObject, String, Number, String, UnknownLiteralName, String, BeginArray, Number, Number, EndArray, String, BeginObject, String, Number, EndObject, EndObject, End)) {
        validTC.tokenize(js.toCharArray, 0, js.length).getTokens()
      }
    }
    it("Must catch errors in json - non-string object key") {
      val js = """{"a":1,{"fred":1,"wilma":2}:true,"c":[1,2],"d":{"one":1}}"""
      val msg = """Character out of place. '{' not expected here.
        |{"a":1,{"fred":1,"wilma":2}:true,"c":[1,2],"d":{"one":1}}
        |-------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
    it("Must catch errors in json - missing quotes for object key") {
      val js = """{"a":1,"b":true,c:[1,2],"d":{"one":1}}"""
      val msg = """Character out of place. Un-quoted literal not expected here.  (Possile un-terminated string earlier in your JSON.)
        |{"a":1,"b":true,c:[1,2],"d":{"one":1}}
        |----------------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
    it("Must catch errors in json - open quotes for object key") {
      val js = """{"a":1,"b":true,"c:[1,2],"d":{"one":1}}"""
      val msg = """Character out of place. Un-quoted literal not expected here.  (Possile un-terminated string earlier in your JSON.)
        |{"a":1,"b":true,"c:[1,2],"d":{"one":1}}
        |--------------------------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
    it("Must catch errors in json - open list") {
      val js = """{"a":1,"b":true,"c":[1,2,"d":{"one":1}}"""
      val msg = """Character out of place. ':' not expected here.
        |{"a":1,"b":true,"c":[1,2,"d":{"one":1}}
        |----------------------------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
    it("Must catch errors in json - open object in list") {
      val js = """{"a":1,"b":true,"c":[{"fred":1,"wilma":2},{"barney":3,"betty":4],"d":{"one":1}}"""
      val msg = """Character out of place. ']' not expected here.
        |ue,"c":[{"fred":1,"wilma":2},{"barney":3,"betty":4],"d":{"one":1}}
        |--------------------------------------------------^""".stripMargin
      the[java.lang.IllegalArgumentException] thrownBy validTC.tokenize(js.toCharArray, 0, js.length) should have message msg
    }
  }
  /* For later reference if we ever develop a streaming adapter...
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
      */
}