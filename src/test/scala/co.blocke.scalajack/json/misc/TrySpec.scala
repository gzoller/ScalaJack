package co.blocke.scalajack
package json
package misc

import ScalaJack.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import java.util.Optional
import TestUtil.*

class TrySpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:          Try Tests          :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Try must work (Success)") {
        val inst = TryHolder(Success(15))
        val sj = sjCodecOf[TryHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":15}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Try of Option (non-None) must work (Success)") {
        val inst = TryHolder[Option[Int]](Success(Some(15)))
        val sj = sjCodecOf[TryHolder[Option[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":15}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Try of Option (None) must work (Success)") {
        val inst = TryHolder[Option[Int]](Success(None))
        val sj = sjCodecOf[TryHolder[Option[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Try of Optional (non-None) must work (Success)") {
        val inst = TryHolder[Optional[Int]](Success(Optional.of(15)))
        val sj = sjCodecOf[TryHolder[Optional[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":15}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Try of Optional (None) must work (Success)") {
        val inst = TryHolder[Optional[Int]](Success(Optional.empty))
        val sj = sjCodecOf[TryHolder[Optional[Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Try of Either must work (Success)") {
        val inst = TryHolder[Either[Boolean, Int]](Success(Right(5)))
        val sj = sjCodecOf[TryHolder[Either[Boolean, Int]]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Try w/policy AS_NULL must work (Failure)") {
        val inst = TryHolder[Int](Failure(new Exception("boom")))
        val sj = sjCodecOf[TryHolder[Int]](SJConfig.withTryFailureHandling(TryPolicy.AS_NULL))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (TryHolder[Int](null))
      }
      it("Try w/policy ERR_MSG_STRING must work (Failure)") {
        val inst = TryHolder[Int](Failure(new Exception("boom")))
        val sj = sjCodecOf[TryHolder[Int]](SJConfig.withTryFailureHandling(TryPolicy.ERR_MSG_STRING))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":"Try Failure with msg: boom"}""")
        val msg = """Unsuccessful attempt to read Try type with failure: Non-numeric character found when integer value expected at position 5 at position [5]
          |{"a":"Try Failure with msg: boom"}
          |-----^""".stripMargin
        val err = intercept[JsonParseError](sj.fromJson(js))
        err.show shouldEqual msg
      }
      it("Try w/policy ATHROW_EXCEPTIONS_NULL must work (Failure)") {
        val inst = TryHolder[Int](Failure(new Exception("boom")))
        val caught =
          intercept[java.lang.Exception] {
            sjCodecOf[TryHolder[Int]](SJConfig.withTryFailureHandling(TryPolicy.THROW_EXCEPTION)).toJson(inst)
          }
        assert(caught.getMessage == "boom")
      }
      it("Seq and Tuple of Try must work for AS_NULL (Failure)") {
        val inst = TryHolder2[Int](List(Success(1), Failure(new Exception("boom")), Success(3)), (Failure(new Exception("boom")), Success(0)))
        val sj = sjCodecOf[TryHolder2[Int]](SJConfig.withTryFailureHandling(TryPolicy.AS_NULL))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":[1,null,3],"b":[null,0]}""")
        sj.fromJson(js) shouldEqual (TryHolder2[Int](List(Success(1), null, Success(3)), (null, Success(0))))
      }
    }
  }
