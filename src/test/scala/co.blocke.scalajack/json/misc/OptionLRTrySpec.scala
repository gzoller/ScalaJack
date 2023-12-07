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

class OptionLRTrySpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:         Option Tests        :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Non-empty Options must work") {
        val inst = OptionHolder[Int](
          Some(5), // straight Option
          (Some(1), "ok"), // tuple w/Option
          List(Some(1), None, Some(3)), // Seq of Option
          Map(1 -> Some(2), 3 -> Some(4)), // Map of Option
          Some(99), // Union of Option (R)
          Some(100), // Union of Option (L)
          Some(Some(0)), // Nested Option
          Some(Person("BoB", 34)), // Option of class
          Right(Some(15)), // Either of Option (R)
          Left(Some(-3)) // Either of Option (L)
        )
        val js = sj[OptionHolder[Int]].toJson(inst)
        js should matchJson("""{"a":5,"b":[1,"ok"],"c":[1,3],"d":{"1":2,"3":4},"e":99,"f":100,"g":0,"h":{"name":"BoB","age":34},"i":15}""")
      }
      it("Empty Options must work (default)") {
        val inst = OptionHolder[Int](
          None, // straight Option
          (None, "ok"), // tuple w/Option
          List(None, None, None), // Seq of Option
          Map(1 -> None, 3 -> None), // Map of Option
          None, // Union of Option (R)
          None, // Union of Option (L)
          Some(None), // Nested Option
          None, // Option of class
          Right(None), // Either of Option (R)
          Left(None) // Either of Option (L)
        )
        val js = sj[OptionHolder[Int]].toJson(inst)
        js should matchJson("""{"b":[null,"ok"],"c":[],"d":{}}""")
      }
      it("Empty Options must work (config noneAsNull = true)") {
        val inst = OptionHolder[Int](
          None, // straight Option
          (None, "ok"), // tuple w/Option
          List(None, None, None), // Seq of Option
          Map(1 -> None, 3 -> None), // Map of Option
          None, // Union of Option (R)
          None, // Union of Option (L)
          Some(None), // Nested Option
          None, // Option of class
          Right(None), // Either of Option (R)
          Left(None) // Either of Option (L)
        )
        val js = sj[OptionHolder[Int]](
          JsonConfig.withNoneAsNull(true).withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)
        ).toJson(inst)
        js should matchJson("""{"a":null,"b":[null,"ok"],"c":[null,null,null],"d":{"1":null,"3":null},"e":null,"f":null,"g":null,"h":null,"i":null,"j":null}""")
      }
    }
  }

  describe(colorString("-------------------------------\n:         Either Tests        :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Complex Either/Option must work (non-None)") {
        val inst = ComplexEither[Int](Some(Right(Some(3))))
        val js = sj[ComplexEither[Int]].toJson(inst)
        js should matchJson("""{"a":3}""")
      }
      it("Complex Either/Option must work (None no-write default)") {
        val inst = ComplexEither[Int](Some(Right(None)))
        val js = sj[ComplexEither[Int]].toJson(inst)
        js should matchJson("""{}""")
      }
      it("Complex Either/Option must work (NoneAsNull)") {
        val inst = ComplexEither[Int](Some(Right(None)))
        val js = sj[ComplexEither[Int]](JsonConfig.withNoneAsNull(true)).toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Complex Either/Option must work (Left-NO_WRITE)") {
        val inst = ComplexEither[Int](Some(Left("err")))
        val js = sj[ComplexEither[Int]].toJson(inst)
        js should matchJson("""{}""")
      }
      it("Complex Either/Option must work (Left-AS_VALUE)") {
        val inst = ComplexEither[Int](Some(Left("err")))
        val js = sj[ComplexEither[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)).toJson(inst)
        js should matchJson("""{"a":"err"}""")
      }
      it("Either with AS_VALUE left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val js = sj[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE)).toJson(inst)
        js should matchJson("""{"a":5,"b":3}""")
      }
      it("Either with AS_NULL left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val js = sj[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_NULL)).toJson(inst)
        js should matchJson("""{"a":null,"b":3}""")
      }
      it("Either with NO_WRITE left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val js = sj[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.NO_WRITE)).toJson(inst)
        js should matchJson("""{"b":3}""")
      }
      it("Either with ERR_MSG_STRING left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val js = sj[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.ERR_MSG_STRING)).toJson(inst)
        js should matchJson("""{"a":"Left Error: 5","b":3}""")
      }
      it("Either with THROW_EXCEPTION left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val caught =
          intercept[JsonEitherLeftError] {
            sj[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.THROW_EXCEPTION)).toJson(inst)
          }
        assert(caught.getMessage == "Left Error: 5")
      }
    }
  }

  describe(colorString("-------------------------------\n:           LR Tests          :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("LR (union) must work with Option (non-None)") {
        val inst = LRHolder[Option[Int], String](List(Some(5), "x"), ("y", Some(10)))
        val js = sj[LRHolder[Option[Int], String]].toJson(inst)
        js should matchJson("""{"a":[5,"x"],"b":["y",10]}""")
      }
      it("LR (union) must work with Option (None)") {
        val inst = LRHolder[Option[Int], String](List(None, "x"), ("y", None))
        val js = sj[LRHolder[Option[Int], String]].toJson(inst)
        js should matchJson("""{"a":["x"],"b":["y",null]}""")
      }
      it("LR (union) must work with Try of Option (non-None)") {
        val inst = LRHolder[Try[Option[Int]], String](List(Success(Some(5)), "x"), ("y", Success(Some(10))))
        val js = sj[LRHolder[Try[Option[Int]], String]].toJson(inst)
        js should matchJson("""{"a":[5,"x"],"b":["y",10]}""")
      }
      it("LR (union) must work with Try of Option (Success(None))") {
        val inst = LRHolder[Try[Option[Int]], String](List(Success(None), "x"), ("y", Success(None)))
        val js = sj[LRHolder[Try[Option[Int]], String]].toJson(inst)
        js should matchJson("""{"a":["x"],"b":["y",null]}""")
      }
      it("LR (union) must work with Try of Option (Failure)") {
        val inst = LRHolder[Try[Option[Int]], String](List(Failure(new Exception("boom")), "x"), ("y", Failure(new Exception("boom2"))))
        val js = sj[LRHolder[Try[Option[Int]], String]].toJson(inst)
        js should matchJson("""{"a":["x"],"b":["y",null]}""")
      }
    }
  }

  describe(colorString("-------------------------------\n:          Try Tests          :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("Try must work (Success)") {
        val inst = TryHolder(Success(15))
        val js = sj[TryHolder[Int]].toJson(inst)
        js should matchJson("""{"a":15}""")
      }
      it("Try of Option (non-None) must work (Success)") {
        val inst = TryHolder[Option[Int]](Success(Some(15)))
        val js = sj[TryHolder[Option[Int]]].toJson(inst)
        js should matchJson("""{"a":15}""")
      }
      it("Try of Option (None) must work (Success)") {
        val inst = TryHolder[Option[Int]](Success(None))
        val js = sj[TryHolder[Option[Int]]].toJson(inst)
        js should matchJson("""{}""")
      }
      it("Try w/policy AS_NULL must work (Failure)") {
        val inst = TryHolder[Int](Failure(new Exception("boom")))
        val js = sj[TryHolder[Int]](JsonConfig.withTryFailureHandling(TryPolicy.AS_NULL)).toJson(inst)
        js should matchJson("""{"a":null}""")
      }
      it("Try w/policy NO_WRITE must work (Failure)") {
        val inst = TryHolder[Int](Failure(new Exception("boom")))
        val js = sj[TryHolder[Int]](JsonConfig.withTryFailureHandling(TryPolicy.NO_WRITE)).toJson(inst)
        js should matchJson("""{}""")
      }
      it("Try w/policy ERR_MSG_STRING must work (Failure)") {
        val inst = TryHolder[Int](Failure(new Exception("boom")))
        val js = sj[TryHolder[Int]](JsonConfig.withTryFailureHandling(TryPolicy.ERR_MSG_STRING)).toJson(inst)
        js should matchJson("""{"a":"Try Failure with msg: boom"}""")
      }
      it("Try w/policy ATHROW_EXCEPTIONS_NULL must work (Failure)") {
        val inst = TryHolder[Int](Failure(new Exception("boom")))
        val caught =
          intercept[java.lang.Exception] {
            sj[TryHolder[Int]](JsonConfig.withTryFailureHandling(TryPolicy.THROW_EXCEPTION)).toJson(inst)
          }
        assert(caught.getMessage == "boom")
      }
      it("Seq and Tuple of Try must work for AS_NULL (Failure)") {
        val inst = TryHolder2[Int](List(Success(1), Failure(new Exception("boom")), Success(3)), (Failure(new Exception("boom")), Success(0)))
        val js = sj[TryHolder2[Int]](JsonConfig.withTryFailureHandling(TryPolicy.AS_NULL)).toJson(inst)
        js should matchJson("""{"a":[1,null,3],"b":[null,0]}""")
      }
      it("Seq and Tuple of Try must work for NO_WRITE (Failure)") {
        val inst = TryHolder2[Int](List(Success(1), Failure(new Exception("boom")), Success(3)), (Failure(new Exception("boom")), Success(0)))
        val js = sj[TryHolder2[Int]](JsonConfig.withTryFailureHandling(TryPolicy.NO_WRITE)).toJson(inst)
        js should matchJson("""{"a":[1,3],"b":[null,0]}""")
      }
      it("Seq and Tuple of Try of an Option must work for NO_WRITE (Failure)") {
        val inst = TryHolder2[Option[Int]](List(Success(None), Failure(new Exception("boom")), Success(Some(3))), (Failure(new Exception("boom")), Success(None)))
        val js = sj[TryHolder2[Option[Int]]](JsonConfig.withTryFailureHandling(TryPolicy.NO_WRITE)).toJson(inst)
        js should matchJson("""{"a":[3],"b":[null,null]}""")
      }
    }
  }
