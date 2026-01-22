package co.blocke.scalajack
package json
package misc

import ScalaJack.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import scala.util.*
import TestUtil.*

class LRSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:         Either Tests        :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("""/// Right Tests ///""")) {
      it("Complex Either/Option must work (non-None)") {
        val inst = ComplexEither[Int](Some(Right(Some(3))))
        val sj = sjCodecOf[ComplexEither[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":3}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Complex Either/Option must work (None -> null for Either)") {
        val inst = ComplexEither[Int](Some(Right(None)))
        val sj = sjCodecOf[ComplexEither[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{}""")
        sj.fromJson(js) shouldEqual (ComplexEither(None))
      }
      it("Complex Either/Option must work (NoneAsNull)") {
        val inst = ComplexEither[Int](Some(Right(None)))
        val sj = sjCodecOf[ComplexEither[Int]](SJConfig.withNoneAsNull)
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""") // same output result regardless of noneAsNull setting
        sj.fromJson(js) shouldEqual (ComplexEither(None)) // None here because value existed, but was null with NoneAsNull
      }
    }
    describe(colorString("""\\\ Left Tests \\\""")) {
      it("Complex Either/Option must work (Left-default: AS_VALUE)") {
        val inst = ComplexEither[Int](Some(Left("msg")))
        val sj = sjCodecOf[ComplexEither[Int]]
        val js = sjCodecOf[ComplexEither[Int]].toJson(inst)
        js should matchJson("""{"a":"msg"}""")
        sj.fromJson(js) shouldEqual inst
      }
      it("Complex Either/Option must work (Left-AS_NULL)") {
        val inst = ComplexEither[Int](Some(Left("err")))
        val sj = sjCodecOf[ComplexEither[Int]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_NULL))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (ComplexEither(null))
      }
      it("Complex Either/Option must work (Left-AS_NULL, Option nullAsNull)") {
        val inst = ComplexEither[Int](Some(Left("err")))
        val sj = sjCodecOf[ComplexEither[Int]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_NULL).withNoneAsNull)
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null}""")
        sj.fromJson(js) shouldEqual (ComplexEither(None))
      }
      it("Either with AS_VALUE (default) left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val sj = sjCodecOf[EitherHolder[Int]]
        val js = sj.toJson(inst)
        js should matchJson("""{"a":5,"b":3}""")
        sj.fromJson(js) shouldEqual (inst)
      }
      it("Either with AS_NULL left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val sj = sjCodecOf[EitherHolder[Int]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.AS_NULL))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":null,"b":3}""")
        sj.fromJson(js) shouldEqual (EitherHolder(null, Right(3)))
      }
      it("Either with ERR_MSG_STRING left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val sj = sjCodecOf[EitherHolder[Int]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.ERR_MSG_STRING))
        val js = sj.toJson(inst)
        js should matchJson("""{"a":"Left Error: 5","b":3}""")
        sj.fromJson(js) shouldEqual (EitherHolder(Right("Left Error: 5"), Right(3)))
        // WARNING!  Here a Left(err_msg) was "promoted" to a Right(String) upon read, because Right was of type
        // String, and "Left Error: 5" is a valid string.  Use with extreme caution.  Best to consider this a 1-way
        // trip for debugging purposes only.  You have been warned.
      }
      it("Either with THROW_EXCEPTION left policy must work") {
        val inst = EitherHolder[Int](Left(5), Right(3))
        val caught =
          intercept[EitherLeftError] {
            sjCodecOf[EitherHolder[Int]](SJConfig.withEitherLeftHandling(EitherLeftPolicy.THROW_EXCEPTION)).toJson(inst)
          }
        assert(caught.getMessage == "Left Error: 5")
      }
    }
  }

  describe(colorString("----------------------------------\n:           Union Tests          :\n----------------------------------", Console.YELLOW)) {
    it("LR (union) must work with Option (non-None)") {
      val inst = LRUnionHolder[Option[Int], String](List(Some(5), "x"), ("y", Some(10)))
      val sj = sjCodecOf[LRUnionHolder[Option[Int], String]]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":[5,"x"],"b":["y",10]}""")
      sj.fromJson(js) shouldEqual inst
    }
    it("LR (union) must work with Option (None)") {
      val inst = LRUnionHolder[Option[Int], String](List(None, "x"), ("y", None))
      val sj = sjCodecOf[LRUnionHolder[Option[Int], String]]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":["x"],"b":["y",null]}""")
      sj.fromJson(js) shouldEqual LRUnionHolder[Option[Int], String](List("x"), ("y", None))
    }
    it("LR (union) must work with Try of Option (non-None)") {
      val inst = LRUnionHolder[Try[Option[Int]], String](List(Success(Some(5)), "x"), ("y", Success(Some(10))))
      val sj = sjCodecOf[LRUnionHolder[Try[Option[Int]], String]]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":[5,"x"],"b":["y",10]}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("LR (union) must work with Try of Option (Success(None))") {
      val inst = LRUnionHolder[Try[Option[Int]], String](List(Success(None), "x"), ("y", Success(None)))
      val sj = sjCodecOf[LRUnionHolder[Try[Option[Int]], String]]
      val js = sj.toJson(inst)
      sj.fromJson(js) shouldEqual (LRUnionHolder(List("x"), ("y", null))) // None's get swallowed
    }
    it("LR (union) must work with Try of Option (Failure)") {
      val inst = LRUnionHolder[Try[Option[Int]], String](List(Failure(new Exception("boom")), "x"), ("y", Failure(new Exception("boom2"))))
      val sj = sjCodecOf[LRUnionHolder[Try[Option[Int]], String]]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":[null,"x"],"b":["y",null]}""")
      sj.fromJson(js) shouldEqual (LRUnionHolder(List(null, "x"), ("y", null))) // Left's come back as null
    }
  }

  // describe(colorString("----------------------------------\n:       Intersection Tests       :\n----------------------------------", Console.YELLOW)) {
  //   it("LR (intersection) must work") {
  //     val inst = LRUnionHolder[Option[Int], String](List(Some(5), "x"), ("y", Some(10)))
  //     val sj = sjCodecOf[LRUnionHolder[Option[Int], String]]
  //     val js = sj.toJson(inst)
  //     js should matchJson("""{"a":[5,"x"],"b":["y",10]}""")
  //     sj.fromJson(js) shouldEqual inst
  //   }
  // }
