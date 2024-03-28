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

class LRSpec() extends AnyFunSpec with JsonMatchers:

  describe(colorString("-------------------------------\n:         Either Tests        :\n-------------------------------", Console.YELLOW)) {
    it("Complex Either/Option must work (non-None)") {
    val inst = ComplexEither[Int](Some(Right(Some(3))))
    val sj = sjCodecOf[ComplexEither[Int]]
    val js = sj.toJson(inst)
    js should matchJson("""{"a":3}""")
    sj.fromJson(js) shouldEqual (inst)
    }
    it("Complex Either/Option must work (None no-write default)") {
    val inst = ComplexEither[Int](Some(Right(None)))
    val sj = sjCodecOf[ComplexEither[Int]]
    val js = sj.toJson(inst)
    js should matchJson("""{}""")
    sj.fromJson(js) shouldEqual (ComplexEither(null))
    }
    it("Complex Either/Option must work (NoneAsNull)") {
    val inst = ComplexEither[Int](Some(Right(None)))
    val sj = sjCodecOf[ComplexEither[Int]](JsonConfig.withNoneAsNull())
    val js = sj.toJson(inst)
    js should matchJson("""{"a":null}""")
    sj.fromJson(js) shouldEqual (ComplexEither(None)) // None here because value existed, but was null with NoneAsNull
    }
    it("Complex Either/Option must work (Left-NO_WRITE)") {
    val inst = ComplexEither[Int](Some(Left("err")))
    val sj = sjCodecOf[ComplexEither[Int]]
    val js = sjCodecOf[ComplexEither[Int]].toJson(inst)
    js should matchJson("""{}""")
    sj.fromJson(js) shouldEqual (ComplexEither(null)) // Null because value didn't exist at all
    }
    it("Complex Either/Option must work (Left-AS_VALUE)") {
    val inst = ComplexEither[Int](Some(Left("err")))
    val sj = sjCodecOf[ComplexEither[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE))
    val js = sj.toJson(inst)
    js should matchJson("""{"a":"err"}""")
    sj.fromJson(js) shouldEqual (inst)
    }
    it("Either with AS_VALUE left policy must work") {
    val inst = EitherHolder[Int](Left(5), Right(3))
    val sj = sjCodecOf[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_VALUE))
    val js = sj.toJson(inst)
    js should matchJson("""{"a":5,"b":3}""")
    sj.fromJson(js) shouldEqual (inst)
    }
    it("Either with AS_NULL left policy must work") {
    val inst = EitherHolder[Int](Left(5), Right(3))
    val sj = sjCodecOf[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.AS_NULL))
    val js = sj.toJson(inst)
    js should matchJson("""{"a":null,"b":3}""")
    sj.fromJson(js) shouldEqual (EitherHolder(null, Right(3)))
    }
    it("Either with NO_WRITE left policy must work") {
    val inst = EitherHolder[Int](Left(5), Right(3))
    val sj = sjCodecOf[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.NO_WRITE))
    val js = sj.toJson(inst)
    js should matchJson("""{"b":3}""")
    // This js cannot be read back in becuase it's missing required field 'a', which wasn't written out
    // per NO_WRITE policy.  This is a 1-way trip... so be advised...
    }
    it("Either with ERR_MSG_STRING left policy must work") {
    val inst = EitherHolder[Int](Left(5), Right(3))
    val sj = sjCodecOf[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.ERR_MSG_STRING))
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
        intercept[JsonEitherLeftError] {
        sjCodecOf[EitherHolder[Int]](JsonConfig.withEitherLeftHandling(EitherLeftPolicy.THROW_EXCEPTION)).toJson(inst)
        }
    assert(caught.getMessage == "Left Error: 5")
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
    // it("LR (union) must work with Try of Option (non-None)") {
    // val inst = LRHolder[Try[Option[Int]], String](List(Success(Some(5)), "x"), ("y", Success(Some(10))))
    // val js = sj[LRHolder[Try[Option[Int]], String]].toJson(inst)
    // js should matchJson("""{"a":[5,"x"],"b":["y",10]}""")
    // }
    // it("LR (union) must work with Try of Option (Success(None))") {
    // val inst = LRHolder[Try[Option[Int]], String](List(Success(None), "x"), ("y", Success(None)))
    // val js = sj[LRHolder[Try[Option[Int]], String]].toJson(inst)
    // js should matchJson("""{"a":["x"],"b":["y",null]}""")
    // }
    // it("LR (union) must work with Try of Option (Failure)") {
    // val inst = LRHolder[Try[Option[Int]], String](List(Failure(new Exception("boom")), "x"), ("y", Failure(new Exception("boom2"))))
    // val js = sj[LRHolder[Try[Option[Int]], String]].toJson(inst)
    // js should matchJson("""{"a":["x"],"b":["y",null]}""")
    // }
  }