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

class MiscSpec() extends AnyFunSpec with JsonMatchers:
  opaque type Count = Int
  opaque type CountX = Option[Int]
  type CountY = String
  type CountZ = Option[String]

  describe(colorString("-------------------------------\n:          Misc Tests         :\n-------------------------------", Console.YELLOW)) {
    describe(colorString("+++ Positive Tests +++")) {
      it("String escaping must work (proves escape can be turned off)") {
        val inst = StringHolder("""This is a "strange" test
on another level.""")
        val js1 = sj[StringHolder].toJson(inst)
        val js2 = sj[StringHolder](JsonConfig.withSuppressEscapedStrings()).toJson(inst)
        js1 should equal("""{"a":"This is a \"strange\" test\non another level."}""")
        js2 should equal("""{"a":"This is a "strange" test
on another level."}""")
      }
      it("NeoType integration must work") {
        val inst = Validated(NonEmptyString("Mike"), XList(List("x", "y", "z")), List(EmptyString(""), EmptyString(""), EmptyString("")))
        val js = sj[Validated].toJson(inst)
        js should equal("""{"name":"Mike","xspot":["x","y","z"],"nada":["","",""]}""")
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
        val js = sj[AnyHolder].toJson(inst)
        js should equal("""{"maybe":[1,2,3],"itried":{"a":-5},"itried2":99,"anymap":{"a":1,"b":2},"whichOneR":3,"bunch":["a",null,"b"]}""")
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
        val js = sj[AnyHolder](
          JsonConfig
            .withNoneAsNull()
            .withEitherLeftHandling(EitherLeftPolicy.ERR_MSG_STRING)
            .withTryFailureHandling(TryPolicy.AS_NULL)
        ).toJson(inst)
        js should equal("""{"maybe":[1,2,3],"maybeNot":"null","itried":{"a":-5},"itried2":99,"ifailed":"null","anymap":{"a":1,"b":2},"whichOneR":3,"whichOneL":"Left Error: nope","bunch":["a",null,"b"]}""")
      }
    }
  }
