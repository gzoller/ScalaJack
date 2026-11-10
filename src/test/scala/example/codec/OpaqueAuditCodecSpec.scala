package example.codec

import co.blocke.scalajack.{JsonParseError, ScalaJack, SJConfig}
import example.model.*
import java.util.UUID
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class OpaqueAuditCodecSpec extends AnyFunSpec:
  given ScalaJack[AuditActor] = ScalaJack.sjCodecOf[AuditActor](SJConfig.withNoneAsNull)
  private val uuid = UUID.fromString("019a6f80-0000-7000-8000-000000000001")
  private val id = UserId(uuid)

  describe("Opaque phantom-tagged IDs outside their definition scope") {
    it("round-trips payload enum cases with their UUID JSON representation") {
      val actors = List[AuditActor](
        AuditActor.System("bootstrap"),
        AuditActor.User(UserId(UUID.fromString("019a6f80-0000-7000-8000-000000000001")))
      )
      val expected = List(
        """{"code":"bootstrap"}""",
        """{"userId":"019a6f80-0000-7000-8000-000000000001"}"""
      )
      val sj = summon[ScalaJack[AuditActor]]
      actors.zip(expected).foreach { (original, json) =>
        sj.toJson(original) shouldEqual json
        sj.fromJson(json) shouldEqual original
      }
    }
    it("round-trips a standalone opaque ID and lists of IDs") {
      val sj = ScalaJack.sjCodecOf[UserId]
      sj.toJson(id) shouldEqual s"\"$uuid\""
      sj.fromJson(sj.toJson(id)) shouldEqual id
      sj.fromJsonList(sj.toJsonList(List(id))) shouldEqual List(id)
    }
    it("supports the default enum configuration and explicit type hints") {
      val actor = AuditActor.User(id)
      val default = ScalaJack.sjCodecOf[AuditActor]
      default.toJson(actor) shouldEqual s"""{"userId":"$uuid"}"""
      default.fromJson(default.toJson(actor)) shouldEqual actor
      val hinted = ScalaJack.sjCodecOf[AuditActor](SJConfig.preferTypeHints)
      hinted.toJson(actor) shouldEqual s"""{"_hint":"User","userId":"$uuid"}"""
      hinted.fromJson(hinted.toJson(actor)) shouldEqual actor
    }
    it("supports opaque IDs in ordinary fields, options, lists and map values") {
      val sj = ScalaJack.sjCodecOf[OpaqueIdHolder]
      val value = OpaqueIdHolder(id, Some(id), List(id), Map("user" -> id))
      sj.toJson(value) shouldEqual s"""{"id":"$uuid","optional":"$uuid","history":["$uuid"],"byName":{"user":"$uuid"}}"""
      sj.fromJson(sj.toJson(value)) shouldEqual value
      sj.fromJson(s"""{"id":"$uuid","history":[],"byName":{}}""") shouldEqual OpaqueIdHolder(id, None, Nil, Map.empty)
    }
    it("preserves Scala defaults for opaque ID fields") {
      val sj = ScalaJack.sjCodecOf[OpaqueIdDefault]
      sj.fromJson("{}") shouldEqual OpaqueIdDefault()
      sj.fromJson(sj.toJson(OpaqueIdDefault())) shouldEqual OpaqueIdDefault()
      val other = OpaqueIdDefault(UserId(UUID.fromString("019a6f80-0000-7000-8000-000000000002")))
      sj.fromJson(sj.toJson(other)) shouldEqual other
    }
    it("still requires an opaque ID payload field") {
      val ex = intercept[JsonParseError](summon[ScalaJack[AuditActor]].fromJson("""{"_hint":"User"}"""))
      ex.getMessage should include("Missing required field(s) userId")
    }
    it("preserves null handling for opaque reference types") {
      val sj = ScalaJack.sjCodecOf[UserId]
      sj.toJson(null.asInstanceOf[UserId]) shouldEqual "null"
      sj.fromJson("null") shouldEqual null.asInstanceOf[UserId]
    }
    it("rejects malformed UUID payloads") {
      intercept[IllegalArgumentException](summon[ScalaJack[AuditActor]].fromJson("""{"userId":"not-a-uuid"}"""))
    }
    it("round-trips simple opaque aliases of every JSON-supported Scala primitive and String") {
      def check[T](sj: ScalaJack[T], value: T, expected: String): org.scalatest.Assertion =
        sj.toJson(value) shouldEqual expected
        sj.fromJson(expected) shouldEqual value
        sj.fromJsonList(sj.toJsonList(List(value))) shouldEqual List(value)

      import PrimitiveOpaques.*
      check(ScalaJack.sjCodecOf[ByteValue], byte(Byte.MinValue), "-128")
      check(ScalaJack.sjCodecOf[ShortValue], short(Short.MaxValue), "32767")
      check(ScalaJack.sjCodecOf[IntValue], int(Int.MinValue), "-2147483648")
      check(ScalaJack.sjCodecOf[LongValue], long(Long.MaxValue), "9223372036854775807")
      check(ScalaJack.sjCodecOf[FloatValue], float(1.25f), "1.25")
      check(ScalaJack.sjCodecOf[DoubleValue], double(-2.5), "-2.5")
      check(ScalaJack.sjCodecOf[BooleanValue], boolean(false), "false")
      check(ScalaJack.sjCodecOf[CharValue], char('x'), "\"x\"")
      check(ScalaJack.sjCodecOf[StringValue], string("opaque"), "\"opaque\"")
    }
    it("round-trips phantom-tagged opaque aliases with primitive, String and UUID representations") {
      def check[T](sj: ScalaJack[TaggedValue[UserIdTag, T]], value: T, expected: String): org.scalatest.Assertion =
        val wrapped = TaggedValue.wrap[UserIdTag, T](value)
        sj.toJson(wrapped) shouldEqual expected
        sj.fromJson(expected) shouldEqual wrapped
        sj.fromJsonList(sj.toJsonList(List(wrapped))) shouldEqual List(wrapped)

      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Byte]], Byte.MinValue, "-128")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Short]], Short.MaxValue, "32767")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Int]], Int.MinValue, "-2147483648")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Long]], Long.MaxValue, "9223372036854775807")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Float]], 1.25f, "1.25")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Double]], -2.5, "-2.5")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Boolean]], false, "false")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, Char]], 'x', "\"x\"")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, String]], "opaque", "\"opaque\"")
      check(ScalaJack.sjCodecOf[TaggedValue[UserIdTag, UUID]], uuid, s"\"$uuid\"")
    }
    it("supports primitive opaque fields, defaults and required-field checks") {
      val sj = ScalaJack.sjCodecOf[PrimitiveOpaqueFields]
      val value = PrimitiveOpaqueFields(PrimitiveOpaques.int(0), TaggedValue.wrap[UserIdTag, Long](0L))
      val json = """{"count":0,"total":0,"enabled":true}"""
      sj.toJson(value) shouldEqual json
      sj.fromJson(json) shouldEqual value
      sj.fromJson("""{"count":0,"total":0}""") shouldEqual value
      intercept[JsonParseError](sj.fromJson("""{"total":0}""")).getMessage should include("Missing required field(s) count")
      intercept[JsonParseError](sj.fromJson("""{"count":0,"total":0,"enabled":true,"enabled":false}""")).getMessage should include("Duplicate field enabled")
    }
    it("supports primitive and UUID opaque types together in payload enums") {
      val sj = ScalaJack.sjCodecOf[PrimitiveOpaqueActor]
      val actors = List[PrimitiveOpaqueActor](
        PrimitiveOpaqueActor.Count(PrimitiveOpaques.int(42)),
        PrimitiveOpaqueActor.TaggedCount(CounterId.wrap[UserIdTag](7)),
        PrimitiveOpaqueActor.User(id)
      )
      val expected = List("""{"count":42}""", """{"taggedCount":7}""", s"""{"userId":"$uuid"}""")
      actors.zip(expected).foreach { (actor, json) =>
        sj.toJson(actor) shouldEqual json
        sj.fromJson(json) shouldEqual actor
      }
    }
  }
