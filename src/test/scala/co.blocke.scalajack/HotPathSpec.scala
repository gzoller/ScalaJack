package co.blocke.scalajack

import co.blocke.scalajack.json.reading.JsonSource
import co.blocke.scalajack.shared.FastStringBuilder
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

class HotPathSpec extends AnyFunSpec:
  describe("optimized JSON hot paths"):
    it("writes integer boundaries without intermediate strings"):
      val values = Seq[Long](
        Long.MinValue,
        Int.MinValue.toLong,
        -101,
        -100,
        -10,
        -1,
        0,
        1,
        9,
        10,
        99,
        100,
        101,
        Int.MaxValue.toLong,
        Long.MaxValue
      )
      val builder = new FastStringBuilder(0)
      values.foreach { value =>
        builder.clear()
        builder.append(value)
        builder.result shouldBe value.toString
      }

    it("reads integer boundaries and leaves the delimiter unread"):
      Seq(Int.MinValue, -1, 0, 1, 9999, 123456789, Int.MaxValue).foreach { value =>
        val source = JsonSource(value.toString + ",")
        source.expectInt() shouldBe value
        source.readToken() shouldBe ','
      }

    it("reads long boundaries and leaves the delimiter unread"):
      Seq(Long.MinValue, Int.MinValue.toLong, -1L, 0L, 1L, 1234567890123456789L, Long.MaxValue).foreach { value =>
        val source = JsonSource(value.toString + ",")
        source.expectLong() shouldBe value
        source.readToken() shouldBe ','
      }

    it("escapes every non-ASCII code unit and continues through the rest of the string"):
      val builder = new FastStringBuilder()
      val value = "préfix☆tail😀done"
      builder.appendEscaped(value, 0, value.length)
      builder.result shouldBe "pr\\u00e9fix\\u2606tail\\ud83d\\ude00done"

    it("decodes unicode escapes without allocating intermediate strings"):
      JsonSource("\"pr\\u00e9fix\\u2606tail\\ud83d\\ude00done\"").expectString() shouldBe "préfix☆tail😀done"

    it("uses separate pooled outputs for reentrant serialization"):
      ScalaJack.withJsonOutput(null) { outer =>
        ScalaJack.withJsonOutput(null) { inner =>
          (inner eq outer) shouldBe false
        }
      }
