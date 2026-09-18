package co.blocke.scalajack

import co.blocke.scalajack.json.reading.JsonSource
import co.blocke.scalajack.shared.{FastStringBuilder, StringMatrix}
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

    it("reads doubles directly and matches JDK rounding"):
      val literals = Seq(
        "0",
        "-0",
        "12345.6789",
        "1.7976931348623157E308",
        "4.9E-324",
        "9007199254740993",
        "1.2345678901234567890123456789",
        "1e-300",
        "1e309"
      )
      val random = new scala.util.Random(0x5ca1aL)
      val generated = Iterator.continually(java.lang.Double.longBitsToDouble(random.nextLong())).filter(_.isFinite).map(_.toString).take(10000)

      (literals.iterator ++ generated).foreach { literal =>
        val source = JsonSource(" \n\t" + literal + ",")
        val actual = source.expectDouble()
        java.lang.Double.doubleToRawLongBits(actual) shouldBe java.lang.Double.doubleToRawLongBits(java.lang.Double.parseDouble(literal))
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

    it("caches field-name matrices in the generated codec"):
      val codec = ScalaJack.sjCodecOf[MatrixHolder]
      val matrixFields = codec.jsonCodec.getClass.getDeclaredFields.filter(field => classOf[StringMatrix].isAssignableFrom(field.getType))
      matrixFields.length shouldBe 2
      matrixFields.foreach(_.setAccessible(true))
      val matrices = matrixFields.map(_.get(codec.jsonCodec))

      codec.fromJson("""{"value":1,"child":{"name":"one"}}""") shouldBe MatrixHolder(1, MatrixChild("one"))
      codec.fromJson("""{"child":{"name":"two"},"value":2}""") shouldBe MatrixHolder(2, MatrixChild("two"))

      matrixFields.map(_.get(codec.jsonCodec)).zip(matrices).foreach { case (after, before) =>
        (after eq before) shouldBe true
      }

case class MatrixChild(name: String)
case class MatrixHolder(value: Int, child: MatrixChild)
