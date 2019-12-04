package co.blocke.scalajack
package mongo

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Matchers
import java.util.UUID
import org.bson.BsonDocument

case class IntKey(m: Map[Int, Int])
case class LongKey(m: Map[Long, Int])
case class DoubleKey(m: Map[Double, Int])
case class FloatKey(m: Map[Float, Int])
case class ShortKey(m: Map[Short, Int])
case class BigDecimalKey(m: Map[BigDecimal, Int])
case class ByteKey(m: Map[Byte, Int])
case class BooleanKey(m: Map[Boolean, Int])
case class CharKey(m: Map[Char, Int])
case class UUIDKey(m: Map[UUID, Int])

case class WithMap(m: Map[Map[Int, Int], Int])

class MapKeys extends AnyFunSpec with Matchers {

  val sj = ScalaJack(MongoFlavor())

  describe(
    "------------------------\n:  Map Keys (MongoDB) :\n------------------------"
  ) {
      it("Renders wrapped non-String scalar Map keys") {
        val short: Short = 5
        val bte: Byte = 5
        val a = IntKey(Map(5 -> 2))
        val b = LongKey(Map(5L -> 2))
        val c = DoubleKey(Map(5.3 -> 2))
        val d = FloatKey(Map(5F -> 2))
        val e = ShortKey(Map(short -> 2))
        val f = BigDecimalKey(Map(BigDecimal(5.1) -> 2))
        val g = ByteKey(Map(bte -> 2))
        val h = BooleanKey(Map(true -> 2))
        val i = CharKey(Map('c' -> 2))

        val docs = List(
          sj.render(a),
          sj.render(b),
          sj.render(c),
          sj.render(d),
          sj.render(e),
          sj.render(f),
          sj.render(g),
          sj.render(h),
          sj.render(i)
        )
        docs.map(_.asDocument.toJson) should be(
          List(
            """{"m": {"5": 2}}""",
            """{"m": {"5": 2}}""",
            """{"m": {"5.3": 2}}""",
            """{"m": {"5.0": 2}}""",
            """{"m": {"5": 2}}""",
            """{"m": {"5.1": 2}}""",
            """{"m": {"5": 2}}""",
            """{"m": {"true": 2}}""",
            """{"m": {"c": 2}}"""
          )
        )

        sj.read[IntKey](docs(0)) should be(a)
        sj.read[LongKey](docs(1)) should be(b)
        sj.read[DoubleKey](docs(2)) should be(c)
        sj.read[FloatKey](docs(3)) should be(d)
        sj.read[ShortKey](docs(4)) should be(e)
        sj.read[BigDecimalKey](docs(5)) should be(f)
        sj.read[ByteKey](docs(6)) should be(g)
        sj.read[BooleanKey](docs(7)) should be(h)
        sj.read[CharKey](docs(8)) should be(i)
      }
      it("Traps attempt to read non-scalar Map key") {
        val d = BsonDocument.parse("""{"m":{"bogus":5}}""")
        the[ScalaJackError] thrownBy sj.read[WithMap](d) should have message "Only scalar values are supported as BSON Map keys"
      }
      it("Traps attempt to write non-scalar Map key") {
        val m = WithMap(Map(Map(1 -> 2) -> 3))
        the[ScalaJackError] thrownBy sj.render(m) should have message "BSON type org.bson.BsonDocument is not supported as a Map key"
      }
    }
}
