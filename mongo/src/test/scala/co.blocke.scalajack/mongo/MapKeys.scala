package co.blocke.scalajack
package mongo

import TestUtil._
import munit._
import munit.internal.console
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
case class NumberKey(m: Map[java.lang.Number, Int])

case class WithMap(m: Map[Map[Int, Int], Int])

class MapKeys extends FunSuite:

  val sj = ScalaJack(MongoFlavor())

  test("Renders wrapped non-String scalar Map keys") {
    describe(
      "------------------------\n:  Map Keys (MongoDB) :\n------------------------", Console.BLUE
    )
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
    val j = NumberKey(Map(5.asInstanceOf[Number] -> 2))

    val docs = List(
      sj.render(a),
      sj.render(b),
      sj.render(c),
      sj.render(d),
      sj.render(e),
      sj.render(f),
      sj.render(g),
      sj.render(h),
      sj.render(i),
      sj.render(j),
    )
    assertEquals(docs.map(_.asDocument.toJson),
      List(
        """{"m": {"5": 2}}""",
        """{"m": {"5": 2}}""",
        """{"m": {"5.3": 2}}""",
        """{"m": {"5.0": 2}}""",
        """{"m": {"5": 2}}""",
        """{"m": {"5.1": 2}}""",
        """{"m": {"5": 2}}""",
        """{"m": {"true": 2}}""",
        """{"m": {"c": 2}}""",
        """{"m": {"5": 2}}"""
      )
    )

    assertEquals(sj.read[IntKey](docs(0)), a)
    assertEquals(sj.read[LongKey](docs(1)), b)
    assertEquals(sj.read[DoubleKey](docs(2)), c)
    assertEquals(sj.read[FloatKey](docs(3)), d)
    assertEquals(sj.read[ShortKey](docs(4)), e)
    assertEquals(sj.read[BigDecimalKey](docs(5)), f)
    assertEquals(sj.read[ByteKey](docs(6)), g)
    assertEquals(sj.read[BooleanKey](docs(7)), h)
    assertEquals(sj.read[CharKey](docs(8)), i)
    assertEquals(sj.read[NumberKey](docs(9)), j)
  }

  test("Traps attempt to read non-scalar Map key") {
    val d = BsonDocument.parse("""{"m":{"bogus":5}}""")
    interceptMessage[ScalaJackError]("Only scalar values are supported as BSON Map keys"){
      sj.read[WithMap](d) 
    }
  }

  test("Traps attempt to write non-scalar Map key") {
    val m = WithMap(Map(Map(1 -> 2) -> 3))
    interceptMessage[ScalaJackError]("BSON type org.bson.BsonDocument is not supported as a Map key"){
      sj.render(m)
    }
  }
