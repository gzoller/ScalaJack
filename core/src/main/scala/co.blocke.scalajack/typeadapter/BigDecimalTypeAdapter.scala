package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, typeOf }

object BigDecimalTypeAdapter extends TypeAdapter.=:=[BigDecimal] {

  override object deserializer extends Deserializer[BigDecimal] {

    private val BigDecimalType: Type = typeOf[BigDecimal]

    override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[BigDecimal] =
      json match {
        case JsonInt(x)  => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
        case JsonLong(x) => DeserializationResult(path)(TypeTagged(BigDecimal(x), BigDecimalType))
      }

  }

  override def read(reader: Reader): BigDecimal =
    reader.peek match {
      case TokenType.Number =>
        reader.read(expected = TokenType.Number)
        BigDecimal(reader.tokenText)

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigDecimal value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: BigDecimal, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}
