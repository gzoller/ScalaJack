package co.blocke.scalajack.flexjson.typeadapter

import java.util.UUID

import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object UUIDTypeAdapter extends SimpleTypeAdapter[UUID] {

  override def read(reader: Reader): UUID =
    reader.peek match {
      case TokenType.String ⇒
        UUID.fromString(reader.readString())

      case TokenType.Null ⇒
        reader.readNull()
    }

  override def write(value: UUID, writer: Writer): Unit =
    if (value == null)
      writer.writeNull()
    else
      writer.writeString(value.toString)
}
