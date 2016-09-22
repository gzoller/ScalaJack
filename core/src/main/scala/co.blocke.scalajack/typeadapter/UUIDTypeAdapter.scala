package co.blocke.scalajack
package typeadapter

import java.util.UUID

object UUIDTypeAdapter extends SimpleTypeAdapter[UUID] {

  override def read(reader: Reader): UUID =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        UUID.fromString(reader.readString())
    }

  override def write(value: UUID, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
