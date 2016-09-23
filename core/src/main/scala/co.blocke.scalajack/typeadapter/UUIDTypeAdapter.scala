package co.blocke.scalajack
package typeadapter

import java.util.UUID

object UUIDTypeAdapter extends SimpleTypeAdapter[UUID] {

  override def read(reader: Reader): UUID =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        try {
          UUID.fromString(reader.readString())
        } catch {
          case u: java.lang.IllegalArgumentException => throw new java.lang.IllegalArgumentException(u.getMessage + "\n" + reader.showError())
        }

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading UUID value.\n" + reader.showError())
      }
    }

  override def write(value: UUID, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
