package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.json.{ StringJsonWriter, Tokenizer }
import co.blocke.scalajack.{ Reader, TypeAdapter, Writer }

case class NoncanonicalMapKeyParsingTypeAdapter[T](
    tokenizer:         Tokenizer,
    stringTypeAdapter: TypeAdapter[String],
    valueTypeAdapter:  TypeAdapter[T]
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val readValue = if (valueTypeAdapter.isStringKind)
      valueTypeAdapter.read(reader)
    else {
      val json = stringTypeAdapter.read(reader)
      val jsonCharArray = json.toCharArray
      val nestedReader = tokenizer.tokenize(jsonCharArray, 0, jsonCharArray.length)
      if (nestedReader.peek == TokenType.UnknownLiteralName) // String values in Any type
        nestedReader.poke(TokenType.String)
      val valueParsed = try {
        valueTypeAdapter.read(nestedReader)
      } catch {
        case t: java.lang.IllegalStateException => // Re-work the error message to point to the src text, not nested text for better clarity
          val msg = t.getMessage.split("\n")(0)
          throw new java.lang.IllegalStateException(msg + "\n" + reader.showError())
        case t: java.lang.NumberFormatException => // Re-work the error message to point to the src text, not nested text for better clarity
          val msg = t.getMessage.split("\n")(0)
          throw new java.lang.NumberFormatException(msg + "\n" + reader.showError())
      }
      if (nestedReader.hasNext)
        throw new java.lang.IllegalStateException("Cannot parse value into intended type\n" + reader.showError())
      valueParsed
    }
    if (readValue == null) throw new java.lang.IllegalStateException("Map keys cannot be null.\n" + reader.showError())
    else readValue
  }

  override def write(value: T, writer: Writer): Unit =
    if ((valueTypeAdapter.getClass == classOf[AnyTypeAdapter] && valueTypeAdapter.asInstanceOf[AnyTypeAdapter].inspectStringKind(value)) || valueTypeAdapter.isStringKind)
      valueTypeAdapter.write(value, writer)
    else {
      val nestedWriter = new StringJsonWriter()
      valueTypeAdapter.write(value, nestedWriter)
      val json = nestedWriter.jsonString
      stringTypeAdapter.write(json, writer)
    }

}
