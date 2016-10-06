package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.json.{ StringJsonWriter, Tokenizer, TokenReader }
import co.blocke.scalajack.{ Reader, TypeAdapter, Writer }

case class NoncanonicalMapKeyParsingTypeAdapter[T](
    tokenizer:         Tokenizer,
    stringTypeAdapter: TypeAdapter[String],
    valueTypeAdapter:  TypeAdapter[T]
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val readValue = valueTypeAdapter match {
      case vta: StringKind ⇒ valueTypeAdapter.read(reader)
      case vta: OptionTypeAdapterEmpty[_] if (vta.valueTypeAdapter.isInstanceOf[StringKind]) => valueTypeAdapter.read(reader)
      case _ ⇒
        val json = stringTypeAdapter.read(reader)
        val jsonCharArray = json.toCharArray
        val nestedReader = try {
          tokenizer.tokenize(jsonCharArray, 0, jsonCharArray.length)
        } catch {
          case t: java.lang.IllegalArgumentException ⇒
            val msg = t.getMessage.split("\n")(0)
            throw new java.lang.IllegalArgumentException(msg + "\n" + tokenizer.showError() + " Extracted from source here:\n" + reader.showError())
        }
        if (nestedReader.peek == TokenType.UnknownLiteralName) // String values in Any type
          nestedReader.poke(
            TokenType.String,
            newTokenOffset = oldTokenOffset => oldTokenOffset - 1, // Simulate the leading double-quote
            newTokenLength = oldTokenLength => oldTokenLength + 2 // Simulate both the leading and trailing double-quotes
          )
        val valueParsed = try {
          valueTypeAdapter.read(nestedReader)
        } catch {
          case t: java.lang.IllegalStateException ⇒ // Re-work the error message to point to the src text, not nested text for better clarity
            val msg = t.getMessage.split("\n")(0)
            throw new java.lang.IllegalStateException(msg + "\n" + reader.showError())
          case t: java.lang.NumberFormatException ⇒ // Re-work the error message to point to the src text, not nested text for better clarity
            val msg = t.getMessage.split("\n")(0)
            throw new java.lang.NumberFormatException(msg + "\n" + reader.showError())
          case t: java.lang.ClassNotFoundException ⇒
            val msg = t.getMessage.split("\n")(0)
            throw new java.lang.ClassNotFoundException(msg + "\n" + reader.showError())
        }
        // if (nestedReader.hasNext)
        //   throw new java.lang.IllegalStateException("Cannot parse value into intended type\n" + reader.showError())
        valueParsed
    }
    if (readValue == null) throw new java.lang.IllegalStateException("Map keys cannot be null.\n" + reader.showError())
    else readValue
  }

  override def write(value: T, writer: Writer): Unit = {
    if (value == null)
      throw new java.lang.IllegalStateException("Attempting to write a null map key (map keys may not be null).")
    valueTypeAdapter.resolved match {
      case vta: AnyTypeAdapter if (vta.inspectStringKind(value)) ⇒ valueTypeAdapter.write(value, writer)
      case vta: OptionTypeAdapterEmpty[_] if (vta.valueTypeAdapter.isInstanceOf[StringKind] || value == None) ⇒ valueTypeAdapter.write(value, writer)
      case vta: StringKind ⇒ valueTypeAdapter.write(value, writer)
      case _ ⇒
        val nestedWriter = new StringJsonWriter()
        valueTypeAdapter.write(value, nestedWriter)
        val json = nestedWriter.jsonString
        stringTypeAdapter.write(json, writer)
    }
  }
}
