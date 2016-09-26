package co.blocke.scalajack.typeadapter

import co.blocke.scalajack.json.{ StringJsonWriter, Tokenizer }
import co.blocke.scalajack.{ Reader, TypeAdapter, Writer }

case class StringParsingTypeAdapter[T](
    tokenizer:         Tokenizer,
    stringTypeAdapter: TypeAdapter[String],
    valueTypeAdapter:  TypeAdapter[T]
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    val json = stringTypeAdapter.read(reader)
    val jsonCharArray = json.toCharArray
    val nestedReader = tokenizer.tokenize(jsonCharArray, 0, jsonCharArray.length)
    valueTypeAdapter.read(nestedReader)
  }

  override def write(value: T, writer: Writer): Unit = {
    val nestedWriter = new StringJsonWriter(canonical = false)
    valueTypeAdapter.write(value, nestedWriter)
    val json = nestedWriter.jsonString
    stringTypeAdapter.write(json, writer)
  }

}
