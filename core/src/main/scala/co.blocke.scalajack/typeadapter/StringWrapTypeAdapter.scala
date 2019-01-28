package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.model.{ Transceiver, TypeAdapter }
import co.blocke.scalajack.util.Path

import scala.collection.mutable.{ Builder, StringBuilder }

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
class StringWrapTypeAdapter[T](val wrappedTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] {

  def read[WIRE](path: Path, reader: Transceiver[WIRE]): T =
    wrappedTypeAdapter.read(path, reader.cloneWithSource(reader.readString(path).asInstanceOf[WIRE]))

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = {
    val stringBuilder = new StringBuilder()
    wrappedTypeAdapter.write(t, writer, stringBuilder.asInstanceOf[Builder[Any, WIRE]])
    writer.writeString(stringBuilder.result, out)
  }

}
