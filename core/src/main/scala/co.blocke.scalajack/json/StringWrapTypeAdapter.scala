package co.blocke.scalajack
package json

import model._
import util.Path
import scala.collection.mutable.{ Builder, StringBuilder }

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
class StringWrapTypeAdapter[T](wrappedTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] {

  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean): T =
    wrappedTypeAdapter.read(path, reader.cloneWithSource(reader.readString(path).asInstanceOf[WIRE]), isMapKey)

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = {
    val stringBuilder = new StringBuilder()
    wrappedTypeAdapter.write(t, writer, stringBuilder.asInstanceOf[Builder[Any, WIRE]])
    writer.writeString(stringBuilder.result, out)
  }

}
