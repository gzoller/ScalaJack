package co.blocke.scalajack
package typeadapter

import model._
import util.{ Path, StringBuilder }

import scala.collection.mutable.Builder

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
class StringWrapTypeAdapter[T](val wrappedTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] with Stringish {

  def read[WIRE](path: Path, reader: Reader[WIRE]): T =
    wrappedTypeAdapter.read(path, reader.jackFlavor.parse(reader.readString(path).asInstanceOf[WIRE]))

  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    val stringBuilder = new StringBuilder()
    wrappedTypeAdapter.write(t, writer, stringBuilder.asInstanceOf[Builder[Any, WIRE]], isMapKey)
    writer.writeString(stringBuilder.result, out)
  }

}
