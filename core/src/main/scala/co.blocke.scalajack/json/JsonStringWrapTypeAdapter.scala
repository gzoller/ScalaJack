package co.blocke.scalajack
package json

import model._
import util.Path
import compat.StringBuilder
import typeadapter.StringWrapTypeAdapter

import scala.collection.mutable.Builder

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
class JsonStringWrapTypeAdapter[T](val wrappedTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] with Stringish with StringWrapTypeAdapter[T] {

  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): T =
    wrappedTypeAdapter.read(path, reader.jackFlavor.parse(reader.readString(path).asInstanceOf[WIRE]), isMapKey)

  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    val stringBuilder = new StringBuilder()
    wrappedTypeAdapter.write(t, writer, stringBuilder.asInstanceOf[Builder[Any, WIRE]], isMapKey)
    writer.writeString(stringBuilder.result, out)
  }
}
