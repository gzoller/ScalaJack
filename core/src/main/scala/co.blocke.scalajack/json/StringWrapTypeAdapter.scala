package co.blocke.scalajack
package json

import co.blocke.scalajack.compat.StringBuilder
import model._

import scala.collection.mutable
import scala.reflect.runtime.universe._

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
case class StringWrapTypeAdapter[T](
    wrappedTypeAdapter: TypeAdapter[T],
    emptyStringOk:      Boolean        = true
)(implicit tt: TypeTag[T])
  extends TypeAdapter[T]
  with Stringish {

  def read(parser: Parser): T =
    parser.expectString() match {
      case null => null.asInstanceOf[T]
      case s if s.isEmpty && !emptyStringOk =>
        parser.backspace()
        throw new ScalaJackError(
          parser.showError(s"Expected a ${typeOf[T]} here")
        )
      case s =>
        wrappedTypeAdapter.read(parser.subParser(s.asInstanceOf[parser.WIRE]))
    }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    val stringBuilder = StringBuilder()
    wrappedTypeAdapter.write(
      t,
      writer,
      stringBuilder.asInstanceOf[mutable.Builder[Any, WIRE]]
    )
    writer.writeString(stringBuilder.result(), out)
  }
}
