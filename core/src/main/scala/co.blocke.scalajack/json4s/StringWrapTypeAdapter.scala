package co.blocke.scalajack
package json4s

import model._
import util.Path
import org.json4s._

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
class StringWrapTypeAdapter[T](val wrappedTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] with Stringish {

  def read[WIRE](path: Path, reader: model.Reader[WIRE]): T = {
    // 1. Read String  (JValue --> String)
    val wrappedValueString = reader.readString(path)

    wrappedTypeAdapter.asInstanceOf[ScalarTypeAdapter[_]].scalarType match {
      case t if t == typeOf[Byte]       => wrappedValueString.toByte.asInstanceOf[T]
      case t if t == typeOf[Char]       => wrappedValueString(0).asInstanceOf[T]
      case t if t == typeOf[Int]        => wrappedValueString.toInt.asInstanceOf[T]
      case t if t == typeOf[Long]       => wrappedValueString.toLong.asInstanceOf[T]
      case t if t == typeOf[Double]     => wrappedValueString.toDouble.asInstanceOf[T]
      case t if t == typeOf[Float]      => wrappedValueString.toFloat.asInstanceOf[T]
      case t if t == typeOf[Short]      => wrappedValueString.toShort.asInstanceOf[T]
      case t if t == typeOf[BigInt]     => BigInt(wrappedValueString).asInstanceOf[T]
      case t if t == typeOf[BigDecimal] => BigDecimal(wrappedValueString).asInstanceOf[T]
      case t if t == typeOf[Boolean]    => wrappedValueString.toBoolean.asInstanceOf[T]
      // $COVERAGE-OFF$Currently all scalars in ScalaJack are supported.  Here just in case...
      case _ =>
        reader.back
        throw new ReadInvalidError(reader.showError(path, "Only Scala scalar values are supported as JValue Map keys"))
      // $COVERAGE-ON$
    }
  }

  def write[WIRE](t: T, writer: model.Writer[WIRE], out: collection.mutable.Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    val keyValBuilder = new JValueBuilder().asInstanceOf[collection.mutable.Builder[Any, WIRE]]
    wrappedTypeAdapter.write(t, writer, keyValBuilder, isMapKey)
    val result = keyValBuilder.result() match {
      case r: JBool    => r.values.toString
      case r: JDecimal => r.values.toString
      case r: JDouble  => r.values.toString
      case r: JInt     => r.values.toString
      case r: JLong    => r.values.toString
      // $COVERAGE-OFF$All scalar/wrapped JValues supported as of this writing.  Here just in case someone invents a new one.
      case r           => throw new SJError("Json4s type " + r.getClass.getName + " is not supported as a Map key")
      // $COVERAGE-ON$
    }
    writer.writeString(result, out)
  }
}
