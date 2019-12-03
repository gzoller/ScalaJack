package co.blocke.scalajack
package mongo

import model._
import org.bson._
import compat.BsonBuilder

import scala.reflect.runtime.universe._
import scala.collection.mutable

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
class StringWrapTypeAdapter[T](val wrappedTypeAdapter: TypeAdapter[T])
  extends TypeAdapter[T]
  with Stringish {

  def read(parser: Parser): T = {
    // 1. Read String  (BsonValue --> String)
    val wrappedValueString = parser.expectString()

    wrappedTypeAdapter match {
      case value: ScalarTypeAdapter[_] =>
        value.scalarType match {
          case t if t == typeOf[Byte] =>
            wrappedValueString.toByte.asInstanceOf[T]
          case t if t == typeOf[Char] => wrappedValueString(0).asInstanceOf[T]
          case t if t == typeOf[Int]  => wrappedValueString.toInt.asInstanceOf[T]
          case t if t == typeOf[Long] =>
            wrappedValueString.toLong.asInstanceOf[T]
          case t if t == typeOf[Double] =>
            wrappedValueString.toDouble.asInstanceOf[T]
          case t if t == typeOf[Float] =>
            wrappedValueString.toFloat.asInstanceOf[T]
          case t if t == typeOf[Short] =>
            wrappedValueString.toShort.asInstanceOf[T]
          case t if t == typeOf[BigDecimal] =>
            BigDecimal(wrappedValueString).asInstanceOf[T]
          case t if t == typeOf[Boolean] =>
            wrappedValueString.toBoolean.asInstanceOf[T]
          // $COVERAGE-OFF$Currently all scalars in ScalaJack are supported.  Here just in case...
          case _ =>
            throw new ScalaJackError(
              "Only Scala scalar values are supported as BSON Map keys"
            )
          // $COVERAGE-ON$
        }
      case _ =>
        throw new ScalaJackError(
          "Only scalar values are supported as BSON Map keys"
        )
    }
  }

  def write[BsonValue](
      t:      T,
      writer: Writer[BsonValue],
      out:    mutable.Builder[BsonValue, BsonValue]): Unit = {
    val keyValBuilder =
      BsonBuilder().asInstanceOf[mutable.Builder[Any, BsonValue]]
    wrappedTypeAdapter.write(t, writer, keyValBuilder)
    val result = keyValBuilder.result() match {
      case r: BsonBoolean    => r.asBoolean().getValue.toString
      case r: BsonInt32      => r.asInt32().getValue.toString
      case r: BsonInt64      => r.asInt64().getValue.toString
      case r: BsonDecimal128 => r.asDecimal128().getValue.toString
      case r: BsonDouble     => r.asDouble().getValue.toString
      case r: BsonNumber     => r.asDecimal128().getValue.toString
      case r =>
        throw new ScalaJackError(
          "BSON type " + r.getClass.getName + " is not supported as a Map key"
        )
    }
    writer.writeString(result, out)
  }
}
