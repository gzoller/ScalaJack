package co.blocke.scalajack
package mongo

import model._
import util.Path
import org.mongodb.scala.bson._

import scala.collection.mutable.Builder

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
class StringWrapTypeAdapter[T](val wrappedTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T] with Stringish {

  def read[WIRE](path: Path, reader: Reader[WIRE]): T = {
    // 1. Read String  (BsonValue --> String)
    val wrappedValueString = reader.readString(path)

    // 2. Based on type, conver String --> T or toss cookies
    wrappedTypeAdapter match {
      case t if t.isInstanceOf[TypeAdapter[Int]]        => wrappedValueString.toInt.asInstanceOf[T]
      case t if t.isInstanceOf[TypeAdapter[Long]]       => wrappedValueString.toLong.asInstanceOf[T]
      case t if t.isInstanceOf[TypeAdapter[Double]]     => wrappedValueString.toDouble.asInstanceOf[T]
      case t if t.isInstanceOf[TypeAdapter[Float]]      => wrappedValueString.toFloat.asInstanceOf[T]
      case t if t.isInstanceOf[TypeAdapter[Short]]      => wrappedValueString.toShort.asInstanceOf[T]
      case t if t.isInstanceOf[TypeAdapter[BigInt]]     => BigInt(wrappedValueString).asInstanceOf[T]
      case t if t.isInstanceOf[TypeAdapter[BigDecimal]] => BigDecimal(wrappedValueString).asInstanceOf[T]
      case t if t.isInstanceOf[TypeAdapter[Boolean]]    => wrappedValueString.toBoolean.asInstanceOf[T]
      case _ =>
        reader.back
        throw new ReadInvalidError(reader.showError(path, "Only scalar values are supported as BSON Map keys"))
    }
  }

  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    val keyValBuilder = new BsonBuilder().asInstanceOf[Builder[Any, WIRE]]
    wrappedTypeAdapter.write(t, writer, keyValBuilder, isMapKey)
    val result = keyValBuilder.result() match {
      case r: BsonBoolean    => r.asBoolean().getValue().toString
      case r: BsonInt32      => r.asInt32().getValue().toString
      case r: BsonInt64      => r.asInt64().getValue().toString
      case r: BsonDecimal128 => r.asDecimal128().getValue().toString
      case r: BsonDouble     => r.asDouble().getValue().toString
      case r: BsonString     => r.asString().getValue()
      case r                 => throw new SJError("BSON type " + r.getClass.getName + " is not supported as a Map key")
    }
    writer.writeString(result, out)
  }
}
