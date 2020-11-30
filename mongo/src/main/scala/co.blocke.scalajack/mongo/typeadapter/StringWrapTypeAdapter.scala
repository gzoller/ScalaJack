package co.blocke.scalajack
package mongo
package typeadapter

import model._
import org.bson._
import co.blocke.scalajack.typeadapter.AnyTypeAdapter
import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.impl.PrimitiveType
import co.blocke.scalajack.typeadapter._

import scala.collection.mutable

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
case class StringWrapTypeAdapter[T](wrappedTypeAdapter: TypeAdapter[T]) extends TypeAdapter[T]:

  override def isStringish: Boolean = true
  val info: RType = wrappedTypeAdapter.info

  def read(parser: Parser): T =
    val wrappedValueString = parser.expectString()
    wrappedTypeAdapter match {
      case value: ScalarTypeAdapter[_] =>
        value.info match {
          case PrimitiveType.Scala_Byte    => wrappedValueString.toByte.asInstanceOf[T]
          case PrimitiveType.Scala_Int     => wrappedValueString.toInt.asInstanceOf[T]
          case PrimitiveType.Scala_Long    => wrappedValueString.toLong.asInstanceOf[T]
          case PrimitiveType.Scala_Double  => wrappedValueString.toDouble.asInstanceOf[T]
          case PrimitiveType.Scala_Float   => wrappedValueString.toFloat.asInstanceOf[T]
          case PrimitiveType.Scala_Short   => wrappedValueString.toShort.asInstanceOf[T]
          case PrimitiveType.Scala_Boolean => wrappedValueString.toBoolean.asInstanceOf[T]
          case PrimitiveType.Java_Number   => wrappedValueString.toDouble.asInstanceOf[T]
          case r: RType if r.name == "scala.math.BigInt"     => BigInt(wrappedValueString).asInstanceOf[T]
          case r: RType if r.name == "scala.math.BigDecimal" => BigDecimal(wrappedValueString).asInstanceOf[T]
          // $COVERAGE-OFF$Currently all scalars in ScalaJack are supported.  Here just in case...
          case _ =>
            throw new ScalaJackError(
              "Only Scala scalar values are supported as BSON Map keys"
            )
          // $COVERAGE-ON$
        }
      case value: AnyTypeAdapter =>
        value.read(parser).asInstanceOf[T]
      case _ =>
        throw new ScalaJackError(
          "Only scalar values are supported as BSON Map keys"
        )
    }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    val keyValBuilder = BsonBuilder().asInstanceOf[mutable.Builder[Any, WIRE]]
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
