package co.blocke.scalajack
package json4s

import typeadapter.AnyTypeAdapter
import model._
import org.json4s._
import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.impl.PrimitiveType

import scala.collection.mutable

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
case class StringWrapTypeAdapter[T](wrappedTypeAdapter: TypeAdapter[T])
  extends TypeAdapter[T] {

  override def isStringish: Boolean = true
  val info: RType = wrappedTypeAdapter.info

  def read(parser: Parser): T = {
    // 1. Read String  (JValue --> String)
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
          case r: RType if r.name == "scala.math.BigInt"     => BigInt(wrappedValueString).asInstanceOf[T]
          case r: RType if r.name == "scala.math.BigDecimal" => BigDecimal(wrappedValueString).asInstanceOf[T]
          // $COVERAGE-OFF$Currently all scalars in ScalaJack are supported.  Here just in case...
          case _ =>
            throw new ScalaJackError(
              "Only Scala scalar values are supported as JValue Map keys"
            )
          // $COVERAGE-ON$
        }
      case value: AnyTypeAdapter =>
        value.read(parser).asInstanceOf[T]
      case _ =>
        throw new ScalaJackError(
          "Only scalar values are supported as JValue Map keys"
        )
    }
  }

  def write[WIRE](
      t:      T,
      writer: model.Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    val keyValBuilder =
      JValueBuilder().asInstanceOf[collection.mutable.Builder[Any, WIRE]]
    wrappedTypeAdapter.write(t, writer, keyValBuilder)
    val result = keyValBuilder.result() match {
      case r: JBool    => r.values.toString
      case r: JDecimal => r.values.toString
      case r: JDouble  => r.values.toString
      case r: JInt     => r.values.toString
      case r: JLong    => r.values.toString
      // $COVERAGE-OFF$All scalar/wrapped JValues supported as of this writing.  Here just in case someone invents a new one.
      case r: JString  => r.values.toString  // just in case someone wraps a String!
      case r =>
        throw new ScalaJackError(
          "Json4s type " + r.getClass.getName + " is not supported as a Map key"
        )
      // $COVERAGE-ON$
    }
    writer.writeString(result, out)
  }
}
