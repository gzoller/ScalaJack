package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigIntegerDeserializer.JavaBigIntegerType
import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigIntegerSerializer.ScalaBigIntType

import scala.reflect.runtime.universe.{ Type, typeOf }

object JavaBigIntegerDeserializer {

  val JavaBigIntegerType: Type = typeOf[java.math.BigInteger]

}

class JavaBigIntegerDeserializer(scalaBigIntDeserializer: Deserializer[scala.math.BigInt]) extends Deserializer[java.math.BigInteger] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[java.math.BigInteger] =
    scalaBigIntDeserializer.deserialize(path, json) map {
      case TypeTagged(null)        => TypeTagged(null, JavaBigIntegerType)
      case TypeTagged(scalaBigInt) => TypeTagged(scalaBigInt.bigInteger, JavaBigIntegerType)
    }

}

object JavaBigIntegerSerializer {

  val ScalaBigIntType: Type = typeOf[scala.math.BigInt]

}

class JavaBigIntegerSerializer(scalaBigIntSerializer: Serializer[scala.math.BigInt]) extends Serializer[java.math.BigInteger] {

  override def serialize[J](tagged: TypeTagged[java.math.BigInteger])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null)           => scalaBigIntSerializer.serialize(TypeTagged(null, ScalaBigIntType))
      case TypeTagged(javaBigInteger) => scalaBigIntSerializer.serialize(TypeTagged(scala.math.BigInt(javaBigInteger), ScalaBigIntType))
    }

}

object JavaBigIntegerTypeAdapter extends TypeAdapterFactory.=:=[java.math.BigInteger] {

  override def create(next: TypeAdapterFactory)(implicit context: Context): TypeAdapter[java.math.BigInteger] = {
    val scalaBigIntTypeAdapter = context.typeAdapterOf[scala.math.BigInt]
    new JavaBigIntegerTypeAdapter(
      deserializer = new JavaBigIntegerDeserializer(scalaBigIntTypeAdapter.deserializer),
      serializer   = new JavaBigIntegerSerializer(scalaBigIntTypeAdapter.serializer))
  }

}

class JavaBigIntegerTypeAdapter(override val deserializer: Deserializer[java.math.BigInteger], override val serializer: Serializer[java.math.BigInteger]) extends TypeAdapter[java.math.BigInteger] {

  override def read(reader: Reader): java.math.BigInteger =
    reader.peek match {
      case TokenType.Number =>
        reader.read(expected = TokenType.Number)
        new java.math.BigInteger(reader.tokenText)

      case TokenType.Null =>
        reader.readNull()

      case actual =>
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading BigInteger value.  (Is your value wrapped in quotes?)\n" + reader.showError())
    }

  override def write(value: java.math.BigInteger, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeRawValue(value.toString)
    }

}