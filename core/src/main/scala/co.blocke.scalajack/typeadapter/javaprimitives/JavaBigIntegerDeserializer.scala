package co.blocke.scalajack
package typeadapter
package javaprimitives

import co.blocke.scalajack.typeadapter.javaprimitives.JavaBigIntegerDeserializer.JavaBigIntegerType

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
