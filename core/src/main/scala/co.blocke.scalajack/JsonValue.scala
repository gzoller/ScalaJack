package co.blocke.scalajack

object JsonValue {

  def transform[A, B](source: A)(implicit sourceOps: JsonOps[A], targetOps: JsonOps[B]): B =
    if (sourceOps == targetOps) {
      source.asInstanceOf[B]
    } else {
      source match {
        case JsonArray(x) =>
          val sourceElements = x.asInstanceOf[sourceOps.ArrayElements]

          JsonArray[B] { appendTargetElement =>
            sourceOps.foreachArrayElement(sourceElements, { (_, sourceElement) =>
              val targetElement = transform[A, B](sourceElement)
              appendTargetElement(targetElement)
            })
          }

        case JsonBoolean(booleanValue) =>
          JsonBoolean[B](booleanValue)

        case JsonDecimal(bigDecimal) =>
          JsonDecimal[B](bigDecimal)

        case JsonDouble(doubleValue) =>
          JsonDouble[B](doubleValue)

        case JsonInt(bigInt) =>
          JsonInt[B](bigInt)

        case JsonLong(longValue) =>
          JsonLong[B](longValue)

        case JsonNull() =>
          JsonNull[B]()

        case JsonInvalid() =>
          JsonInvalid[B]()

        case JsonObject(x) =>
          val sourceFields = x.asInstanceOf[sourceOps.ObjectFields]

          JsonObject[B] { appendTargetField =>
            sourceOps.foreachObjectField(sourceFields, { (fieldName, sourceFieldValue) =>
              val targetFieldValue = transform[A, B](sourceFieldValue)
              appendTargetField(fieldName, targetFieldValue)
            })
          }

        case JsonString(string) =>
          JsonString[B](string)
      }
    }

}

object JsonArray {

  @inline final def apply[J](build: (J => Unit) => Unit)(implicit ops: JsonOps[J]): J =
    ops.applyArray(build)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[ops.ArrayElements] =
    ops.unapplyArray(json)

}

object JsonBoolean {

  @inline final def apply[J](value: Boolean)(implicit ops: JsonOps[J]): J =
    ops.applyBoolean(value)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[Boolean] =
    ops.unapplyBoolean(json)

}

object JsonDecimal {

  @inline final def apply[J](value: BigDecimal)(implicit ops: JsonOps[J]): J =
    ops.applyDecimal(value)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[BigDecimal] =
    ops.unapplyDecimal(json)

}

object JsonDouble {

  @inline final def apply[J](value: Double)(implicit ops: JsonOps[J]): J =
    ops.applyDouble(value)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[Double] =
    ops.unapplyDouble(json)

}

object JsonInt {

  @inline final def apply[J](value: BigInt)(implicit ops: JsonOps[J]): J =
    ops.applyInt(value)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[BigInt] =
    ops.unapplyInt(json)

}

object JsonLong {

  @inline final def apply[J](value: Long)(implicit ops: JsonOps[J]): J =
    ops.applyLong(value)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[Long] =
    ops.unapplyLong(json)

}

object JsonNull {

  @inline final def apply[J]()(implicit ops: JsonOps[J]): J =
    ops.applyNull()

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Boolean =
    ops.unapplyNull(json)

}

object JsonObject {

  @inline final def apply[J](build: ((String, J) => Unit) => Unit)(implicit ops: JsonOps[J]): J =
    ops.applyObject(build)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[ops.ObjectFields] =
    ops.unapplyObject(json)

}

object JsonString {

  @inline final def apply[J](value: String)(implicit ops: JsonOps[J]): J =
    ops.applyString(value)

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Option[String] =
    ops.unapplyString(json)

}

object JsonInvalid {

  @inline final def apply[J]()(implicit ops: JsonOps[J]): J =
    ops.applyInvalid()

  @inline final def unapply[J](json: J)(implicit ops: JsonOps[J]): Boolean =
    ops.unapplyInvalid(json)

}
