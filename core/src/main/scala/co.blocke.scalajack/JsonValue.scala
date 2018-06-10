package co.blocke.scalajack

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
