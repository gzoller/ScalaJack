package co.blocke.scalajack

object AstValue {

  def transform[ASTA, ASTB, SRCA, SRCB](source: ASTA)(implicit sourceOps: AstOps[ASTA, SRCA], targetOps: AstOps[ASTB, SRCB]): ASTB =
    if (sourceOps == targetOps) {
      source.asInstanceOf[ASTB]
    } else {
      source match {
        case AstArray(x) =>
          val sourceElements = x.asInstanceOf[sourceOps.ArrayElements]

          AstArray[ASTB, SRCB] { appendTargetElement =>
            sourceOps.foreachArrayElement(sourceElements, { (_, sourceElement) =>
              val targetElement = transform[ASTA, ASTB, SRCA, SRCB](sourceElement)
              appendTargetElement(targetElement)
            })
          }

        case AstBoolean(booleanValue) =>
          AstBoolean[ASTB, SRCB](booleanValue)

        case AstDecimal(bigDecimal) =>
          AstDecimal[ASTB, SRCB](bigDecimal)

        case AstDouble(doubleValue) =>
          AstDouble[ASTB, SRCB](doubleValue)

        case AstInt(bigInt) =>
          AstInt[ASTB, SRCB](bigInt)

        case AstLong(longValue) =>
          AstLong[ASTB, SRCB](longValue)

        case AstNull() =>
          AstNull[ASTB, SRCB]()

        case AstObject(x) =>
          val sourceFields = x.asInstanceOf[sourceOps.ObjectFields]

          AstObject[ASTB, SRCB] { appendTargetField =>
            sourceOps.foreachObjectField(sourceFields, { (fieldName, sourceFieldValue) =>
              val targetFieldValue = transform[ASTA, ASTB, SRCA, SRCB](sourceFieldValue)
              appendTargetField(fieldName, targetFieldValue)
            })
          }

        case AstString(string) =>
          AstString[ASTB, SRCB](string)
      }
    }

}

object AstArray {

  @inline final def apply[AST, S](build: (AST => Unit) => Unit)(implicit ops: AstOps[AST, S]): AST =
    ops.applyArray(build)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[ops.ArrayElements] =
    ops.unapplyArray(ast)

}

object AstBoolean {

  @inline final def apply[AST, S](value: Boolean)(implicit ops: AstOps[AST, S]): AST =
    ops.applyBoolean(value)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[Boolean] =
    ops.unapplyBoolean(ast)

}

object AstDecimal {

  @inline final def apply[AST, S](value: BigDecimal)(implicit ops: AstOps[AST, S]): AST =
    ops.applyDecimal(value)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[BigDecimal] =
    ops.unapplyDecimal(ast)

}

object AstDouble {

  @inline final def apply[AST, S](value: Double)(implicit ops: AstOps[AST, S]): AST =
    ops.applyDouble(value)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[Double] =
    ops.unapplyDouble(ast)

}

object AstInt {

  @inline final def apply[AST, S](value: BigInt)(implicit ops: AstOps[AST, S]): AST =
    ops.applyInt(value)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[BigInt] =
    ops.unapplyInt(ast)

}

object AstLong {

  @inline final def apply[AST, S](value: Long)(implicit ops: AstOps[AST, S]): AST =
    ops.applyLong(value)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[Long] =
    ops.unapplyLong(ast)

}

object AstNull {

  @inline final def apply[AST, S]()(implicit ops: AstOps[AST, S]): AST =
    ops.applyNull()

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Boolean =
    ops.unapplyNull(ast)

}

object AstObject {

  @inline final def apply[AST, S](build: ((String, AST) => Unit) => Unit)(implicit ops: AstOps[AST, S]): AST =
    ops.applyObject(build)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[ops.ObjectFields] =
    ops.unapplyObject(ast)

}

object AstString {

  @inline final def apply[AST, S](value: String)(implicit ops: AstOps[AST, S]): AST =
    ops.applyString(value)

  @inline final def unapply[AST, S](ast: AST)(implicit ops: AstOps[AST, S]): Option[String] =
    ops.unapplyString(ast)

}
