package co.blocke.scalajackx

object ASTArray {
  @inline final def apply[AST](elements: Seq[AST])(implicit ops: OpsBase[AST]): AST = ops.applyArray(elements)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[Seq[AST]] = ops.unapplyArray(ir)
}

object ASTBoolean {
  @inline final def apply[AST](value: Boolean)(implicit ops: OpsBase[AST]): AST = ops.applyBoolean(value)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[Boolean] = ops.unapplyBoolean(ir)
}

/*
// Pseudo-AST wrapper for custom types utilizing an ASTArray tripple: ("ASTCustom", typeName, AST)
// The kind of AST in the 3rd element depends on the typeName and is managed with the TypeAdapter.
object ASTCustom {
  @inline final def apply[AST](typeName: String, value: AST)(implicit ops: OpsBase[AST]): AST =
    ops.applyArray(List(ASTString("ASTCustom"), ASTString(typeName), value))
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[(String, AST)] = ir match {
    case ASTArray(a) if (a.size == 3 && a(0) == ASTString("ASTCustom")) => Some((ops.unapplyString(a(1)).get, a(2)))
    case _ => None
  }
}

object ASTDecimal {
  @inline final def apply[AST](value: BigDecimal)(implicit ops: OpsBase[AST]): AST = ops.applyDecimal(value)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[BigDecimal] = ops.unapplyDecimal(ir)
}

object ASTDouble {
  @inline final def apply[AST](value: Double)(implicit ops: OpsBase[AST]): AST = ops.applyDouble(value)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[Double] = ops.unapplyDouble(ir)
}

object ASTInt {
  @inline final def apply[AST](value: BigInt)(implicit ops: OpsBase[AST]): AST = ops.applyInt(value)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[BigInt] = ops.unapplyInt(ir)
}

object ASTLong {
  @inline final def apply[AST](value: Long)(implicit ops: OpsBase[AST]): AST = ops.applyLong(value)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[Long] = ops.unapplyLong(ir)
}

object ASTNull {
  @inline final def apply[AST]()(implicit ops: OpsBase[AST]): AST = ops.applyNull()
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Boolean = ops.unapplyNull(ir)
}

object ASTObject {
  @inline final def apply[AST](elements: Seq[(String, AST)])(implicit ops: OpsBase[AST]): AST = ops.applyObject(elements)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[Seq[(String, AST)]] = ops.unapplyObject(ir)
}

// ASTMap is a synthetic AST creation--it doesn't exist in the actual underlying AST, e.g. Json4s.  Json4s is a great
// AST, except it presumes JSON's conventions (understandably), notably a JObject's key is always a String.  Scala Maps
// aren't bounded by this convention, so we need a [AST,AST] representation for Map types.  We "fake" it with this
// synthetic construction, implemented by AST primitives.
object ASTMap {
  @inline final def apply[AST](elements: Seq[(AST, AST)])(implicit ops: OpsBase[AST]): AST =
    ASTCustom("__ScalaMap", ASTArray(elements.map { case (k, v) => ASTArray(Seq(k, v)) }))
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[Seq[(AST, AST)]] =
    ir match {
      case ASTCustom("__ScalaMap", ASTArray(elements)) =>
        Some(elements.map {
          _ match {
            case ASTArray(kvArr) => (kvArr(0), kvArr(1))
          }
        })
      case _ => None
    }
}

object ASTString {
  @inline final def apply[AST](value: String)(implicit ops: OpsBase[AST]): AST = ops.applyString(value)
  @inline final def unapply[AST](ir: AST)(implicit ops: OpsBase[AST]): Option[String] = ops.unapplyString(ir)
}
*/