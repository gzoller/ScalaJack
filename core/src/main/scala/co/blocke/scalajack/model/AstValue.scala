package co.blocke.scalajack
package model

object AstArray {
  @inline final def apply[AST](elements: Seq[AST])(implicit ops: Ops[AST]): AST = ops.applyArray(elements)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[Seq[AST]] = ops.unapplyArray(ast)
}

object AstBoolean {
  @inline final def apply[AST](value: Boolean)(implicit ops: Ops[AST]): AST = ops.applyBoolean(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[Boolean] = ops.unapplyBoolean(ast)
}

// Pseudo-Ast wrapper for custom types utilizing an AstArray tripple: ("AstCustom", typeName, AST)
// The kind of Ast in the 3rd element depends on the typeName and is managed with the TypeAdapter.
object AstCustom {
  @inline final def apply[AST](typeName: String, value: AST)(implicit ops: Ops[AST]): AST =
    ops.applyArray(List(AstString("AstCustom"), AstString(typeName), value))
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[(String, AST)] = ast match {
    case AstArray(a) if (a.size == 3 && a(0) == AstString("AstCustom")) => Some((ops.unapplyString(a(1)).get, a(2)))
    case _ => None
  }
}

object AstDecimal {
  @inline final def apply[AST](value: BigDecimal)(implicit ops: Ops[AST]): AST = ops.applyDecimal(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[BigDecimal] = ops.unapplyDecimal(ast)
}

object AstDouble {
  @inline final def apply[AST](value: Double)(implicit ops: Ops[AST]): AST = ops.applyDouble(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[Double] = ops.unapplyDouble(ast)
}

object AstInt {
  @inline final def apply[AST](value: BigInt)(implicit ops: Ops[AST]): AST = ops.applyInt(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[BigInt] = ops.unapplyInt(ast)
}

object AstLong {
  @inline final def apply[AST](value: Long)(implicit ops: Ops[AST]): AST = ops.applyLong(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[Long] = ops.unapplyLong(ast)
}

object AstNull {
  @inline final def apply[AST]()(implicit ops: Ops[AST]): AST = ops.applyNull()
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Boolean = ops.unapplyNull(ast)
}

object AstObject {
  @inline final def apply[AST](elements: Seq[(String, AST)])(implicit ops: Ops[AST]): AST = ops.applyObject(elements)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[Seq[(String, AST)]] = ops.unapplyObject(ast)
}

// AstMap is a synthetic Ast creation--it doesn't exist in the actual underlying Ast, e.g. Json4s.  Json4s is a great
// Ast, except it presumes JSON's conventions (understandably), notably a JObject's key is always a String.  Scala Maps
// aren't bounded by this convention, so we need a [Ast,AST] representation for Map types.  We "fake" it with this
// synthetic construction, implemented by Ast primitives.
object AstMap {
  @inline final def apply[AST](elements: Seq[(AST, AST)])(implicit ops: Ops[AST]): AST =
    AstCustom("__ScalaMap", AstArray(elements.map { case (k, v) => AstArray(Seq(k, v)) }))
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[Seq[(AST, AST)]] =
    ast match {
      case AstCustom("__ScalaMap", AstArray(elements)) =>
        Some(elements.map {
          _ match {
            case AstArray(kvArr) => (kvArr(0), kvArr(1))
          }
        })
      case _ => None
    }
}

object AstString {
  @inline final def apply[AST](value: String)(implicit ops: Ops[AST]): AST = ops.applyString(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: Ops[AST]): Option[String] = ops.unapplyString(ast)
}