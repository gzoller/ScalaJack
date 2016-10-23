package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, TypeTag }

/**
 * __DO NOT__ EXTEND THIS CLASS FOR TypeAdapter OVERRIDES FOR PRIMITIVE TYPES (e.g. in VisitorContext)!
 *
 * See note in BasicTypeAdapter for details.
 */
object SimpleTypeAdapter {

  abstract class ForTypeSymbolOf[T](implicit valueTypeTag: TypeTag[T]) extends TypeAdapterFactory with TypeAdapter[T] {

    override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): Option[TypeAdapter[_]] =
      if (tpe.typeSymbol == valueTypeTag.tpe.typeSymbol) {
        Some(this)
      } else {
        next.typeAdapter(tpe, context)
      }

  }

}

abstract class SimpleTypeAdapter[T](implicit valueTypeTag: TypeTag[T]) extends TypeAdapterFactory with TypeAdapter[T] {

  val valueType = valueTypeTag.tpe

  override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): Option[TypeAdapter[_]] =
    if (tpe =:= valueType) {
      Some(this)
    } else {
      next.typeAdapter(tpe, context)
    }

}
