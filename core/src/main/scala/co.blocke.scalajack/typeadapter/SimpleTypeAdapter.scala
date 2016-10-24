package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, TypeTag }

/**
 * __DO NOT__ EXTEND THIS CLASS FOR TypeAdapter OVERRIDES FOR PRIMITIVE TYPES (e.g. in VisitorContext)!
 *
 * See note in BasicTypeAdapter for details.
 */
object SimpleTypeAdapter {

  abstract class ForTypeSymbolOf[V](implicit valueTypeTag: TypeTag[V]) extends TypeAdapterFactory with TypeAdapter[V] {

    override def typeAdapterOf[T](context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] =
      if (tt.tpe.typeSymbol == valueTypeTag.tpe.typeSymbol) {
        this.asInstanceOf[TypeAdapter[T]]
      } else {
        next.typeAdapterOf[T](context)
      }

  }

}

abstract class SimpleTypeAdapter[T](implicit valueTypeTag: TypeTag[T]) extends TypeAdapterFactory with TypeAdapter[T] {

  val valueType = valueTypeTag.tpe

  override def typeAdapterOf[U](context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[U]): TypeAdapter[U] =
    if (tt.tpe =:= valueType) {
      this.asInstanceOf[TypeAdapter[U]]
    } else {
      next.typeAdapterOf[U](context)
    }

}
